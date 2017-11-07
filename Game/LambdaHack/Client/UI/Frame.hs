{-# LANGUAGE RankNTypes #-}
-- | Screen frames.
module Game.LambdaHack.Client.UI.Frame
  ( FrameST, FrameForall(..), writeLine
  , SingleFrame(..), Frames
  , blankSingleFrame, overlayFrame, overlayFrameWithLines
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Monad.ST.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word

import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

type FrameST s = G.Mutable U.Vector s Word32 -> ST s ()

newtype FrameForall = FrameForall {unFrameForall :: forall s. FrameST s}

writeLine :: Int -> AttrLine -> FrameForall
{-# INLINE writeLine #-}
writeLine offset l = FrameForall $ \v -> do
  let writeAt _ [] = return ()
      writeAt off (ac32 : rest) = do
        VM.write v off (Color.attrCharW32 ac32)
        writeAt (off + 1) rest
  writeAt offset l

-- | An overlay that fits on the screen (or is meant to be truncated on display)
-- and is padded to fill the whole screen
-- and is displayed as a single game screen frame.
--
-- Note that we don't provide a list of color-highlighed positions separately,
-- because overlays need to obscure not only map, but the highlights as well.
newtype SingleFrame = SingleFrame
  {singleFrame :: PointArray.GArray Word32 Color.AttrCharW32}
  deriving (Eq, Show)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe FrameForall]

blankSingleFrame :: SingleFrame
blankSingleFrame =
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
  in SingleFrame $ PointArray.replicateA lxsize lysize Color.spaceAttrW32

-- | Truncate the overlay: for each line, if it's too long, it's truncated
-- and if there are too many lines, excess is dropped and warning is appended.
truncateLines :: Bool -> Overlay -> Overlay
truncateLines onBlank l =
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 1
      canvasLength = if onBlank then lysize + 3 else lysize + 1
      topLayer = if length l <= canvasLength
                 then l ++ [[] | length l < canvasLength && length l > 3]
                 else take (canvasLength - 1) l
                      ++ [stringToAL "--a portion of the text trimmed--"]
      f lenPrev lenNext layerLine =
        truncateAttrLine lxsize layerLine (max lenPrev lenNext)
      lens = map (min (lxsize - 1) . length) topLayer
  in zipWith3 f (0 : lens) (drop 1 lens ++ [0]) topLayer

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines.
truncateAttrLine :: X -> AttrLine -> X -> AttrLine
truncateAttrLine w xs lenMax =
  case compare w (length xs) of
    LT -> let discarded = drop w xs
          in if all (== Color.spaceAttrW32) discarded
             then take w xs
             else take (w - 1) xs ++ [Color.attrChar2ToW32 Color.BrBlack '$']
    EQ -> xs
    GT -> let xsSpace = if null xs || last xs == Color.spaceAttrW32
                        then xs
                        else xs ++ [Color.spaceAttrW32]
              whiteN = max (40 - length xsSpace) (1 + lenMax - length xsSpace)
          in xsSpace ++ replicate whiteN Color.spaceAttrW32

-- | Overlays either the game map only or the whole empty screen frame.
-- We assume the lines of the overlay are not too long nor too many.
overlayFrame :: IntOverlay -> FrameForall -> FrameForall
overlayFrame ov ff = FrameForall $ \v -> do
  unFrameForall ff v
  mapM_ (\(offset, l) -> unFrameForall (writeLine offset l) v) ov

overlayFrameWithLines :: Bool -> Overlay -> FrameForall -> FrameForall
overlayFrameWithLines onBlank l msf =
  let lxsize = fst normalLevelBound + 1
      ov = map (\(y, al) -> (y * lxsize, al))
           $ zip [0..] $ truncateLines onBlank l
  in overlayFrame ov msf
