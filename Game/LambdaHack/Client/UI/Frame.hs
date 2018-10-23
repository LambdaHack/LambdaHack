{-# LANGUAGE RankNTypes, TypeFamilies #-}
-- | Screen frames.
module Game.LambdaHack.Client.UI.Frame
  ( FrameST, FrameForall(..), FrameBase(..), Frame, Frames, writeLine
  , SingleFrame(..)
  , blankSingleFrame, overlayFrame, overlayFrameWithLines
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , truncateAttrLine
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Monad.ST.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

type FrameST s = G.Mutable U.Vector s Word32 -> ST s ()

-- | Efficiently composable representation of an operation
-- on a frame, that is, on a mutable vector. When the composite operation
-- is eventually performed, the vector is frozen to become a 'SingleFrame'.
newtype FrameForall = FrameForall {unFrameForall :: forall s. FrameST s}

-- | Action that results in a base frame, to be modified further.
newtype FrameBase = FrameBase
  {unFrameBase :: forall s. ST s (G.Mutable U.Vector s Word32)}

-- | A frame, that is, a base frame and all its modifications.
type Frame = (FrameBase, FrameForall)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe Frame]

-- | Representation of an operation of overwriting a frame with a single line
-- at the given row.
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
  {singleFrame :: PointArray.Array Color.AttrCharW32}
  deriving (Eq, Show)

blankSingleFrame :: ScreenContent -> SingleFrame
blankSingleFrame ScreenContent{rwidth, rheight} =
  SingleFrame $ PointArray.replicateA rwidth rheight Color.spaceAttrW32

-- | Truncate the overlay: for each line, if it's too long, it's truncated
-- and if there are too many lines, excess is dropped and warning is appended.
truncateLines :: ScreenContent -> Bool -> Overlay -> Overlay
truncateLines ScreenContent{rwidth, rheight} onBlank l =
  let canvasLength = if onBlank then rheight else rheight - 2
      topLayer = if length l <= canvasLength
                 then l ++ [[] | length l < canvasLength && length l > 3]
                 else take (canvasLength - 1) l
                      ++ [stringToAL "--a portion of the text trimmed--"]
      f lenPrev lenNext layerLine =
        truncateAttrLine rwidth layerLine (max lenPrev lenNext)
      lens = map (min (rwidth - 1) . length) topLayer
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
    GT -> let xsSpace =
                if | null xs -> xs
                   | last xs == Color.spaceAttrW32 -> xs ++ [Color.spaceAttrW32]
                   | otherwise -> xs ++ [Color.spaceAttrW32, Color.spaceAttrW32]
              whiteN = max (40 - length xsSpace) (1 + lenMax - length xsSpace)
          in xsSpace ++ replicate whiteN Color.spaceAttrW32

-- | Overlays either the game map only or the whole empty screen frame.
-- We assume the lines of the overlay are not too long nor too many.
overlayFrame :: IntOverlay -> Frame -> Frame
overlayFrame ov (m, ff) = (m, FrameForall $ \v -> do
  unFrameForall ff v
  mapM_ (\(offset, l) -> unFrameForall (writeLine offset l) v) ov)

overlayFrameWithLines :: ScreenContent -> Bool -> Overlay -> Frame
                      -> Frame
overlayFrameWithLines coscreen@ScreenContent{rwidth} onBlank l fr =
  let ov = map (\(y, al) -> (y * rwidth, al))
           $ zip [0..] $ truncateLines coscreen onBlank l
  in overlayFrame ov fr
