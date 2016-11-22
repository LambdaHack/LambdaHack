{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames.
module Game.LambdaHack.Client.UI.Frame
  ( SingleFrame(..), Frames
  , blankSingleFrame, overlayFrame, overlayFrameWithLines
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Word (Word32)

import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | An overlay that fits on the screen (or is meant to be truncated on display)
-- and is padded to fill the whole screen
-- and is displayed as a single game screen frame.
newtype SingleFrame = SingleFrame
  {singleFrame :: PointArray.GArray Word32 AttrCharW32}
  deriving (Eq, Show)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe FrameForall]

blankSingleFrame :: SingleFrame
blankSingleFrame =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 4
  in SingleFrame $ PointArray.replicateA lxsize lysize spaceAttrW32

-- | Truncate the overlay: for each line, if it's too long, it's truncated
-- and if there are too many lines, excess is dropped and warning is appended.
truncateLines :: Bool -> [AttrLine] -> [AttrLine]
truncateLines onBlank l =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      canvasLength = if onBlank then lysize + 3 else lysize + 1
      topLayer = if length l <= canvasLength
                 then l ++ if length l < canvasLength && length l > 3
                           then [[]]
                           else []
                 else take (canvasLength - 1) l
                      ++ [stringToAL "--a portion of the text trimmed--"]
      f lenPrev lenNext layerLine =
        truncateAttrLine lxsize layerLine (max lenPrev lenNext)
      lens = map (\al -> min (lxsize - 1) (length al)) topLayer
  in zipWith3 f (0 : lens) (drop 1 lens ++ [0]) topLayer

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines.
truncateAttrLine :: X -> AttrLine -> X -> AttrLine
truncateAttrLine w xs lenMax =
  case compare w (length xs) of
    LT -> let discarded = drop w xs
          in if all (== spaceAttrW32) discarded
             then take w xs
             else take (w - 1) xs ++ [attrChar2ToW32 BrBlack '$']
    EQ -> xs
    GT -> let xsSpace = if null xs || last xs == spaceAttrW32
                        then xs
                        else xs ++ [spaceAttrW32]
              whiteN = max (40 - length xsSpace) (1 + lenMax - length xsSpace)
          in xsSpace ++ replicate whiteN spaceAttrW32

-- | Overlays either the game map only or the whole empty screen frame.
-- We assume the lines of the overlay are not too long nor too many.
overlayFrame :: Overlay -> FrameForall -> FrameForall
overlayFrame ov ff = FrameForall $ \v -> do
  unFrameForall ff v
  mapM_ (\(offset, l) -> unFrameForall (writeLine offset l) v) ov

overlayFrameWithLines :: Bool -> [AttrLine] -> FrameForall -> FrameForall
overlayFrameWithLines onBlank l msf =
  let lxsize = fst normalLevelBound + 1  -- TODO
      ov = map (\(y, al) -> (y * lxsize, al))
           $ zip [0..] $ truncateLines onBlank l
  in overlayFrame ov msf
