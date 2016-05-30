{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames.
module Game.LambdaHack.Client.UI.Frame
  ( SingleFrame(..), Frames, overlayFrame
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point

-- | An overlay that fits on the screen (or is meant to be truncated on display)
-- and is padded to fill the whole screen
-- and is displayed as a single game screen frame.
newtype SingleFrame = SingleFrame {singleFrame :: Overlay}
  deriving (Eq, Show)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

-- | Overlays with a given overlay either the complete game map and UI screen
-- or the empty screen frame.
-- For each line of the overlay, if it's too long, it's truncated.
overlayFrame :: Overlay -> Maybe SingleFrame -> SingleFrame
overlayFrame topTrunc msf =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      emptyLine = toAttrLine $ T.replicate lxsize " "
      canvasLength = if isNothing msf then lysize + 3 else lysize + 1
      canvas = maybe (replicate canvasLength emptyLine)
                     (\sf -> singleFrame sf)
                     msf
      topLayer = if length topTrunc <= canvasLength
                 then topTrunc ++ if length topTrunc < canvasLength
                                  then [emptyLine]
                                  else []
                 else take (canvasLength - 1) topTrunc
                      ++ [toAttrLine "--a portion of the text trimmed--"]
      f lenPrev lenNext layerLine canvasLine =
        let truncated = truncateAttrLine lxsize layerLine (max lenPrev lenNext)
        in truncated ++ drop (length truncated) canvasLine
      lens = map (\al -> min (lxsize - 1) (length al)) topLayer
      picture = zipWith4 f (0 : lens) (drop 1 lens ++ [0]) topLayer canvas
      newLevel = picture ++ drop (length picture) canvas
  in SingleFrame newLevel

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines.
truncateAttrLine :: X -> AttrLine -> X -> AttrLine
truncateAttrLine w xs lenMax =
  case compare w (length xs) of
    LT -> let discarded = drop w xs
          in if all ((== ' ') . acChar) discarded
             then take w xs
             else take (w - 1) xs ++ [AttrChar (Attr BrBlack defBG) '$']
    EQ -> xs
    GT -> let xsSpace = if null xs || acChar (last xs) == ' '
                        then xs
                        else xs ++ toAttrLine " "
          in xsSpace
             ++ replicate (1 + lenMax - length xsSpace) (AttrChar defAttr ' ')
