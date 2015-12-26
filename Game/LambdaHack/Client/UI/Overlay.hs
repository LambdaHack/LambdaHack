{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen overlays and frames.
module Game.LambdaHack.Client.UI.Overlay
  ( AttrLine, toAttrLine, moreMsgAttr
  , Overlay(overlay), toOverlayRaw, truncateToOverlay, toOverlay
  , updateOverlayLine, splitReport, renderHistory
  , SingleFrame(..), Frames, overlayFrame
  , Slideshow(slideshow), splitOverlay, toSlideshow
  , KYX, OKX, keyOfEKM
  ) where

import Prelude ()
import Prelude.Compat

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point

type AttrLine = [AttrChar]

toAttrLine :: Text -> AttrLine
toAttrLine = map (AttrChar defAttr) . T.unpack

-- | The \"press something to see more\" mark.
moreMsgAttr :: AttrLine
moreMsgAttr = toAttrLine moreMsg

-- | A series of screen lines that either fit the width of the screen
-- or are intended for truncation when displayed. The length of overlay
-- may exceed the length of the screen, unlike in @SingleFrame@.
newtype Overlay = Overlay {overlay :: [AttrLine]}
  deriving (Show, Eq, Monoid)

-- TODO: get rid of
toOverlayRaw :: [AttrLine] -> Overlay
toOverlayRaw = Overlay

truncateToOverlay :: Text -> Overlay
truncateToOverlay msg = toOverlay [msg]

toOverlay :: [Text] -> Overlay
toOverlay = Overlay . map toAttrLine

-- @f@ should not enlarge the line beyond screen width.
updateOverlayLine :: Int -> (AttrLine -> AttrLine) -> Overlay -> Overlay
updateOverlayLine n f Overlay{overlay} =
  let upd k (l : ls) = if k == 0
                       then f l : ls
                       else l : upd (k - 1) ls
      upd _ [] = []
  in Overlay $ upd n overlay

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: X -> Report -> Overlay
splitReport w r = toOverlay $ splitText w $ renderReport r

-- | An overlay that fits on the screen (or is meant to be truncated on display)
-- and is padded to fill the whole screen
-- and is displayed as a single game screen frame.
newtype SingleFrame = SingleFrame { sfLevel :: Overlay }
  deriving (Eq, Show)

-- | Split an overlay into a slideshow in which each overlay,
-- prefixed by @msg@ and postfixed by @moreMsg@ except for the last one,
-- fits on the screen wrt height (but lines may be too wide).
splitOverlay :: Y -> Overlay -> Overlay -> Slideshow
splitOverlay yspace (Overlay msg) (Overlay ls0) =
  if length msg > yspace `div` 2
  then  -- too long msg, no sense repeating it on each page
    splitOverlay yspace mempty (Overlay msg <> Overlay ls0)
  else let splitO ls =
             let (pre, post) = splitAt (yspace - 1) $ msg ++ ls
             in if null (drop 1 post)  -- (don't call @length@ on @ls0@)
                then [Overlay $ msg ++ ls]
                       -- all fits on screen
                else let rest = splitO post
                     in Overlay (pre ++ [moreMsgAttr]) : rest
       in Slideshow (splitO ls0)

-- | A few overlays, displayed one by one upon keypress.
-- When displayed, they are trimmed, not wrapped
-- and any lines below the lower screen edge are not visible.
-- The first pair element determines if the overlay is displayed
-- over a blank screen, including the bottom lines.
newtype Slideshow = Slideshow {slideshow :: [Overlay]}
  deriving (Show, Eq, Monoid)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

-- | Declare the list of raw overlays to be fit for display on the screen.
-- In particular, current @Report@ is eiter empty or unimportant
-- or contained in the overlays and if any vertical or horizontal
-- trimming of the overlays happens, this is intended.
toSlideshow :: [[Text]] -> Slideshow
toSlideshow l = Slideshow $ map toOverlay l

-- | Overlays with a given overlay either the top line and level map area
-- of a screen frame or the whole area of a completely empty screen frame.
overlayFrame :: Overlay -> Maybe SingleFrame -> SingleFrame
overlayFrame sfTop msf =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      emptyLine = toAttrLine $ T.replicate lxsize " "
      canvasLength = if isNothing msf then lysize + 3 else lysize + 1
      canvas = maybe (replicate canvasLength emptyLine)
                     (\sf -> overlay (sfLevel sf))
                     msf
      topTrunc = overlay sfTop
      topLayer = if length topTrunc <= canvasLength
                 then topTrunc
                 else take (canvasLength - 1) topTrunc
                      ++ overlay (toOverlay ["--a portion of the text trimmed--"])
      f layerLine canvasLine =
        let truncated = truncateAttrLine lxsize layerLine
        in truncated ++ drop (length truncated) canvasLine
      picture = zipWith f topLayer canvas
      newLevel = picture ++ drop (length picture) canvas
  in SingleFrame $ toOverlayRaw newLevel

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines.
truncateAttrLine :: X -> AttrLine -> AttrLine
truncateAttrLine w xs =
  case compare w (length xs) of
    LT -> let discarded = drop w xs
          in if all ((== ' ') . acChar) discarded
             then take w xs
             else take (w - 1) xs ++ [AttrChar (Attr BrBlack defBG) '$']
    EQ -> xs
    GT -> if null xs || acChar (last xs) == ' '
          then xs
          else xs ++ [AttrChar Color.defAttr ' ']

type KYX = (Either K.KM SlotChar, (Y, X, X))

type OKX = (Overlay, [KYX])

keyOfEKM :: Int -> Either K.KM SlotChar -> Maybe K.KM
keyOfEKM _ (Left km) = Just km
keyOfEKM numPrefix (Right SlotChar{..}) | slotPrefix == numPrefix =
  Just $ K.toKM K.NoModifier $ K.Char slotChar
keyOfEKM _ _ = Nothing
