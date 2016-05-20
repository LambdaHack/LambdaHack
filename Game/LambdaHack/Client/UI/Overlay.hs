{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen overlays and frames.
module Game.LambdaHack.Client.UI.Overlay
  ( AttrLine, toAttrLine, (<+:>)
  , tmoreMsg, tendMsg, tyesnoMsg, moreMsg, endMsg, yesnoMsg
  , Overlay(overlay), toOverlayRaw, toOverlay
  , updateOverlayLine, itemDesc
  , SingleFrame(..), Frames, overlayFrame
  , Slideshow(slideshow), toSlideshow, menuToSlideshow, textsToSlideshow
  , KYX, OKX, keyOfEKM
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

type AttrLine = [Color.AttrChar]

toAttrLine :: Text -> AttrLine
toAttrLine = map (Color.AttrChar Color.defAttr) . T.unpack

-- TODO: make a class, for monoids with neutral elements
infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ toAttrLine " " ++ l2

-- tmp, until help in colour
tmoreMsg :: Text
tmoreMsg = "--more--  "

-- tmp, until help in colour
tendMsg :: Text
tendMsg = "--end--  "

tyesnoMsg :: Text
tyesnoMsg = "[y, n, ESC]"

-- | The \"press something to see more\" mark.
moreMsg :: AttrLine
moreMsg = toAttrLine "--more--  "

-- | The \"end of screenfuls of text\" mark.
endMsg :: AttrLine
endMsg = toAttrLine"--end--  "

-- | The confirmation request message.
yesnoMsg :: AttrLine
yesnoMsg = toAttrLine "[y, n, ESC]"

-- | A series of screen lines that either fit the width of the screen
-- or are intended for truncation when displayed. The length of overlay
-- may exceed the length of the screen, unlike in @SingleFrame@.
newtype Overlay = Overlay {overlay :: [AttrLine]}
  deriving (Show, Eq, Monoid)

-- TODO: get rid of
toOverlayRaw :: [AttrLine] -> Overlay
toOverlayRaw = Overlay

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

itemDesc :: CStore -> Time -> ItemFull -> AttrLine
itemDesc c localTime itemFull =
  let (_, name, stats) = partItemN 10 100 c localTime itemFull
      nstats = makePhrase [name, stats]
      desc = case itemDisco itemFull of
        Nothing -> "This item is as unremarkable as can be."
        Just ItemDisco{itemKind} -> IK.idesc itemKind
      weight = jweight (itemBase itemFull)
      (scaledWeight, unitWeight)
        | weight > 1000 =
          (tshow $ fromIntegral weight / (1000 :: Double), "kg")
        | weight > 0 = (tshow weight, "g")
        | otherwise = ("", "")
      ln = abs $ fromEnum $ jlid (itemBase itemFull)
      colorSymbol = uncurry (flip Color.AttrChar) (viewItem $ itemBase itemFull)
      blurb =
        " "
        <> nstats
        <> ":"
        <+> desc
        <+> makeSentence ["Weighs", MU.Text scaledWeight <> unitWeight]
        <+> makeSentence ["First found on level", MU.Text $ tshow ln]
  in colorSymbol : toAttrLine blurb

-- | An overlay that fits on the screen (or is meant to be truncated on display)
-- and is padded to fill the whole screen
-- and is displayed as a single game screen frame.
newtype SingleFrame = SingleFrame { sfLevel :: Overlay }
  deriving (Eq, Show)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

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

-- Neither list may be empty.
type OKX = ([AttrLine], [KYX])

-- May be empty, but nothing inside may be empty.
newtype Slideshow = Slideshow {slideshow :: [OKX]}
  deriving (Show, Eq, Monoid)

toSlideshow :: [OKX] -> Slideshow
toSlideshow okxs = Slideshow $ addFooters okxs
 where
  addFooters [] = assert `failure` okxs
  addFooters [(als, kxs)] =
    [( als ++ [endMsg]
     , kxs ++ [(Left K.escKM, (length als, 0, 8))] )]
  addFooters ((als, kxs) : rest) =
    ( als ++ [moreMsg]
    , kxs ++ [(Left K.pgdnKM, (length als, 0, 8))] )
    : addFooters rest

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (null als || null kxs)) $ Slideshow [(als, kxs)]

textsToSlideshow :: [[Text]] -> Slideshow
textsToSlideshow = toSlideshow . map (\t -> (map toAttrLine t, []))

keyOfEKM :: Int -> Either K.KM SlotChar -> Maybe K.KM
keyOfEKM _ (Left km) = Just km
keyOfEKM numPrefix (Right SlotChar{..}) | slotPrefix == numPrefix =
  Just $ K.KM K.NoModifier $ K.Char slotChar
keyOfEKM _ _ = Nothing
