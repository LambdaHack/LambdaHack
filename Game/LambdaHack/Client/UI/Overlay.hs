{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen overlays and frames.
module Game.LambdaHack.Client.UI.Overlay
  ( AttrLine, toAttrLine, (<+:>)
  , tmoreMsg, tendMsg, tyesnoMsg, moreMsg, endMsg, yesnoMsg
  , Overlay(overlay), toOverlayRaw, toOverlay
  , updateOverlayLine, itemDesc
  , SingleFrame(..), Frames, overlayFrame
  , Slideshow(slideshow), splitOverlay, toSlideshow
  , KYX, OKX, keyOfEKM, splitOverlayOKX
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
                     in Overlay (pre ++ [moreMsg]) : rest
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
  Just $ K.KM K.NoModifier $ K.Char slotChar
keyOfEKM _ _ = Nothing

-- TODO: assert that ov0 nonempty and perhaps that kxs0 not too short
-- (or should we just keep the rest of the overlay unclickable?)
splitOverlayOKX :: Y -> Overlay -> OKX -> [OKX]
splitOverlayOKX yspace omsg (ov0, kxs0) =
  let msg = overlay omsg
      msg0 = if yspace - length msg - 1 <= 0  -- all space taken by @msg@
             then take 1 msg
             else msg
      ls0 = overlay ov0
      len = length msg0
      renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
      zipRenumber y = zipWith renumber [y..]
      splitO ls kxs =
        let (pre, post) = splitAt (yspace - 1) $ msg0 ++ ls
        in if null post
           then  -- all fits on screen
             let bottomMsgAttr = toAttrLine $
                   T.replicate (length $ last pre) " "
             in [( toOverlayRaw $ pre ++ [bottomMsgAttr]
                 , zipRenumber len kxs )]
           else let (preX, postX) = splitAt (yspace - len - 1) kxs
                    rest = splitO post postX
                in ( toOverlayRaw (pre ++ [moreMsg])
                   , zipRenumber len preX )
                   : rest
  in splitO ls0 kxs0
