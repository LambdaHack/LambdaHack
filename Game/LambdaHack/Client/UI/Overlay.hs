-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * AttrLine
    AttrLine, toAttrLine, (<+:>), splitAttrLine, itemDesc
    -- * Overlay
  , Overlay, glueOverlay, updateOverlayLine
    -- * Misc
  , ColorMode(..), tmoreMsg, tendMsg
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Char
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- * AttrLine

type AttrLine = [Color.AttrChar]

toAttrLine :: Text -> AttrLine
toAttrLine = map (Color.AttrChar Color.defAttr) . T.unpack

infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ toAttrLine " " ++ l2

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitAttrLine :: X -> AttrLine -> [AttrLine]
splitAttrLine w l =
  concatMap (splitAttrPhrase w . dropWhile (isSpace . Color.acChar))
  $ linesAttr l

linesAttr :: AttrLine -> [AttrLine]
linesAttr l | null l = []
            | otherwise = h : if null t then [] else linesAttr (tail t)
 where (h, t) = span ((/= '\n') . Color.acChar) l

splitAttrPhrase :: X -> AttrLine -> [AttrLine]
splitAttrPhrase w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt w xs
          (ppre, ppost) = break ((== ' ') . Color.acChar) $ reverse pre
          testPost = dropWhileEnd (isSpace . Color.acChar) ppost
      in if null testPost
         then pre : splitAttrPhrase w post
         else reverse ppost : splitAttrPhrase w (reverse ppre ++ post)

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

-- * Overlay

-- | A series of screen lines that either fit the width of the screen
-- or are intended for truncation when displayed. The length of overlay
-- may exceed the length of the screen, unlike in @SingleFrame@.
type Overlay = [AttrLine]

glueOverlay :: Overlay -> Overlay -> Overlay
glueOverlay ov1 ov2 = reverse $ glue (reverse ov1) ov2
 where glue [] l = l
       glue m [] = m
       glue (mh : mt) (lh : lt) = reverse lt ++ (mh <+:> lh) : mt

-- @f@ should not enlarge the line beyond screen width.
updateOverlayLine :: Int -> (AttrLine -> AttrLine) -> Overlay -> Overlay
updateOverlayLine n f ov =
  let upd k (l : ls) = if k == 0
                       then f l : ls
                       else l : upd (k - 1) ls
      upd _ [] = []
  in upd n ov

-- * Misc

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

tmoreMsg :: Text
tmoreMsg = "--more--  "

tendMsg :: Text
tendMsg = "--end--  "
