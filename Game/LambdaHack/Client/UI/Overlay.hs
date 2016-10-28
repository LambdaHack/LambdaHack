{-# LANGUAGE RankNTypes #-}
-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * AttrLine
    AttrLine, emptyAttrLine, textToAL, stringToAL
  , (<+:>), splitAttrLine, itemDesc, glueLines, updateLines
    -- * Overlay
  , Overlay
    -- * Misc
  , ColorMode(..)
  , FrameST, FrameForall(..), writeLine
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Monad.ST.Strict
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- * AttrLine

type AttrLine = [Color.AttrCharW32]

emptyAttrLine :: Int -> AttrLine
emptyAttrLine xsize = replicate xsize Color.spaceAttrW32

textToAL :: Text -> AttrLine
textToAL !t =
  let f c l = let !ac = Color.attrCharToW32 $ Color.AttrChar Color.defAttr c
              in ac : l
  in T.foldr f [] t

stringToAL :: String -> AttrLine
stringToAL s = map (Color.attrCharToW32 . Color.AttrChar Color.defAttr) s

infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ [Color.spaceAttrW32] ++ l2

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitAttrLine :: X -> AttrLine -> [AttrLine]
splitAttrLine w l =
  concatMap (splitAttrPhrase w . dropWhile (== Color.spaceAttrW32))
  $ linesAttr l

linesAttr :: AttrLine -> [AttrLine]
linesAttr l | null l = []
            | otherwise = h : if null t then [] else linesAttr (tail t)
 where (h, t) = span (/= Color.retAttrW32) l

splitAttrPhrase :: X -> AttrLine -> [AttrLine]
splitAttrPhrase w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt w xs
          (ppre, ppost) = break (== Color.spaceAttrW32) $ reverse pre
          testPost = dropWhileEnd (== Color.spaceAttrW32) ppost
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
        | otherwise = (tshow weight, "g")
      ln = abs $ fromEnum $ jlid (itemBase itemFull)
      colorSymbol = viewItem $ itemBase itemFull
      blurb =
        " "
        <> nstats
        <> ":"
        <+> desc
        <+> if weight > 0
            then makeSentence ["Weighs", MU.Text scaledWeight <> unitWeight]
            else ""
        <+> makeSentence ["First found on level", MU.Text $ tshow ln]
  in Color.attrCharToW32 colorSymbol : textToAL blurb

glueLines :: [AttrLine] -> [AttrLine] -> [AttrLine]
glueLines ov1 ov2 = reverse $ glue (reverse ov1) ov2
 where glue [] l = l
       glue m [] = m
       glue (mh : mt) (lh : lt) = reverse lt ++ (mh <+:> lh) : mt

-- @f@ should not enlarge the line beyond screen width.
updateLines :: Int -> (AttrLine -> AttrLine) -> [AttrLine] -> [AttrLine]
updateLines n f ov =
  let upd k (l : ls) = if k == 0
                       then f l : ls
                       else l : upd (k - 1) ls
      upd _ [] = []
  in upd n ov

-- blurb about [AttrLine]:
-- | A series of screen lines that either fit the width of the screen
-- or are intended for truncation when displayed. The length of overlay
-- may exceed the length of the screen, unlike in @SingleFrame@.
-- An exception is lines generated from animation, which have to fit
-- in either dimension.

-- * Overlay

type Overlay = [(Int, AttrLine)]

-- * Misc

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only
  deriving Eq

type FrameST s = G.Mutable U.Vector s Word32 -> ST s ()

newtype FrameForall = FrameForall {unFrameForall :: forall s. FrameST s}

writeLine :: Int -> AttrLine -> FrameForall
writeLine offset l = FrameForall $ \v -> do
  let writeAt _ [] = return ()
      writeAt off (ac32 : rest) = do
        VM.write v off (Color.attrCharW32 ac32)
        writeAt (off + 1) rest
  writeAt offset l
