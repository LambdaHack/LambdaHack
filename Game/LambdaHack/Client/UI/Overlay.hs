{-# LANGUAGE RankNTypes #-}
-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * AttrLine
    AttrLine, emptyAttrLine, textToAL, fgToAL, stringToAL, (<+:>)
    -- * Overlay
  , Overlay, IntOverlay
  , splitAttrLine, indentSplitAttrLine, glueLines, updateLines
    -- * Misc
  , ColorMode(..)
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , linesAttr, splitAttrPhrase
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Point

-- * AttrLine

-- | Line of colourful text.
type AttrLine = [Color.AttrCharW32]

emptyAttrLine :: Int -> AttrLine
emptyAttrLine w = replicate w Color.spaceAttrW32

textToAL :: Text -> AttrLine
textToAL !t =
  let f c l = let !ac = Color.attrChar1ToW32 c
              in ac : l
  in T.foldr f [] t

-- | Render line of text in the given foreground colour.
fgToAL :: Color.Color -> Text -> AttrLine
fgToAL !fg !t =
  let f c l = let !ac = Color.attrChar2ToW32 fg c
              in ac : l
  in T.foldr f [] t

stringToAL :: String -> AttrLine
stringToAL = map Color.attrChar1ToW32

infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ [Color.spaceAttrW32] ++ l2

-- * Overlay

-- | A series of screen lines that either fit the width of the screen
-- or are intended for truncation when displayed. The length of overlay
-- may exceed the length of the screen, unlike in @SingleFrame@.
-- An exception is lines generated from animation, which have to fit
-- in either dimension.
type Overlay = [AttrLine]

-- | Sparse screen overlay representation where only the indicated rows
-- are overlayed and the remaining rows are kept unchanged.
type IntOverlay = [(Int, AttrLine)]

-- | Split a string into lines. Avoids ending the line with
-- a character other than space. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitAttrLine :: X -> AttrLine -> Overlay
splitAttrLine w l =
  concatMap (splitAttrPhrase w . dropWhile (== Color.spaceAttrW32))
  $ linesAttr l

indentSplitAttrLine :: X -> AttrLine -> [AttrLine]
indentSplitAttrLine w l =
  -- First line could be split at @w@, not @w - 1@, but it's good enough.
  let ts = splitAttrLine (w - 1) l
  in case ts of
    [] -> []
    hd : tl -> hd : map ([Color.spaceAttrW32] ++) tl

linesAttr :: AttrLine -> Overlay
linesAttr l | null l = []
            | otherwise = h : if null t then [] else linesAttr (tail t)
 where (h, t) = span (/= Color.retAttrW32) l

splitAttrPhrase :: X -> AttrLine -> Overlay
splitAttrPhrase w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, postRaw) = splitAt w xs
          ((ppre, ppost), post) = case postRaw of
            c : rest | c == Color.spaceAttrW32 -> (([], reverse pre), rest)
            _ -> (break (== Color.spaceAttrW32) $ reverse pre, postRaw)
          testPost = dropWhileEnd (== Color.spaceAttrW32) ppost
      in if null testPost
         then pre : splitAttrPhrase w post
         else reverse ppost : splitAttrPhrase w (reverse ppre ++ post)

glueLines :: Overlay -> Overlay -> Overlay
glueLines ov1 ov2 = reverse $ glue (reverse ov1) ov2
 where glue [] l = l
       glue m [] = m
       glue (mh : mt) (lh : lt) = reverse lt ++ (mh <+:> lh) : mt

-- @f@ should not enlarge the line beyond screen width.
updateLines :: Int -> (AttrLine -> AttrLine) -> Overlay -> Overlay
updateLines n f ov =
  let upd k (l : ls) = if k == 0
                       then f l : ls
                       else l : upd (k - 1) ls
      upd _ [] = []
  in upd n ov

-- * Misc

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black and white only
  deriving Eq
