{-# LANGUAGE RankNTypes #-}
-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * AttrLine
    AttrLine, emptyAttrLine, textToAL, textFgToAL, stringToAL, (<+:>)
    -- * Overlay
  , Overlay
  , offsetOverlay, splitAttrLine, indentSplitAttrLine, glueLines, updateLine
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , linesAttr, splitAttrPhrase
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

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

textFgToAL :: Color.Color -> Text -> AttrLine
textFgToAL !fg !t =
  let f ' ' l = Color.spaceAttrW32 : l
                  -- for speed and simplicity (testing if char is a space)
                  -- we always keep the space @White@
      f c l = let !ac = Color.attrChar2ToW32 fg c
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

-- | A series of screen lines with points at which they should ber overlayed
-- over the base frame or a blank screen, depending on context.
-- The point is represented as in integer that is an index into the
-- frame character array.
-- The lines either fit the width of the screen or are intended
-- for truncation when displayed. The positions of lines may fall outside
-- the length of the screen, too, unlike in @SingleFrame@. Then they are
-- simply not shown.
type Overlay = [(PointI, AttrLine)]

offsetOverlay :: X -> [AttrLine] -> Overlay
offsetOverlay width l = map (\(y, al) -> (y * width, al)) $ zip [0..] l

-- | Split a string into lines. Avoids ending the line with
-- a character other than space. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
--
-- Note that we only split wrt @White@ space, nothing else.
splitAttrLine :: X -> AttrLine -> [AttrLine]
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

linesAttr :: AttrLine -> [AttrLine]
linesAttr l | null l = []
            | otherwise = h : if null t then [] else linesAttr (tail t)
 where (h, t) = span (/= Color.retAttrW32) l

-- We consider only these, because they are short and form a closed category.
nonbreakableRev :: [AttrLine]
nonbreakableRev = map stringToAL ["eht", "a", "na", "ehT", "A", "nA"]

breakAtSpace :: AttrLine -> (AttrLine, AttrLine)
breakAtSpace lRev =
  let (pre, post) = break (== Color.spaceAttrW32) lRev
  in case post of
    c : rest | c == Color.spaceAttrW32 ->
      if any (`isPrefixOf` rest) nonbreakableRev
      then let (pre2, post2) = breakAtSpace rest
           in (pre ++ c : pre2, post2)
      else (pre, post)
    _ -> (pre, post)  -- no space found, give up

splitAttrPhrase :: X -> AttrLine -> [AttrLine]
splitAttrPhrase w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, postRaw) = splitAt w xs
          preRev = reverse pre
          ((ppre, ppost), post) = case postRaw of
            c : rest | c == Color.spaceAttrW32
                       && not (any (`isPrefixOf` preRev) nonbreakableRev) ->
              (([], preRev), rest)
            _ -> (breakAtSpace preRev, postRaw)
          testPost = dropWhileEnd (== Color.spaceAttrW32) ppost
      in if null testPost
         then pre : splitAttrPhrase w post
         else reverse ppost : splitAttrPhrase w (reverse ppre ++ post)

-- There are pretty very strong assumptions here: only one overlap,
-- between the last line of the first argument and the first line
-- of the second, all lines of @ov1@ start at the first column,
-- the concatenation in the overlapping line fits in line width.
-- We keep the order of lines to ensure the assumptions hold
-- for nested calls.
glueLines :: Overlay -> Overlay -> Overlay
glueLines ov1 ov2 = reverse $ glue (reverse ov1) ov2
 where glue [] l = l
       glue m [] = m
       glue ((p, mh) : mt) ((_, lh) : lt) =
         let offsetOv = \(mp, ma) -> (mp + p, ma)
         in reverse (map offsetOv lt) ++ (p, mh <+:> lh) : mt

-- @f@ should not enlarge the line beyond screen width.
updateLine :: Y -> (X -> AttrLine -> AttrLine) -> Overlay -> Overlay
updateLine y f ov =
  let upd (p, l) = let Point{..} = toEnum p
                   in if py == y then (p, f px l) else (p, l)
  in map upd ov
