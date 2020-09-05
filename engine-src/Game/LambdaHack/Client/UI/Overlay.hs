{-# LANGUAGE RankNTypes #-}
-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * AttrString
    AttrString, blankAttrString, textToAS, textFgToAS, stringToAS
  , (<+:>), (<\:>)
    -- * AttrLine
  , AttrLine, attrLine, emptyAttrLine, attrStringToAL, firstParagraph, linesAttr
  , textToAL, textFgToAL, stringToAL, splitAttrString, indentSplitAttrString
    -- * Overlay
  , Overlay, offsetOverlay, offsetOverlayX, updateLine
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , splitAttrPhrase
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Char (isSpace)
import qualified Data.Text as T

import           Game.LambdaHack.Client.UI.Key (PointUI (..))
import qualified Game.LambdaHack.Definition.Color as Color

-- * AttrString

-- | String of colourful text. End of line characters permitted.
type AttrString = [Color.AttrCharW32]

blankAttrString :: Int -> AttrString
blankAttrString w = replicate w Color.spaceAttrW32

textToAS :: Text -> AttrString
textToAS !t =
  let f c l = let !ac = Color.attrChar1ToW32 c
              in ac : l
  in T.foldr f [] t

textFgToAS :: Color.Color -> Text -> AttrString
textFgToAS !fg !t =
  let f ' ' l = Color.spaceAttrW32 : l
                  -- for speed and simplicity (testing if char is a space)
                  -- we always keep the space @White@
      f c l = let !ac = Color.attrChar2ToW32 fg c
              in ac : l
  in T.foldr f [] t

stringToAS :: String -> AttrString
stringToAS = map Color.attrChar1ToW32

-- Follows minimorph.<+>.
infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrString -> AttrString -> AttrString
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2@(c2 : _) =
  if isSpace (Color.charFromW32 c2) || isSpace (Color.charFromW32 (last l1))
  then l1 ++ l2
  else l1 ++ [Color.spaceAttrW32] ++ l2

infixr 6 <\:>  -- matches Monoid.<>
(<\:>) :: AttrString -> AttrString -> AttrString
(<\:>) [] l2 = l2
(<\:>) l1 [] = l1
(<\:>) l1 l2@(c2 : _) =
  if Color.charFromW32 c2 == '\n' || Color.charFromW32 (last l1) == '\n'
  then l1 ++ l2
  else l1 ++ stringToAS "\n" ++ l2

-- We consider only these, because they are short and form a closed category.
nonbreakableRev :: [String]
nonbreakableRev = ["eht", "a", "na", "ehT", "A", "nA", "I"]

isPrefixOfNonbreakable :: AttrString -> Bool
isPrefixOfNonbreakable s =
  let isPrefixOfNb sRev nbRev = case stripPrefix nbRev sRev of
        Nothing -> False
        Just [] -> True
        Just (c : _) -> isSpace c
  in any (isPrefixOfNb $ map Color.charFromW32 s) nonbreakableRev

breakAtSpace :: AttrString -> (AttrString, AttrString)
breakAtSpace lRev =
  let (pre, post) = break (== Color.spaceAttrW32) lRev
  in case post of
    c : rest | c == Color.spaceAttrW32 ->
      if isPrefixOfNonbreakable rest
      then let (pre2, post2) = breakAtSpace rest
           in (pre ++ c : pre2, post2)
      else (pre, post)
    _ -> (pre, post)  -- no space found, give up

-- * AttrLine

-- | Line of colourful text. End of line characters forbidden.
newtype AttrLine = AttrLine {attrLine :: AttrString}
  deriving (Show, Eq)

emptyAttrLine :: AttrLine
emptyAttrLine = AttrLine []

attrStringToAL :: AttrString -> AttrLine
attrStringToAL s =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (all (\ac -> Color.charFromW32 ac /= '\n') s) $  -- expensive in menus
  assert (length s == 0 || last s /= Color.spaceAttrW32
          `blame` map Color.charFromW32 s) $
    -- only expensive for menus, but often violated by changes, so disabled
#endif
    AttrLine s

firstParagraph :: AttrString -> AttrLine
firstParagraph s = case linesAttr s of
  [] -> emptyAttrLine
  l : _ -> l

textToAL :: Text -> AttrLine
textToAL !t =
  let f '\n' _ = error $ "illegal end of line in: " ++ T.unpack t
      f c l = let !ac = Color.attrChar1ToW32 c
              in ac : l
      s = T.foldr f [] t
  in AttrLine $
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (length s == 0 || last s /= Color.spaceAttrW32 `blame` t)
#endif
    s

textFgToAL :: Color.Color -> Text -> AttrLine
textFgToAL !fg !t =
  let f '\n' _ = error $ "illegal end of line in: " ++ T.unpack t
      f ' ' l = Color.spaceAttrW32 : l
                  -- for speed and simplicity (testing if char is a space)
                  -- we always keep the space @White@
      f c l = let !ac = Color.attrChar2ToW32 fg c
              in ac : l
      s = T.foldr f [] t
  in AttrLine $
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (length s == 0 || last s /= Color.spaceAttrW32 `blame` t)
#endif
    s

stringToAL :: String -> AttrLine
stringToAL s = attrStringToAL $ map Color.attrChar1ToW32 s

-- Mimics @lines@.
linesAttr :: AttrString -> [AttrLine]
linesAttr [] = []
linesAttr l = cons (case break (\ac -> Color.charFromW32 ac == '\n') l of
  (h, t) -> (attrStringToAL h, case t of
                                 [] -> []
                                 _ : tt -> linesAttr tt))
 where
  cons ~(h, t) = h : t

-- | Split a string into lines. Avoids breaking the line at a character
-- other than space. Remove space characters from the starts and ends
-- of created lines. Newlines are respected.
--
-- Note that we only split wrt @White@ space, nothing else,
-- and the width, in the first argument, is calculated in characters,
-- not in UI (mono font) coordinates, so that taking and dropping characters
-- is performed correctly.
splitAttrString :: Int -> AttrString -> [AttrLine]
splitAttrString w l =
  concatMap (splitAttrPhrase w
             . AttrLine . dropWhile (== Color.spaceAttrW32) . attrLine)
  $ linesAttr l

indentSplitAttrString :: Int -> AttrString -> [AttrLine]
indentSplitAttrString w l =
  -- First line could be split at @w@, not @w - 1@, but it's good enough.
  let ts = splitAttrString (w - 1) l
  in case ts of
    [] -> []
    hd : tl -> hd : map (AttrLine . ([Color.spaceAttrW32] ++) . attrLine) tl

-- We pass empty line along for the case of appended buttons, which need
-- either space or new lines before them.
splitAttrPhrase :: Int -> AttrLine -> [AttrLine]
splitAttrPhrase w (AttrLine xs)
  | w >= length xs = [AttrLine xs]  -- no problem, everything fits
  | otherwise =
      let (pre, postRaw) = splitAt w xs
          preRev = reverse pre
          ((ppre, ppost), post) = case postRaw of
            c : rest | c == Color.spaceAttrW32
                       && not (isPrefixOfNonbreakable preRev) ->
              (([], preRev), rest)
            _ -> (breakAtSpace preRev, postRaw)
      in if all (== Color.spaceAttrW32) ppost
         then AttrLine (reverse $ dropWhile (== Color.spaceAttrW32) preRev) :
              splitAttrPhrase w (AttrLine post)
         else AttrLine (reverse $ dropWhile (== Color.spaceAttrW32) ppost)
              : splitAttrPhrase w (AttrLine $ reverse ppre ++ post)

-- * Overlay

-- | A series of screen lines with start positions at which they should
-- be overlayed over the base frame or a blank screen, depending on context.
-- The position point is represented as in integer that is an index into the
-- frame character array.
-- The lines either fit the width of the screen or are intended
-- for truncation when displayed. The start positions of lines may fall outside
-- the length of the screen, too, unlike in @SingleFrame@. Then they are
-- simply not shown.
type Overlay = [(PointUI, AttrLine)]

offsetOverlay :: [AttrLine] -> Overlay
offsetOverlay l = map (first $ PointUI 0) $ zip [0..] l

offsetOverlayX :: [(Int, AttrLine)] -> Overlay
offsetOverlayX l =
  map (\(y, (x, al)) -> (PointUI x y, al)) $ zip [0..] l

-- @f@ should not enlarge the line beyond screen width nor introduce linebreaks.
updateLine :: Int -> (Int -> AttrString -> AttrString) -> Overlay -> Overlay
updateLine y f ov =
  let upd (p@(PointUI px py), AttrLine l) =
        if py == y then (p, AttrLine $ f px l) else (p, AttrLine l)
  in map upd ov
