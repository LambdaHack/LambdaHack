{-# LANGUAGE RankNTypes, TupleSections #-}
-- | Screen overlays.
module Game.LambdaHack.Client.UI.Overlay
  ( -- * DisplayFont
    DisplayFont(..), textSize
  , -- * AttrString
    AttrString, blankAttrString, textToAS, textFgToAS, stringToAS
  , (<+:>), (<\:>)
    -- * AttrLine
  , AttrLine, attrLine, emptyAttrLine, attrStringToAL, firstParagraph
  , textToAL, textFgToAL, stringToAL, linesAttr
  , splitAttrString, indentSplitAttrString
    -- * Overlay
  , Overlay, xytranslateOverlay, xtranslateOverlay, ytranslateOverlay
  , offsetOverlay, offsetOverlayX, typesetXY
  , updateLine, rectangleOfSpaces, maxYofOverlay, labDescOverlay
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , nonbreakableRev, isPrefixOfNonbreakable, breakAtSpace, splitAttrPhrase
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Char (isSpace)
import qualified Data.Text as T

import           Game.LambdaHack.Client.UI.PointUI
import qualified Game.LambdaHack.Definition.Color as Color

-- * DisplayFont

-- | Three types of fonts used in the UI. Overlays (layers, more or less)
-- in proportional font are overwritten by layers in square font,
-- which are overwritten by layers in mono font.
-- All overlays overwrite the rendering of the game map, which is
-- the underlying basic UI frame, comprised of square font glyps.
--
-- Note that the order of constructors has limited effect (probably only
-- when square font is used instead of all other fonts and all overlays
-- are flattened), but it represents how overwriting is explicitly
-- implemented in frontends that support all fonts.
data DisplayFont = PropFont | SquareFont | MonoFont
  deriving (Show, Eq, Enum)

textSize :: DisplayFont -> [a] -> Int
textSize SquareFont l = 2 * length l
textSize MonoFont l = length l
textSize PropFont _ = error "size of proportional font texts is not defined"

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

-- | Line of colourful text. End of line characters forbidden. Trailing
-- @White@ space forbidden.
newtype AttrLine = AttrLine {attrLine :: AttrString}
  deriving (Show, Eq)

emptyAttrLine :: AttrLine
emptyAttrLine = AttrLine []

attrStringToAL :: AttrString -> AttrLine
attrStringToAL s =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (allB (\ac -> Color.charFromW32 ac /= '\n') s) $  -- expensive in menus
  assert (length s == 0 || last s /= Color.spaceAttrW32
          `blame` map Color.charFromW32 s) $
    -- only expensive for menus, but often violated by code changes, so disabled
    -- outside test runs
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

-- | Split a string into lines. Avoid breaking the line at a character
-- other than space. Remove the spaces on which lines are broken,
-- keep other spaces. In expensive assertions mode (dev debug mode)
-- fail at trailing spaces, but keep leading spaces, e.g., to make
-- distance from a text in another font. Newlines are respected.
--
-- Note that we only split wrt @White@ space, nothing else,
-- and the width, in the first argument, is calculated in characters,
-- not in UI (mono font) coordinates, so that taking and dropping characters
-- is performed correctly.
splitAttrString :: Int -> Int -> AttrString -> [AttrLine]
splitAttrString w0 w1 l = case linesAttr l of
  [] -> []
  x : xs -> splitAttrPhrase w0 w1 x ++ concatMap (splitAttrPhrase w1 w1) xs

indentSplitAttrString :: DisplayFont -> Int -> AttrString -> [AttrLine]
indentSplitAttrString font w l =
  -- Sadly this depends on how wide the space is in propotional font,
  -- which varies wildly, so we err on the side of larger indent.
  let nspaces = case font of
        SquareFont -> 1
        MonoFont -> 2
        PropFont -> 4
      ts = splitAttrString w (w - nspaces) l
      -- Proportional spaces are very narrow.
      spaces = replicate nspaces Color.spaceAttrW32
  in case ts of
    [] -> []
    hd : tl -> hd : map (AttrLine . (spaces ++) . attrLine) tl

-- We pass empty line along for the case of appended buttons, which need
-- either space or new lines before them.
splitAttrPhrase :: Int -> Int -> AttrLine -> [AttrLine]
splitAttrPhrase w0 w1 (AttrLine xs)
  | w0 >= length xs = [AttrLine xs]  -- no problem, everything fits
  | otherwise =
      let (pre, postRaw) = splitAt w0 xs
          preRev = reverse pre
          ((ppre, ppost), post) = case postRaw of
            c : rest | c == Color.spaceAttrW32
                       && not (isPrefixOfNonbreakable preRev) ->
              (([], preRev), rest)
            _ -> (breakAtSpace preRev, postRaw)
      in if all (== Color.spaceAttrW32) ppost
         then AttrLine (reverse $ dropWhile (== Color.spaceAttrW32) preRev) :
              splitAttrPhrase w1 w1 (AttrLine post)
         else AttrLine (reverse $ dropWhile (== Color.spaceAttrW32) ppost)
              : splitAttrPhrase w1 w1 (AttrLine $ reverse ppre ++ post)

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

xytranslateOverlay :: Int -> Int -> Overlay -> Overlay
xytranslateOverlay dx dy =
  map (\(PointUI x y, al) -> (PointUI (x + dx) (y + dy), al))

xtranslateOverlay :: Int -> Overlay -> Overlay
xtranslateOverlay dx = xytranslateOverlay dx 0

ytranslateOverlay :: Int -> Overlay -> Overlay
ytranslateOverlay = xytranslateOverlay 0

offsetOverlay :: [AttrLine] -> Overlay
offsetOverlay l = map (first $ PointUI 0) $ zip [0..] l

offsetOverlayX :: [(Int, AttrLine)] -> Overlay
offsetOverlayX l =
  map (\(y, (x, al)) -> (PointUI x y, al)) $ zip [0..] l

typesetXY :: (Int, Int) -> [AttrLine] -> Overlay
typesetXY (xoffset, yoffset) =
  map (\(y, al) -> (PointUI xoffset (y + yoffset), al)) . zip [0..]

-- @f@ should not enlarge the line beyond screen width nor introduce linebreaks.
updateLine :: Int -> (Int -> AttrString -> AttrString) -> Overlay -> Overlay
updateLine y f ov =
  let upd (p@(PointUI px py), AttrLine l) =
        if py == y then (p, AttrLine $ f px l) else (p, AttrLine l)
  in map upd ov

rectangleOfSpaces :: Int -> Int -> Overlay
rectangleOfSpaces x y =
  let blankAttrLine = AttrLine $ replicate x Color.nbspAttrW32
  in offsetOverlay $ replicate y blankAttrLine

maxYofOverlay :: Overlay -> Int
maxYofOverlay ov = let yOfOverlay (PointUI _ y, _) = y
                   in maximum $ 0 : map yOfOverlay ov

labDescOverlay :: DisplayFont -> Int -> AttrString -> (Overlay, Overlay)
labDescOverlay labFont width as =
  let (tLab, tDesc) = span (/= Color.spaceAttrW32) as
      labLen = textSize labFont tLab
      ovLab = offsetOverlay [attrStringToAL tLab]
      ovDesc = offsetOverlayX $
        case splitAttrString (width - labLen) width tDesc of
          [] -> []
          l : ls -> (labLen, l) : map (0,) ls
  in (ovLab, ovDesc)
