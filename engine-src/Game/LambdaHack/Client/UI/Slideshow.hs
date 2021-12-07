{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( FontOverlayMap, maxYofFontOverlayMap
  , KeyOrSlot, MenuSlot, natSlots
  , ButtonWidth(..)
  , KYX, xytranslateKXY, xtranslateKXY, ytranslateKXY, yrenumberKXY
  , OKX, emptyOKX, xytranslateOKX, sideBySideOKX, labDescOKX
  , Slideshow(slideshow), emptySlideshow, unsnoc, toSlideshow
  , attrLinesToFontMap, menuToSlideshow, wrapOKX, splitOverlay, splitOKX
  , highSlideshow
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , keysOKX, showTable, showNearbyScores
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           Data.Time.LocalTime

import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import qualified Game.LambdaHack.Common.HighScore as HighScore
import qualified Game.LambdaHack.Definition.Color as Color

type FontOverlayMap = EM.EnumMap DisplayFont Overlay

maxYofFontOverlayMap :: FontOverlayMap -> Int
maxYofFontOverlayMap ovs = maximum (0 : map maxYofOverlay (EM.elems ovs))

type KeyOrSlot = Either K.KM MenuSlot

newtype MenuSlot = MenuSlot Int
  deriving (Show, Eq, Ord, Binary, Enum)

natSlots :: [MenuSlot]
{-# INLINE natSlots #-}
natSlots = [MenuSlot 0 ..]

-- TODO: probably best merge the PointUI into that and represent
-- the position as characters, too, translating to UI positions as needed.
-- The problem is that then I need to do a lot of reverse translation
-- when creating buttons.
-- | Width of on-screen button text, expressed in characters,
-- and so UI (mono font) width is deduced from the used font.
data ButtonWidth = ButtonWidth
  { buttonFont  :: DisplayFont
  , buttonWidth :: Int }
  deriving (Show, Eq)

-- | A key or a menu slot at a given position on the screen.
type KYX = (KeyOrSlot, (PointUI, ButtonWidth))

xytranslateKXY :: Int -> Int -> KYX -> KYX
xytranslateKXY dx dy (km, (PointUI x y, len)) =
  (km, (PointUI (x + dx) (y + dy), len))

xtranslateKXY :: Int -> KYX -> KYX
xtranslateKXY dx = xytranslateKXY dx 0

ytranslateKXY :: Int -> KYX -> KYX
ytranslateKXY = xytranslateKXY 0

yrenumberKXY :: Int -> KYX -> KYX
yrenumberKXY ynew (km, (PointUI x _, len)) = (km, (PointUI x ynew, len))

-- | An Overlay of text with an associated list of keys or slots
-- that activate when the specified screen position is pointed at.
-- The list should be sorted wrt rows and then columns.
type OKX = (FontOverlayMap, [KYX])

emptyOKX :: OKX
emptyOKX = (EM.empty, [])

xytranslateOKX ::Int -> Int -> OKX -> OKX
xytranslateOKX dx dy (ovs, kyxs) =
  ( EM.map (xytranslateOverlay dx dy) ovs
  , map (xytranslateKXY dx dy) kyxs )

sideBySideOKX :: Int -> Int -> OKX -> OKX -> OKX
sideBySideOKX dx dy (ovs1, kyxs1) (ovs2, kyxs2) =
  let (ovs3, kyxs3) = xytranslateOKX dx dy (ovs2, kyxs2)
  in ( EM.unionWith (++) ovs1 ovs3
     , sortOn (\(_, (PointUI x y, _)) -> (y, x)) $ kyxs1 ++ kyxs3 )

-- The bangs are to free the possibly very long input list ASAP.
labDescOKX :: DisplayFont -> DisplayFont
           -> [(AttrString, AttrString, KeyOrSlot)]
           -> OKX
labDescOKX labFont descFont l =
  let descFontSize | isPropFont descFont = length  -- may be less or a bit more
                   | otherwise = textSize descFont
      processRow :: (AttrString, AttrString, KeyOrSlot)
                 -> (AttrLine, (Int, AttrLine), KYX)
      processRow (!tLab, !tDesc, !ekm) =
        let labLen = textSize labFont tLab
            lenButton = labLen + descFontSize tDesc
        in ( attrStringToAL tLab
           , (labLen, attrStringToAL tDesc)
           , (ekm, (PointUI 0 0, ButtonWidth descFont lenButton)) )
      (tsLab, tsDesc, kxs) = unzip3 $ map processRow l
      ovs = EM.insertWith (++) labFont (offsetOverlay tsLab)
            $ EM.singleton descFont $ offsetOverlayX tsDesc
  in (ovs, zipWith yrenumberKXY [0..] kxs)

-- | A list of active screenfulls to be shown one after another.
-- Each screenful has an independent numbering of rows and columns.
newtype Slideshow = Slideshow {slideshow :: [OKX]}
  deriving (Show, Eq)

emptySlideshow :: Slideshow
emptySlideshow = Slideshow []

unsnoc :: Slideshow -> Maybe (Slideshow, OKX)
unsnoc Slideshow{slideshow} =
  case reverse slideshow of
    [] -> Nothing
    okx : rest -> Just (Slideshow $ reverse rest, okx)

toSlideshow :: FontSetup -> Bool -> [OKX] -> Slideshow
toSlideshow FontSetup{..}displayTutorialHints okxs =
  Slideshow $ addFooters False okxs
 where
  atEnd = flip (++)
  appendToFontOverlayMap :: FontOverlayMap -> String
                         -> (FontOverlayMap, PointUI, DisplayFont, Int)
  appendToFontOverlayMap ovs msgPrefix =
    let msg | displayTutorialHints =
              msgPrefix
              ++ "  (ESC to exit, PGUP, HOME, mouse, wheel, arrows, etc.)"
            | otherwise = msgPrefix
        maxYminXofOverlay ov =
          let ymxOfOverlay (PointUI x y, _) = (- y, x)
          in minimum $ maxBound : map ymxOfOverlay ov
        -- @sortOn@ less efficient here, because function cheap.
        assocsYX = sortBy (comparing snd)
                   $ EM.assocs $ EM.map maxYminXofOverlay ovs
        (fontMax, yMax) = case assocsYX of
          [] -> (monoFont, 0)
          (font, (yNeg, _x)) : rest ->
            let unique = all (\(_, (yNeg2, _)) -> yNeg /= yNeg2) rest
            in ( if isSquareFont font && unique
                 then font
                 else monoFont
               , - yNeg )
        pMax = PointUI 0 (yMax + 1)  -- append after last line
    in ( EM.insertWith atEnd fontMax [(pMax, stringToAL msg)] ovs
       , pMax
       , fontMax
       , length msg )
  addFooters :: Bool -> [OKX] -> [OKX]
  addFooters _ [] = error $ "" `showFailure` okxs
  addFooters _ [(als, [])] =
    -- TODO: make sure this case never coincides with the space button
    -- actually returning to top, as opposed to finishing preview.
    let (ovs, p, font, width) = appendToFontOverlayMap als "--end--"
    in [(ovs, [(Left K.safeSpaceKM, (p, ButtonWidth font width))])]
  addFooters False [(als, kxs)] = [(als, kxs)]
  addFooters True [(als, kxs)] =
    let (ovs, p, font, width) = appendToFontOverlayMap als "--back to top--"
    in [(ovs, kxs ++ [(Left K.safeSpaceKM, (p, ButtonWidth font width))])]
  addFooters _ ((als, kxs) : rest) =
    let (ovs, p, font, width) = appendToFontOverlayMap als "--more--"
    in (ovs, kxs ++ [(Left K.safeSpaceKM, (p, ButtonWidth font width))])
       : addFooters True rest

-- | This appends vertically a list of blurbs into a single font overlay map.
-- Not to be used if some blurbs need to be places overlapping vertically,
-- e.g., when the square font symbol needs to be in the same line
-- as the start of the descritpion of the denoted item
-- or when mono font buttons need to be after a prompt.
attrLinesToFontMap :: [(DisplayFont, [AttrLine])] -> FontOverlayMap
attrLinesToFontMap blurb =
  let zipAttrLines :: Int -> [AttrLine] -> (Overlay, Int)
      zipAttrLines start als =
        ( zipWith (curry (first $ PointUI 0)) [start ..] als
        , start + length als )
      addOverlay :: (FontOverlayMap, Int) -> (DisplayFont, [AttrLine])
                 -> (FontOverlayMap, Int)
      addOverlay (!em, !start) (font, als) =
        let (als2, start2) = zipAttrLines start als
        in ( EM.insertWith (++) font als2 em
           , start2 )
      (ov, _) = foldl' addOverlay (EM.empty, 0) blurb
  in ov

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (EM.null als || null kxs)) $ Slideshow [(als, kxs)]

wrapOKX :: DisplayFont -> Int -> Int -> Int -> [(K.KM, String)]
        -> (Overlay, [KYX])
wrapOKX _ _ _ _ [] = ([], [])
wrapOKX displayFont ystart xstart width ks =
  let overlayLineFromStrings :: Int -> Int -> [String] -> (PointUI, AttrLine)
      overlayLineFromStrings xlineStart y strings =
        let p = PointUI xlineStart y
        in (p, stringToAL $ unwords (reverse strings))
      f :: ((Int, Int), (Int, [String], Overlay, [KYX])) -> (K.KM, String)
        -> ((Int, Int), (Int, [String], Overlay, [KYX]))
      f ((y, x), (xlineStart, kL, kV, kX)) (key, s) =
        let len = textSize displayFont s
            len1 = len + textSize displayFont " "
        in if x + len >= width
           then let iov = overlayLineFromStrings xlineStart y kL
                in f ((y + 1, 0), (0, [], iov : kV, kX)) (key, s)
           else ( (y, x + len1)
                , ( xlineStart
                  , s : kL
                  , kV
                  , (Left key, ( PointUI x y
                               , ButtonWidth displayFont (length s) ))
                    : kX ) )
      ((ystop, _), (xlineStop, kL1, kV1, kX1)) =
        foldl' f ((ystart, xstart), (xstart, [], [], [])) ks
      iov1 = overlayLineFromStrings xlineStop ystop kL1
  in (reverse $ iov1 : kV1, reverse kX1)

keysOKX :: DisplayFont -> Int -> Int -> Int -> [K.KM] -> (Overlay, [KYX])
keysOKX displayFont ystart xstart width keys =
  let wrapB :: String -> String
      wrapB s = "[" ++ s ++ "]"
      ks = map (\key -> (key, wrapB $ K.showKM key)) keys
  in wrapOKX displayFont ystart xstart width ks

-- The font argument is for the report and keys overlay. Others already have
-- assigned fonts.
splitOverlay :: FontSetup -> Bool -> Int -> Int -> Int -> Report -> [K.KM]
             -> OKX
             -> Slideshow
splitOverlay fontSetup displayTutorialHints
             width height wrap report keys (ls0, kxs0) =
  let renderedReport = renderReport True report
      reportAS = foldr (<\:>) [] renderedReport
  in toSlideshow fontSetup displayTutorialHints $
       splitOKX fontSetup False width height wrap reportAS keys (ls0, kxs0)

-- Note that we only split wrt @White@ space, nothing else.
splitOKX :: FontSetup -> Bool -> Int -> Int -> Int -> AttrString -> [K.KM]
         -> OKX
         -> [OKX]
splitOKX FontSetup{..} msgLong width height wrap reportAS keys (ls0, kxs0) =
  assert (height > 2) $
  assert (width > 2) $  -- if the strings to split are long these minimums won't be enough
  let reportParagraphs = linesAttr reportAS
      -- TODO: until SDL support for measuring prop font text is released,
      -- we have to use MonoFont for the paragraph that ends with buttons.
      (repProp, repMono) =
        if null keys
        then (reportParagraphs, emptyAttrLine)
        else case reverse reportParagraphs of
          [] -> ([], emptyAttrLine)
          l : rest ->
            (reverse rest, attrStringToAL $ attrLine l ++ [Color.nbspAttrW32])
      msgWrap = if msgLong && not (isSquareFont propFont)
                then 2 * width
                else wrap  -- TODO if with width fits on one screen, use it
      msgWidth = if msgLong && not (isSquareFont propFont)
                 then 2 * width
                 else width
      repProp0 = offsetOverlay $ case repProp of
        [] -> []
        r : rs ->
          -- Make lines of first paragraph long if it has 2 lines at most.
          -- The first line does not obscure anything and the second line
          -- is often short anyway.
          let firstWidth = if length (attrLine r) <= 2 * msgWidth
                           then msgWidth
                           else msgWrap
          in (indentSplitAttrString propFont firstWidth . attrLine) r
               -- first possibly long
             ++ concatMap (indentSplitAttrString propFont msgWrap . attrLine) rs
      -- TODO: refactor this ugly pile of copy-paste
      repPropW = offsetOverlay
                 $ concatMap (indentSplitAttrString propFont width . attrLine)
                             repProp
      -- If the mono portion first on the line, let it take half width,
      -- but if previous lines shorter, match them and only buttons
      -- are permitted to stick out.
      monoWidth = if null repProp then msgWidth else msgWrap
      repMono0 = ytranslateOverlay (length repProp0)
                 $ offsetOverlay
                 $ indentSplitAttrString monoFont monoWidth $ attrLine repMono
      repMonoW = ytranslateOverlay (length repPropW)
                 $ offsetOverlay
                 $ indentSplitAttrString monoFont width $ attrLine repMono
      repWhole0 = offsetOverlay
                  $ concatMap (indentSplitAttrString propFont msgWidth
                               . attrLine)
                              reportParagraphs
      repWhole1 = ytranslateOverlay 1 repWhole0
      lenOfRep0 = length repProp0 + length repMono0
      lenOfRepW = length repPropW + length repMonoW
      startOfKeys = if null repMono0
                    then 0
                    else textSize monoFont (attrLine $ snd $ last repMono0)
      startOfKeysW = if null repMonoW
                     then 0
                     else textSize monoFont (attrLine $ snd $ last repMonoW)
      pressAKey = stringToAS "A long report is shown. Press a key:"
                  ++ [Color.nbspAttrW32]
      (lX0, keysX0) = keysOKX monoFont 0 (length pressAKey) width keys
      (lX1, keysX1) = keysOKX monoFont 1 0 width keys
      (lX, keysX) = keysOKX monoFont (max 0 $ lenOfRep0 - 1) startOfKeys
                            (2 * width) keys
      (lXW, keysXW) = keysOKX monoFont (max 0 $ lenOfRepW - 1) startOfKeysW
                              (2 * width) keys
      splitO :: Int -> (Overlay, Overlay, [KYX]) -> OKX -> [OKX]
      splitO yoffset (hdrProp, hdrMono, rk) (ls, kxs) =
        let hdrOff | null hdrProp && null hdrMono = 0
                   | otherwise = 1 + maxYofOverlay hdrMono
            keyTranslate = map $ ytranslateKXY (hdrOff - yoffset)
            lineTranslate = EM.map $ ytranslateOverlay (hdrOff - yoffset)
            yoffsetNew = yoffset + height - hdrOff - 1
            ltOffset :: (PointUI, a) -> Bool
            ltOffset (PointUI _ y, _) = y < yoffsetNew
            (pre, post) = ( filter ltOffset <$> ls
                          , filter (not . ltOffset) <$> ls )
            prependHdr = EM.insertWith (++) propFont hdrProp
                         . EM.insertWith (++) monoFont hdrMono
        in if all null $ EM.elems post  -- all fits on one screen
           then [(prependHdr $ lineTranslate pre, rk ++ keyTranslate kxs)]
           else let (preX, postX) = span (\(_, pa) -> ltOffset pa) kxs
                in (prependHdr $ lineTranslate pre, rk ++ keyTranslate preX)
                   : splitO yoffsetNew (hdrProp, hdrMono, rk) (post, postX)
      firstParaReport = firstParagraph reportAS
      hdrShortened = ( [(PointUI 0 0, firstParaReport)]
                         -- shortened for the main slides; in full beforehand
                     , take 3 lX1  -- 3 lines ought to be enough for everyone
                     , keysX1 )
      ((lsInit, kxsInit), (headerProp, headerMono, rkxs)) =
        -- Check whether all space taken by report and keys.
        if | (lenOfRep0 + length lX) < height ->  -- display normally
             (emptyOKX, (repProp0, lX ++ repMono0, keysX))
           | (lenOfRepW + length lXW) < height ->  -- display widely
             (emptyOKX, (repPropW, lXW ++ repMonoW, keysXW))
           | length reportParagraphs == 1
             && length (attrLine firstParaReport) <= 2 * width ->
             ( emptyOKX  -- already shown in full in @hdrShortened@
             , hdrShortened )
           | otherwise -> case lX0 of
               [] ->
                 ( (EM.singleton propFont repWhole0, [])
                     -- showing in full in the init slide
                 , hdrShortened )
               lX0first : _ ->
                 ( ( EM.insertWith (++) propFont repWhole1
                     $ EM.singleton monoFont
                         [(PointUI 0 0, firstParagraph pressAKey), lX0first]
                   , filter (\(_, (PointUI _ y, _)) -> y == 0) keysX0 )
                 , hdrShortened )
      initSlides = if EM.null lsInit
                   then assert (null kxsInit) []
                   else splitO 0 ([], [], []) (lsInit, kxsInit)
      -- If @ls0@ is not empty, we still want to display the report,
      -- one way or another.
      mainSlides = if EM.null ls0 && not (EM.null lsInit)
                   then assert (null kxs0) []
                   else splitO 0 (headerProp, headerMono, rkxs) (ls0, kxs0)
  in initSlides ++ mainSlides

-- | Generate a slideshow with the current and previous scores.
highSlideshow :: FontSetup
              -> Bool
              -> Int        -- ^ width of the display area
              -> Int        -- ^ height of the display area
              -> HighScore.ScoreTable -- ^ current score table
              -> Int        -- ^ position of the current score in the table
              -> Text       -- ^ the name of the game mode
              -> TimeZone   -- ^ the timezone where the game is run
              -> Slideshow
highSlideshow fontSetup@FontSetup{monoFont} displayTutorialHints
              width height table pos gameModeName tz =
  let entries = (height - 3) `div` 3
      msg = HighScore.showAward entries table pos gameModeName
      tts = map offsetOverlay $ showNearbyScores tz pos table entries
      al = textToAS msg
      splitScreen ts =
        splitOKX fontSetup False width height width al [K.spaceKM, K.escKM]
                 (EM.singleton monoFont ts, [])
  in toSlideshow fontSetup displayTutorialHints $ concatMap splitScreen tts

-- | Show a screenful of the high scores table.
-- Parameter @entries@ is the number of (3-line) scores to be shown.
showTable :: TimeZone -> Int -> HighScore.ScoreTable -> Int -> Int
          -> [AttrLine]
showTable tz pos table start entries =
  let zipped    = zip [1..] $ HighScore.unTable table
      screenful = take entries . drop (start - 1) $ zipped
      renderScore (pos1, score1) =
        map (if pos1 == pos then textFgToAL Color.BrWhite else textToAL)
        $ HighScore.showScore tz pos1 score1
  in emptyAttrLine : intercalate [emptyAttrLine] (map renderScore screenful)

-- | Produce a couple of renderings of the high scores table.
showNearbyScores :: TimeZone -> Int -> HighScore.ScoreTable -> Int
                 -> [[AttrLine]]
showNearbyScores tz pos h entries =
  if pos <= entries
  then [showTable tz pos h 1 entries]
  else [ showTable tz pos h 1 entries
       , showTable tz pos h (max (entries + 1) (pos - entries `div` 2))
                   entries ]
