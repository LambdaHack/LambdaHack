-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( DisplayFont(..), FontOverlayMap
  , KYX, OKX, Slideshow(slideshow)
  , emptySlideshow, unsnoc, toSlideshow, menuToSlideshow
  , wrapOKX, splitOverlay, splitOKX, highSlideshow
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , moreMsg, endMsg, keysOKX, showTable, showNearbyScores
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Time.LocalTime

import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

data DisplayFont = SquareFont | MonoFont | PropFont
  deriving (Show, Eq, Enum)

type FontOverlayMap = EM.EnumMap DisplayFont Overlay

-- | A key or an item slot label at a given position on the screen.
type KYX = (Either [K.KM] SlotChar, (Y, X, X))

-- | An Overlay of text with an associated list of keys or slots
-- that activated when the specified screen position is pointed at.
-- The list should be sorted wrt rows and then columns.
type OKX = (FontOverlayMap, [KYX])

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

toSlideshow :: [OKX] -> Slideshow
toSlideshow okxs = Slideshow $ addFooters False okxsNotNull
 where
  okxFilter (ov, kyxs) =
    (ov, filter (either (not . null) (const True) . fst) kyxs)
  okxsNotNull = map okxFilter okxs
  pofOv :: Overlay -> (X, Y)
  pofOv [] = (0, 0)
  pofOv l@((pFirst, _) : _) =
    let pMax = maximum $ map fst l
        Point pxFirst _ = toEnum pFirst  -- match x of the header
        Point _ pyLast = toEnum pMax  -- append after last line
    in (pxFirst, pyLast + 1)
  atEnd = flip (++)
  appendToFontOverlayMap :: FontOverlayMap -> AttrLine
                         -> (FontOverlayMap, X, Y)
  appendToFontOverlayMap ovs al =
    let insertAl square ovF =
          let (x, y) = pofOv ovF
              p = fromEnum $ Point x y
              displayFont = if square then SquareFont else MonoFont
          in (EM.insertWith atEnd displayFont [(p, al)] ovs, x, y)
    in case EM.lookup PropFont ovs of
      Just ovF -> insertAl False ovF
      Nothing -> case EM.lookup MonoFont ovs of
        Just ovF -> insertAl False ovF
        Nothing -> case EM.lookup SquareFont ovs of
          Just ovF -> insertAl True ovF
          Nothing -> insertAl False []
  addFooters :: Bool -> [OKX] -> [OKX]
  addFooters _ [] = error $ "" `showFailure` okxsNotNull
  addFooters _ [(als, [])] =
    let (ovs, x, y) = appendToFontOverlayMap als (stringToAL endMsg)
    in [(ovs, [(Left [K.safeSpaceKM], (y, x, x + 15))])]
  addFooters False [(als, kxs)] = [(als, kxs)]
  addFooters True [(als, kxs)] =
    let (ovs, x, y) = appendToFontOverlayMap als (stringToAL endMsg)
    in [(ovs, kxs ++ [(Left [K.safeSpaceKM], (y, x, x + 15))])]
  addFooters _ ((als, kxs) : rest) =
    let (ovs, x, y) = appendToFontOverlayMap als (stringToAL moreMsg)
    in (ovs, kxs ++ [(Left [K.safeSpaceKM], (y, x, x + 8))])
       : addFooters True rest

moreMsg :: String
moreMsg = "--more--  "

endMsg :: String
endMsg = "--back to top--  "

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (EM.null als || null kxs)) $ Slideshow [(als, kxs)]

wrapOKX :: Y -> X -> X -> [(K.KM, String)] -> (Overlay, [KYX])
wrapOKX _ _ _ [] = ([], [])
wrapOKX ystart xstart width ks =
  let f :: ((Y, X), (X, [String], Overlay, [KYX])) -> (K.KM, String)
        -> ((Y, X), (X, [String], Overlay, [KYX]))
      f ((y, x), (xlineStart, kL, kV, kX)) (key, s) =
        let len = length s
        in if x + len >= width
           then let p = fromEnum $ Point xlineStart y
                    iov = (p, stringToAL $ intercalate " " (reverse kL))
                in f ((y + 1, 0), (0, [], iov : kV, kX)) (key, s)
           else ( (y, x + len + 1)
                , (xlineStart, s : kL, kV, (Left [key], (y, x, x + len)) : kX) )
      ((ystop, _), (xlineStop, kL1, kV1, kX1)) =
        foldl' f ((ystart, xstart), (xstart, [], [], [])) ks
      p1 = fromEnum $ Point xlineStop ystop
      iov1 = (p1, stringToAL $ intercalate " " (reverse kL1))
  in (reverse $ iov1 : kV1, reverse kX1)

keysOKX :: Y -> X -> X -> [K.KM] -> (Overlay, [KYX])
keysOKX ystart xstart width keys =
  let wrapB :: String -> String
      wrapB s = "[" ++ s ++ "]"
      ks = map (\key -> (key, wrapB $ K.showKM key)) keys
  in wrapOKX ystart xstart width ks

-- The font argument is for the report and keys overlay. Others already have
-- assigned fonts.
splitOverlay :: DisplayFont -> X -> Y -> Report -> [K.KM] -> OKX -> Slideshow
splitOverlay displayFont width height report keys (ls0, kxs0) =
  toSlideshow $ splitOKX displayFont width height (renderReport report)
                         keys (ls0, kxs0)

-- Note that we only split wrt @White@ space, nothing else.
splitOKX :: DisplayFont -> X -> Y -> AttrLine -> [K.KM] -> OKX -> [OKX]
splitOKX displayFont width height rrep keys (ls0, kxs0) =
  assert (height > 2) $  -- and kxs0 is sorted
  let msgRaw0 = offsetOverlay width $ splitAttrLine width rrep
      msgRaw1 = map (\(p, al) -> (p + width, al)) msgRaw0
      (lX0, keysX0) = keysOKX 0 0 width keys
      (lX1, keysX1) = keysOKX 1 0 width keys
      endOfMsgRaw = if null rrep then 0 else length (snd $ last msgRaw0) + 1
      endOfMsg = if displayFont /= SquareFont
                 then endOfMsgRaw `divUp` 2
                 else endOfMsgRaw
      (lX, keysX) = keysOKX (length msgRaw0 - 1) endOfMsg width keys
      renumber y (km, (y0, x1, x2)) = (km, (y0 + y, x1, x2))
      renumberOv y = map (\(p, al) -> (p + y * width, al))
      splitO :: Y -> (Overlay, Overlay, [KYX]) -> OKX -> [OKX]
      splitO yoffset (hdrFont, hdrMono, rk) (ls, kxs) =
        let hdrOff | null hdrFont && null hdrMono = 0
                   | otherwise = 1 + maximum (0 : map fst hdrMono) `divUp` width
            keyRenumber = map $ renumber (hdrOff - yoffset)
            lineRenumber = EM.map $ renumberOv (hdrOff - yoffset)
            yoffsetNew = yoffset + height - hdrOff - 1
            ltOffset (p, _) = p < yoffsetNew * width
            (pre, post) = ( filter ltOffset <$> ls
                          , filter (not . ltOffset) <$> ls )
            -- TODO: until SDL support for measuring prop font text is released,
            -- we have to put MonoFont also for hdrFont; undo when possible
            msgFont = if null hdrMono then PropFont else MonoFont
            prependHdr = EM.insertWith (++) msgFont hdrFont
                         . EM.insertWith (++) MonoFont hdrMono
        in if all null $ EM.elems post  -- all fits on one screen
           then [(prependHdr $ lineRenumber pre, rk ++ keyRenumber kxs)]
           else let (preX, postX) =
                      break (\(_, (y1, _, _)) -> y1 >= yoffsetNew) kxs
                in (prependHdr $ lineRenumber pre, rk ++ keyRenumber preX)
                   : splitO yoffsetNew (hdrFont, hdrMono, rk) (post, postX)
      hdrShortened = ( [(0, rrep)]  -- shortened for the main slides
                     , take (height - 1) lX1  -- try to fit on one screen
                     , keysX1 )
      ((lsInit, kxsInit), (headerFont, headerMono, rkxs)) =
        -- Check whether most space taken by report and keys.
        if | (length msgRaw0 + length lX) * 2 <= height ->
             ((EM.empty, []), (msgRaw0, lX, keysX))  -- display normally
           | length rrep <= width ->  -- very crude check, but OK
             ( (EM.empty, [])  -- already shown in full in shortened header
             , hdrShortened )
           | otherwise -> case lX0 of
               [] ->
                 ( (EM.singleton displayFont msgRaw0, [])
                     -- showing in full in the init slide
                 , hdrShortened )
               lX0first : _ ->
                 ( ( EM.insertWith (++) displayFont msgRaw1
                     $ EM.singleton MonoFont [lX0first]
                   , filter (\(_, (y, _, _)) -> y == 0) keysX0 )
                 , hdrShortened )
      initSlides = if EM.null lsInit
                   then assert (null kxsInit) []
                   else splitO 0 ([], [], []) (lsInit, kxsInit)
      -- If @ls0@ we still want to display the report, one way or another.
      mainSlides = if EM.null ls0 && (not $ EM.null lsInit)
                   then assert (null kxs0) []
                   else splitO 0 (headerFont, headerMono, rkxs) (ls0, kxs0)
  in initSlides ++ mainSlides

-- | Generate a slideshow with the current and previous scores.
highSlideshow :: X          -- ^ width of the display area
              -> Y          -- ^ height of the display area
              -> HighScore.ScoreTable -- ^ current score table
              -> Int        -- ^ position of the current score in the table
              -> Text       -- ^ the name of the game mode
              -> TimeZone   -- ^ the timezone where the game is run
              -> Slideshow
highSlideshow width height table pos gameModeName tz =
  let entries = (height - 3) `div` 3
      msg = HighScore.showAward entries table pos gameModeName
      tts = map (offsetOverlay width) $ showNearbyScores tz pos table entries
      al = textToAL msg
      splitScreen ts =
        splitOKX MonoFont width height al [K.spaceKM, K.escKM]
                 (EM.singleton MonoFont ts, [])
  in toSlideshow $ concat $ map splitScreen tts

-- | Show a screenful of the high scores table.
-- Parameter @entries@ is the number of (3-line) scores to be shown.
showTable :: TimeZone -> Int -> HighScore.ScoreTable -> Int -> Int -> [AttrLine]
showTable tz pos table start entries =
  let zipped    = zip [1..] $ HighScore.unTable table
      screenful = take entries . drop (start - 1) $ zipped
      renderScore (pos1, score1) =
        map (if pos1 == pos then textFgToAL Color.BrWhite else textToAL)
        $ HighScore.showScore tz pos1 score1
  in [] : intercalate [[]] (map renderScore screenful)

-- | Produce a couple of renderings of the high scores table.
showNearbyScores :: TimeZone -> Int -> HighScore.ScoreTable -> Int
                 -> [[AttrLine]]
showNearbyScores tz pos h entries =
  if pos <= entries
  then [showTable tz pos h 1 entries]
  else [showTable tz pos h 1 entries,
        showTable tz pos h (max (entries + 1) (pos - entries `div` 2)) entries]
