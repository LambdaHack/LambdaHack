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
  pofOv :: Overlay -> PointI
  pofOv [] = 0
  pofOv l@((pFirst, _) : _) =
    let (pLast, _) = last l
        Point pxFirst _ = toEnum pFirst  -- match x of the header
        Point _ pyLast = toEnum pLast  -- append after last line
    in fromEnum $ Point pxFirst (pyLast + 1)
  atEnd = flip (++)
  appendToFontOverlayMap :: FontOverlayMap -> AttrLine -> (FontOverlayMap, Int)
  appendToFontOverlayMap ovs al =
    case EM.lookup PropFont ovs of
      Just ovF ->
        (EM.insertWith atEnd PropFont [(pofOv ovF, al)] ovs, length ovF)
      Nothing -> case EM.lookup MonoFont ovs of
        Just ovF ->
          (EM.insertWith atEnd MonoFont [(pofOv ovF, al)] ovs, length ovF)
        Nothing -> case EM.lookup SquareFont ovs of
          Just ovF ->
            (EM.insertWith atEnd SquareFont [(pofOv ovF, al)] ovs, length ovF)
          Nothing -> (EM.insertWith atEnd PropFont [(pofOv [], al)] ovs, 0)
  addFooters :: Bool -> [OKX] -> [OKX]
  addFooters _ [] = error $ "" `showFailure` okxsNotNull
  addFooters _ [(als, [])] =
    let (ovs, len) = appendToFontOverlayMap als (stringToAL endMsg)
    in [(ovs, [(Left [K.safeSpaceKM], (len, 0, 15))])]
  addFooters False [(als, kxs)] = [(als, kxs)]
  addFooters True [(als, kxs)] =
    let (ovs, len) = appendToFontOverlayMap als (stringToAL endMsg)
    in [(ovs, kxs ++ [(Left [K.safeSpaceKM], (len, 0, 15))])]
  addFooters _ ((als, kxs) : rest) =
    let (ovs, len) = appendToFontOverlayMap als (stringToAL moreMsg)
    in (ovs, kxs ++ [(Left [K.safeSpaceKM], (len, 0, 8))])
       : addFooters True rest

moreMsg :: String
moreMsg = "--more--  "

endMsg :: String
endMsg = "--back to top--  "

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (EM.null als || null kxs)) $ Slideshow [(als, kxs)]

wrapOKX :: Y -> X -> X -> [(K.KM, String)] -> (Overlay, [KYX])
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
  let msgRaw = offsetOverlay width $ splitAttrLine width rrep
      (lX0, keysX0) = keysOKX 0 0 maxBound keys
      pX0 = case lX0 of
        [] -> 0
        (p, _) : _ -> p
      (lX, keysX) =
        if null msgRaw
        then (lX0, keysX0)
        else keysOKX (length msgRaw - 1)(length (snd $ last msgRaw) + 1)
                     width keys
      msgOkx = (glueLines msgRaw lX, keysX)
      ((lsInit, kxsInit), (header, rkxs)) =
        -- Check whether most space taken by report and keys.
        if length (glueLines msgRaw lX0) * 2 > height
        then ( msgOkx
             , ( [(pX0, intercalate [Color.spaceAttrW32] (map snd lX0)
                        <+:> rrep)]
               , keysX0 ) )
               -- will display "$" (unless has EOLs)
        else (([], []), msgOkx)
      renumber y (km, (y0, x1, x2)) = (km, (y0 + y, x1, x2))
      renumberOv y = map (\(p, al) -> (p + y * width, al))
      splitO :: Y -> (Overlay, [KYX]) -> OKX -> [OKX]
      splitO yoffset (hdr, rk) (ls, kxs) =
        let keyRenumber = map $ renumber (length hdr - yoffset)
            lineRenumber = EM.map $ renumberOv (length hdr - yoffset)
            yoffsetNew = yoffset + height - length hdr - 1
            ltOffset (p, _) = p < yoffsetNew * width
            (pre, post) = ( filter ltOffset <$> ls
                          , filter (not . ltOffset) <$> ls )
            prependHdr = EM.insertWith (++) displayFont hdr
        in if all null $ EM.elems post  -- all fits on one screen
           then [(prependHdr $ lineRenumber pre, rk ++ keyRenumber kxs)]
           else let (preX, postX) =
                      break (\(_, (y1, _, _)) -> y1 >= yoffsetNew) kxs
                in (prependHdr $ lineRenumber pre, rk ++ keyRenumber preX)
                   : splitO yoffsetNew (hdr, rk) (post, postX)
      initSlides = if null lsInit
                   then assert (null kxsInit) []
                   else splitO 0 ([], [])
                               (EM.singleton displayFont lsInit, kxsInit)
      mainSlides = if EM.null ls0 && not (null lsInit)
                   then assert (null kxs0) []
                   else splitO 0 (header, rkxs) (ls0, kxs0)
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
