{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Common.Msg
  ( makePhrase, makeSentence
  , Msg, (<>), (<+>), tshow, toWidth, moreMsg, endMsg, yesnoMsg, truncateMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg, prependMsg
  , splitReport, renderReport, findInReport, lastMsgOfReport
  , History, emptyHistory, lengthHistory, linesHistory
  , addReport, renderHistory, splitReportForHistory, lastReportOfHistory
  , Overlay(overlay), emptyOverlay, toOverlayRaw, truncateToOverlay, toOverlay
  , updateOverlayLine
  , Slideshow(slideshow), splitOverlay, toSlideshow
  , ScreenLine, toScreenLine, splitText
  )
  where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Binary.Orphans ()
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

infixr 6 <+>  -- TODO: not needed when we require a very new minimorph
(<+>) :: Text -> Text -> Text
(<+>) = (MU.<+>)

-- Show and pack the result of @show@.
tshow :: Show a => a -> Text
tshow x = T.pack $ show x

toWidth :: Int -> Text -> Text
toWidth n x = T.take n (T.justifyLeft n ' ' x)

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrregular
makeSentence = MU.makeSentence MU.defIrregular

-- | The type of a single message.
type Msg = Text

-- | The \"press something to see more\" mark.
moreMsg :: Msg
moreMsg = "--more--  "

-- | The \"end of screenfuls of text\" mark.
endMsg :: Msg
endMsg = "--end--  "

-- | The confirmation request message.
yesnoMsg :: Msg
yesnoMsg = "[yn]"

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trims (does not wrap!) too long lines. In case of newlines,
-- displays only the first line, but marks the message as partial.
truncateMsg :: X -> Text -> Text
truncateMsg w xsRaw =
  let xs = case T.lines xsRaw of
        [] -> xsRaw
        [line] -> line
        line : _ -> T.justifyLeft (w + 1) ' ' line
      len = T.length xs
  in case compare w len of
       LT -> T.snoc (T.take (w - 1) xs) '$'
       EQ -> xs
       GT -> if T.null xs || T.last xs == ' '
             then xs
             else T.snoc xs ' '

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(BS.ByteString, Int)]
  deriving (Show, Binary)

-- | Empty set of messages.
emptyReport :: Report
emptyReport = Report []

-- | Test if the set of messages is empty.
nullReport :: Report -> Bool
nullReport (Report l) = null l

-- | Construct a singleton set of messages.
singletonReport :: Msg -> Report
singletonReport = addMsg emptyReport

-- TODO: Differentiate from msgAdd. Generally, invent more informative names.
-- | Add message to the end of report.
addMsg :: Report -> Msg -> Report
addMsg r m | T.null m = r
addMsg (Report ((x, n) : xns)) y' | x == y =
  Report $ (y, n + 1) : xns
 where y = encodeUtf8 y'
addMsg (Report xns) y = Report $ (encodeUtf8 y, 1) : xns

prependMsg :: Msg -> Report -> Report
prependMsg m r | T.null m = r
prependMsg y (Report xns) = Report $ xns ++ [(encodeUtf8 y, 1)]

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: X -> Report -> Overlay
splitReport w r = toOverlay $ splitReportList w r

splitReportList :: X -> Report -> [Text]
splitReportList w r = splitText w $ renderReport r

-- | Render a report as a (possibly very long) string.
renderReport :: Report  -> Text
renderReport (Report []) = T.empty
renderReport (Report (xn : xs)) =
  renderReport (Report xs) <+> renderRepetition xn

renderRepetition :: (BS.ByteString, Int) -> Text
renderRepetition (s, 1) = decodeUtf8 s
renderRepetition (s, n) = decodeUtf8 s <> "<x" <> tshow n <> ">"

findInReport :: (BS.ByteString -> Bool) -> Report -> Maybe BS.ByteString
findInReport f (Report xns) = find f $ map fst xns

lastMsgOfReport :: Report -> (BS.ByteString, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> assert `failure` rep
  (lmsg, 1) : repRest -> (lmsg, Report repRest)
  (lmsg, n) : repRest -> (lmsg, Report $ (lmsg, n - 1) : repRest)

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitText :: X -> Text -> [Text]
splitText w xs = concatMap (splitText' w . T.stripStart) $ T.lines xs

splitText' :: X -> Text -> [Text]
splitText' w xs
  | w >= T.length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = T.splitAt w xs
          (ppre, ppost) = T.break (== ' ') $ T.reverse pre
          testPost = T.stripEnd ppost
      in if T.null testPost
         then pre : splitText w post
         else T.reverse ppost : splitText w (T.reverse ppre <> post)

-- | The history of reports. This is a ring buffer of the given length
newtype History = History (RB.RingBuffer (Time, Report))
  deriving (Show, Binary)

-- | Empty history of reports of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History $ RB.empty size (timeZero, Report [])

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport h _ (Report []) = h
addReport !(History rb) !time !rep@(Report m) =
  case RB.uncons rb of
    Nothing -> History $ RB.cons (time, rep) rb
    Just ((oldTime, Report h), hRest) ->
      case (reverse m, h) of
        ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
          let hist = RB.cons (oldTime, Report ((s2, n1 + n2) : hhs)) hRest
          in History $ if null rs
                       then hist
                       else RB.cons (time, Report (reverse rs)) hist
        _ -> History $ RB.cons (time, rep) rb

lengthHistory :: History -> Int
lengthHistory (History rs) = RB.rbLength rs

linesHistory :: History -> [(Time, Report)]
linesHistory (History rb) = RB.toList rb

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> Overlay
renderHistory (History rb) =
  let l = RB.toList rb
      (x, _) = normalLevelBound
      reportLines = map (truncateHistory (x + 1)) l
  in toOverlay reportLines

truncateHistory :: X -> (Time, Report) -> Text
truncateHistory w (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible
  let turns = time `timeFitUp` timeTurn
  in truncateMsg w $ tshow turns <> ":" <+> renderReport r

splitReportForHistory :: X -> (Time, Report) -> (Text, [Text])
splitReportForHistory w (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible.
  -- or perhaps here display up to 4 decimal points
  let tturns = tshow $ time `timeFitUp` timeTurn
      ts = splitText (w - 1) $ tturns <> ":" <+> renderReport r
      rep = case ts of
        [] -> []
        hd : tl -> hd : map (T.cons ' ') tl
  in (tturns, rep)

lastReportOfHistory :: History -> Maybe Report
lastReportOfHistory (History rb) = snd . fst <$> RB.uncons rb

type ScreenLine = [AttrChar]

toScreenLine :: Text -> ScreenLine
toScreenLine t = let f = AttrChar defAttr
                 in map f $ T.unpack t

-- | A series of screen lines that may or may not fit the width nor height
-- of the screen. An overlay may be transformed by adding the first line
-- and/or by splitting into a slideshow of smaller overlays.
newtype Overlay = Overlay {overlay :: [ScreenLine]}
  deriving (Show, Eq)

emptyOverlay :: Overlay
emptyOverlay = Overlay []

-- TODO: get rid of
toOverlayRaw :: [ScreenLine] -> Overlay
toOverlayRaw = Overlay

truncateToOverlay :: Text -> Overlay
truncateToOverlay msg = toOverlay [msg]

toOverlay :: [Text] -> Overlay
toOverlay = let lxsize = fst normalLevelBound + 1  -- TODO
            in Overlay . map (toScreenLine . truncateMsg lxsize)

-- @f@ should not enlarge the line beyond screen width.
updateOverlayLine :: Int -> ([AttrChar] -> [AttrChar]) -> Overlay -> Overlay
updateOverlayLine n f Overlay{overlay} =
  let upd k (l : ls) = if k == 0
                       then f l : ls
                       else l : upd (k - 1) ls
      upd _ [] = []
  in Overlay $ upd n overlay

-- | Split an overlay into a slideshow in which each overlay,
-- prefixed by @msg@ and postfixed by @moreMsg@ except for the last one,
-- fits on the screen wrt height (but lines may be too wide).
splitOverlay :: Y -> Overlay -> Overlay -> Slideshow
splitOverlay yspace (Overlay msg) (Overlay ls0) =
  let len = length msg
  in if len >= yspace
     then  -- no space left for @ls0@
       Slideshow ([Overlay $ take (yspace - 1) msg
                                    ++ [toScreenLine moreMsg]])
     else let splitO ls =
                let (pre, post) = splitAt (yspace - 1) $ msg ++ ls
                in if null (drop 1 post)  -- (don't call @length@ on @ls0@)
                   then [Overlay $ msg ++ ls]  -- all fits on screen
                   else let rest = splitO post
                        in Overlay (pre ++ [toScreenLine moreMsg]) : rest
          in Slideshow (splitO ls0)

-- | A few overlays, displayed one by one upon keypress.
-- When displayed, they are trimmed, not wrapped
-- and any lines below the lower screen edge are not visible.
-- The first pair element determines if the overlay is displayed
-- over a blank screen, including the bottom lines.
newtype Slideshow = Slideshow {slideshow :: [Overlay]}
  deriving (Show, Eq)

instance Monoid Slideshow where
  mempty = Slideshow []
  mappend (Slideshow l1) (Slideshow l2) = Slideshow (l1 ++ l2)

-- | Declare the list of raw overlays to be fit for display on the screen.
-- In particular, current @Report@ is eiter empty or unimportant
-- or contained in the overlays and if any vertical or horizontal
-- trimming of the overlays happens, this is intended.
toSlideshow :: [[Text]] -> Slideshow
toSlideshow l = Slideshow $ map toOverlay l
