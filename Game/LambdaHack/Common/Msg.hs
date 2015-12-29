{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Common.Msg
  ( Msg
  , moreMsg, endMsg, yesnoMsg
  , splitText
  , Report, emptyReport, nullReport, singletonReport, addMsg, prependMsg
  , renderReport, findInReport, lastMsgOfReport
  , History, emptyHistory, lengthHistory, linesHistory, renderHistory
  , addReport, splitReportForHistory, lastReportOfHistory
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

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

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

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> [Text]
renderHistory (History rb) = map truncateHistory $ RB.toList rb

truncateHistory :: (Time, Report) -> Text
truncateHistory (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible
  let turns = time `timeFitUp` timeTurn
  in tshow turns <> ":" <+> renderReport r
