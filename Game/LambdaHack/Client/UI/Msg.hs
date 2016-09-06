{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg, toPrompt
    -- * Report
  , RepMsgN, Report, emptyReport, nullReport, singletonReport
  , snocReport, consReportNoScrub
  , renderReport, findInReport, lastMsgOfReport
    -- * History
  , History, emptyHistory, addReport, lengthHistory, linesHistory
  , lastReportOfHistory, splitReportForHistory, renderHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Binary.Orphans ()
import GHC.Generics (Generic)

import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgLine :: !AttrLine  -- ^ the colours and characters of the message
  , msgHist :: !Bool      -- ^ whether the message should be recorded in history
  }
  deriving (Show, Eq, Generic)

instance Binary Msg

toMsg :: AttrLine -> Msg
toMsg l = Msg { msgLine = l
              , msgHist = True }

toPrompt :: AttrLine -> Msg
toPrompt l = Msg { msgLine = l
                 , msgHist = False }

-- * Report

data RepMsgN = RepMsgN {repMsg :: !Msg, _repN :: !Int}
  deriving (Show, Generic)

instance Binary RepMsgN

-- | The set of messages, with repetitions, to show at the screen at once.
newtype Report = Report [RepMsgN]
  deriving (Show, Binary)

-- | Empty set of messages.
emptyReport :: Report
emptyReport = Report []

-- | Test if the set of messages is empty.
nullReport :: Report -> Bool
nullReport (Report l) = null l

-- | Construct a singleton set of messages.
singletonReport :: Msg -> Report
singletonReport = snocReport emptyReport

-- | Add a message to the end of report. Deletes old prompt messages.
snocReport :: Report -> Msg -> Report
snocReport (Report !r) y =
  let scrubPrompts = filter (msgHist . repMsg)
  in case scrubPrompts r of
    _ | null $ msgLine y -> Report r
    RepMsgN x n : xns | x == y -> Report $ RepMsgN x (n + 1) : xns
    xns -> Report $ RepMsgN y 1 : xns

-- | Add a message to the end of report. Does not delete old prompt messages
-- nor handle repetitions.
consReportNoScrub :: Msg -> Report -> Report
consReportNoScrub Msg{msgLine=[]} rep = rep
consReportNoScrub y (Report r) = Report $ r ++ [RepMsgN y 1]

-- | Render a report as a (possibly very long) 'AttrLine'.
renderReport :: Report  -> AttrLine
renderReport (Report []) = []
renderReport (Report (x : xs)) =
  renderReport (Report xs) <+:> renderRepetition x

renderRepetition :: RepMsgN -> AttrLine
renderRepetition (RepMsgN s 1) = msgLine s
renderRepetition (RepMsgN s n) = msgLine s ++ toAttrLine ("<x" <> tshow n <> ">")

findInReport :: (AttrLine -> Bool) -> Report -> Maybe Msg
findInReport f (Report xns) = find (f . msgLine) $ map repMsg xns

lastMsgOfReport :: Report -> (AttrLine, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> ([], Report [])
  RepMsgN lmsg 1 : repRest -> (msgLine lmsg, Report repRest)
  RepMsgN lmsg n : repRest ->
    let !repMsg = RepMsgN lmsg (n - 1)
    in (msgLine lmsg, Report $ repMsg : repRest)

-- * History

-- | The history of reports. This is a ring buffer of the given length
newtype History = History (RB.RingBuffer (Time, Report))
  deriving (Show, Binary)

-- | Empty history of reports of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History $ RB.empty size (timeZero, Report [])

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport !(History rb) !time (Report m') =
  let !rep@(Report m) = Report $ filter (msgHist . repMsg) m'
  in case RB.uncons rb of
    _ | null m -> History rb
    Nothing -> History $ RB.cons (time, rep) rb
    Just ((oldTime, Report h), hRest) ->
      case (reverse m, h) of
        (RepMsgN s1 n1 : rs, RepMsgN s2 n2 : hhs) | s1 == s2 ->
          let !rephh = Report $ RepMsgN s2 (n1 + n2) : hhs
              hist = RB.cons (oldTime, rephh) hRest
          in History $ if null rs
                       then hist
                       else let !repr = Report $ reverse rs
                            in RB.cons (time, repr) hist
        _ -> History $ RB.cons (time, rep) rb

lengthHistory :: History -> Int
lengthHistory (History rs) = RB.length rs

linesHistory :: History -> [(Time, Report)]
linesHistory (History rb) = RB.toList rb

lastReportOfHistory :: History -> Maybe Report
lastReportOfHistory (History rb) = snd . fst <$> RB.uncons rb

splitReportForHistory :: X -> (Time, Report) -> (AttrLine, [AttrLine])
splitReportForHistory w (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible.
  -- or perhaps here display up to 4 decimal points
  let tturns = toAttrLine $ tshow $ time `timeFitUp` timeTurn
      ts = splitAttrLine (w - 1) $ tturns ++ toAttrLine ": " ++ renderReport r
      rep = case ts of
        [] -> []
        hd : tl -> hd : map (toAttrLine " " ++) tl
  in (tturns, rep)

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> Overlay
renderHistory (History rb) =
  let truncateForHistory (time, r) =
        -- TODO: display time fractions with granularity enough to differ
        -- from previous and next report, if possible
        let turns = time `timeFitUp` timeTurn
        in toAttrLine (tshow turns <> ": ") ++ renderReport r
  in map truncateForHistory $ RB.toList rb
