{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg, toPrompt
    -- * Report
  , RepMsgN, Report, emptyReport, nullReport, snocReport, consReportNoScrub
  , renderReport, findInReport, incrementInReport, lastMsgOfReport
    -- * History
  , History, newReport, emptyHistory, addToReport, archiveReport, lengthHistory
  , lastReportOfHistory, replaceLastReportOfHistory, replaceNewReportOfHistory
  , renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , UAttrLine, uToAttrLine, attrLineToU
  , renderRepetition, renderTimeReport,
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.RingBuffer as RB
import           Game.LambdaHack.Common.Time

-- * UAttrLine

type UAttrLine = U.Vector Word32

uToAttrLine :: UAttrLine -> AttrLine
uToAttrLine v = map Color.AttrCharW32 $ U.toList v

attrLineToU :: AttrLine -> UAttrLine
attrLineToU l = U.fromList $ map Color.attrCharW32 l

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgLine :: AttrLine  -- ^ the colours and characters of the message
  , msgHist :: Bool      -- ^ whether message should be recorded in history
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

data RepMsgN = RepMsgN {repMsg :: Msg, _repN :: Int}
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

-- | Add a message to the start of the report.
snocReport :: Report -> Msg -> Report
snocReport (Report !r) y = case r of
  _ | null $ msgLine y -> Report r
  RepMsgN x n : xns | x == y -> Report $ RepMsgN x (n + 1) : xns
  xns -> Report $ RepMsgN y 1 : xns

-- | Add a message to the end of report. Does not delete old prompt messages
-- nor handle repetitions.
consReportNoScrub :: Msg -> Report -> Report
consReportNoScrub Msg{msgLine=[]} rep = rep
consReportNoScrub y (Report r) = Report $ r ++ [RepMsgN y 1]

-- | Render a report as a (possibly very long) 'AttrLine'.
renderReport :: Report -> AttrLine
renderReport (Report []) = []
renderReport (Report (x : xs)) =
  renderReport (Report xs) <+:> renderRepetition x

renderRepetition :: RepMsgN -> AttrLine
renderRepetition (RepMsgN s 1) = msgLine s
renderRepetition (RepMsgN s n) = msgLine s ++ stringToAL ("<x" ++ show n ++ ">")

findInReport :: (AttrLine -> Bool) -> Report -> Maybe Msg
findInReport f (Report xns) = find (f . msgLine) $ map repMsg xns

incrementInReport :: (AttrLine -> Bool) -> Report -> Maybe Report
incrementInReport f (Report xns) =
  case break (f . msgLine . repMsg) xns of
    (pre, msg : post) ->
      Just $ Report $ pre ++ msg {_repN = _repN msg + 1} : post
    _ -> Nothing

lastMsgOfReport :: Report -> (AttrLine, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> ([], Report [])
  RepMsgN lmsg 1 : repRest -> (msgLine lmsg, Report repRest)
  RepMsgN lmsg n : repRest ->
    let !repMsg = RepMsgN lmsg (n - 1)
    in (msgLine lmsg, Report $ repMsg : repRest)

-- * History

-- | The history of reports. This is a ring buffer of the given length
-- containing old archived history and two most recent reports stored
-- separately.
data History = History
  { newReport       :: Report
  , newTime         :: Time
  , oldReport       :: Report
  , oldTime         :: Time
  , archivedHistory :: RB.RingBuffer UAttrLine }
  deriving (Show, Generic)

instance Binary History

-- | Empty history of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History emptyReport timeZero emptyReport timeZero
                    $ RB.empty size U.empty

-- | Add a message to the new report of history.
addToReport :: History -> Msg -> History
addToReport History{..} msg = History{newReport = snocReport newReport msg, ..}

-- | Archive old report to history, handling repetitions and filtering prompts.
archiveReport :: History -> Time -> History
archiveReport h@History{newReport=Report []} _ = h
archiveReport History{ newReport = newReport@(Report newMsgs)
                     , oldReport = oldReport@(Report oldMsgs)
                     , .. }
              !newT =
  let renderAppend oldR newR =
        let lU = map attrLineToU $ renderTimeReport oldTime oldR
        in History emptyReport newT newR newTime
           $ foldl' (flip RB.cons) archivedHistory (reverse lU)
  in case (reverse oldMsgs, newMsgs) of
      (RepMsgN s1 n1 : rest1, RepMsgN s2 n2 : rest2) | s1 == s2 ->
        let oldR = Report $ reverse rest1
            newR = Report $ RepMsgN s2 (n1 + n2) : rest2
        in renderAppend oldR newR
      _ -> renderAppend oldReport newReport

renderTimeReport :: Time -> Report -> [AttrLine]
renderTimeReport !t (Report r') =
  let turns = t `timeFitUp` timeTurn
      rep = Report $ filter (msgHist . repMsg) r'
  in if nullReport rep
     then []
     else [stringToAL (show turns ++ ": ") ++ renderReport rep]

lengthHistory :: History -> Int
lengthHistory History{archivedHistory} = RB.length archivedHistory

lastReportOfHistory :: History -> Report
lastReportOfHistory (History _ _ r _ _) = r

replaceLastReportOfHistory :: Report -> Report -> History -> History
replaceLastReportOfHistory rep0 rep (History _ t _ t2 rb) =
  History rep0 t rep t2 rb

replaceNewReportOfHistory :: Report -> History -> History
replaceNewReportOfHistory rep0 (History _ t r t2 rb) = History rep0 t r t2 rb

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: History -> [AttrLine]
renderHistory History{..} = map uToAttrLine (RB.toList archivedHistory)
                            ++ renderTimeReport oldTime oldReport
