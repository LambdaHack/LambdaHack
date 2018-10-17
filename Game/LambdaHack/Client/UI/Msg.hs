{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg, toPrompt
    -- * Report
  , Report, nullReport, consReport, renderReport, findInReport
    -- * History
  , History, newReport, emptyHistory, addToReport, archiveReport, lengthHistory
  , renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , UAttrLine, RepMsgN, uToAttrLine, attrLineToU
  , emptyReport, snocReport, renderRepetition, scrapRepetition, renderTimeReport
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
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

-- | Add a message to the end of the report.
snocReport :: Report -> Msg -> Int -> Report
snocReport (Report !r) y n =
  if null $ msgLine y then Report r else Report $ RepMsgN y n : r

-- | Add a message to the start of report.
consReport :: Msg -> Report -> Report
consReport Msg{msgLine=[]} rep = rep
consReport y (Report r) = Report $ r ++ [RepMsgN y 1]

-- | Render a report as a (possibly very long) 'AttrLine'.
renderReport :: Report -> AttrLine
renderReport (Report []) = []
renderReport (Report (x : xs)) =
  renderReport (Report xs) <+:> renderRepetition x

renderRepetition :: RepMsgN -> AttrLine
renderRepetition (RepMsgN s 0) = msgLine s
renderRepetition (RepMsgN s 1) = msgLine s
renderRepetition (RepMsgN s n) = msgLine s ++ stringToAL ("<x" ++ show n ++ ">")

findInReport :: (AttrLine -> Bool) -> Report -> Maybe Msg
findInReport f (Report xns) = find (f . msgLine) $ map repMsg xns

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

scrapRepetition :: History -> Maybe History
scrapRepetition History{ newReport = Report newMsgs
                       , oldReport = Report oldMsgs
                       , .. } =
  case newMsgs of
    -- We take into account only first message of the new report,
    -- because others were deduplicated as they were added.
    -- We keep the message in the new report, because it should not
    -- vanish from the screen. In this way the message may be passed
    -- along many reports and, e.g., reduce disturbance over many turns,
    -- as for "X hears something".
    RepMsgN s1 n1 : rest1 ->
      let f (RepMsgN s2 _) = s1 == s2
      in case break f rest1 of
        (_, []) -> case break f oldMsgs of
          (_, []) -> Nothing
          (noDup, RepMsgN _ n2 : rest2) ->
            -- We keep the occurence of the message in the new report only.
            let newReport = Report $ RepMsgN s1 (n1 + n2) : rest1
                oldReport = Report $ noDup ++ rest2
            in Just History{..}
        (noDup, RepMsgN _ n2 : rest2) ->
          -- We keep the older (and so, oldest) occurence of the message,
          -- to avoid visual disruption by moving the message around.
          let newReport = Report $ noDup ++ RepMsgN s1 (n1 + n2) : rest2
              oldReport = Report oldMsgs
          in Just History{..}
    _ -> Nothing  -- empty new report

-- | Add a message to the new report of history, eliminating a possible
-- duplicate and noting its existence in the result.
addToReport :: History -> Msg -> Int -> (History, Bool)
addToReport History{..} msg n =
  let newH = History{newReport = snocReport newReport msg n, ..}
  in case scrapRepetition newH of
    Just scrappedH -> (scrappedH, True)
    Nothing -> (newH, False)

-- | Archive old report to history, filtering out prompts.
-- Set up new report with a new timestamp.
archiveReport :: History -> Time -> History
archiveReport History{newReport=Report newMsgs, ..} !newT =
  let f (RepMsgN _ n) = n > 0
      newReportNon0 = Report $ filter f newMsgs
  in if nullReport newReportNon0
     then -- Drop empty new report. Start a new one with the new timestamp.
          History emptyReport newT oldReport oldTime archivedHistory
     else let lU = map attrLineToU $ renderTimeReport oldTime oldReport
          in History emptyReport newT newReportNon0 newTime
             $ foldl' (\ !h !v -> RB.cons v h) archivedHistory (reverse lU)

renderTimeReport :: Time -> Report -> [AttrLine]
renderTimeReport !t (Report r') =
  let turns = t `timeFitUp` timeTurn
      rep = Report $ filter (msgHist . repMsg) r'
  in if nullReport rep
     then []
     else [stringToAL (show turns ++ ": ") ++ renderReport rep]

lengthHistory :: History -> Int
lengthHistory History{oldReport, archivedHistory} =
  RB.length archivedHistory + if nullReport oldReport then 0 else 1

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: History -> [AttrLine]
renderHistory History{..} = map uToAttrLine (RB.toList archivedHistory)
                            ++ renderTimeReport oldTime oldReport
