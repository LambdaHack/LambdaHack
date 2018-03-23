{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg, toPrompt
    -- * Report
  , RepMsgN, Report, emptyReport, nullReport, singletonReport
  , snocReport, consReportNoScrub
  , renderReport, findInReport, incrementInReport, lastMsgOfReport
    -- * History
  , History, emptyHistory, addReport, lengthHistory
  , lastReportOfHistory, replaceLastReportOfHistory, renderHistory
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

-- | Construct a singleton set of messages.
singletonReport :: Msg -> Report
singletonReport = snocReport emptyReport

-- | Add a message to the end of report.
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
data History = History Time Report (RB.RingBuffer UAttrLine)
  deriving (Show, Generic)

instance Binary History

-- | Empty history of reports of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History timeZero emptyReport $ RB.empty size U.empty

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport histOld@(History oldT oldRep@(Report h) hRest) !time rep@(Report m) =
  if null m then histOld else
    case (reverse m, h) of
      (RepMsgN s1 n1 : rs, RepMsgN s2 n2 : hhs) | s1 == s2 ->
        let rephh = Report $ RepMsgN s2 (n1 + n2) : hhs
        in if null rs
           then History oldT rephh hRest
           else let repr = Report $ reverse rs
                    lU = reverse $ map attrLineToU $ renderTimeReport oldT rephh
                in History time repr $ foldl' (flip RB.cons) hRest lU
      (_, []) -> History time rep hRest
      _ -> let lU = reverse $ map attrLineToU $ renderTimeReport oldT oldRep
           in History time rep $ foldl' (flip RB.cons) hRest lU

renderTimeReport :: Time -> Report -> [AttrLine]
renderTimeReport !t (Report r') =
  let turns = t `timeFitUp` timeTurn
      rep = Report $ filter (msgHist . repMsg) r'
  in if nullReport rep
     then []
     else [stringToAL (show turns ++ ": ") ++ renderReport rep]

lengthHistory :: History -> Int
lengthHistory (History _ r rs) = RB.length rs + if nullReport r then 0 else 1

lastReportOfHistory :: History -> Report
lastReportOfHistory (History _ r _) = r

replaceLastReportOfHistory :: Report -> History -> History
replaceLastReportOfHistory rep (History t _r rb) = History t rep rb

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> [AttrLine]
renderHistory (History t r rb) =
  map uToAttrLine (RB.toList rb) ++ renderTimeReport t r
