{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg
  , MsgClass(..), interruptsRunning, disturbsResting
    -- * Report
  , Report, nullReport, consReport, renderReport, anyInReport
    -- * History
  , History, newReport, emptyHistory, addToReport, archiveReport, lengthHistory
  , renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , colorAttrChar, isSavedToHistory, isDisplayed, msgColor
  , UAttrLine, RepMsgN, uToAttrLine, attrLineToU
  , emptyReport, snocReport, renderWholeReport, renderRepetition
  , scrapRepetition, renderTimeReport
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.RingBuffer as RB
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Definition.Color as Color

-- * UAttrLine

type UAttrLine = U.Vector Word32

uToAttrLine :: UAttrLine -> AttrLine
uToAttrLine v = map Color.AttrCharW32 $ U.toList v

attrLineToU :: AttrLine -> UAttrLine
attrLineToU l = U.fromList $ map Color.attrCharW32 l

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgLine  :: AttrLine  -- ^ the colours and characters of the message
  , msgClass :: MsgClass  -- ^ whether message should be displayed,
                          --   recorded in history, with what color, etc.
  }
  deriving (Show, Eq, Generic)

instance Binary Msg

toMsg :: Maybe (EM.EnumMap MsgClass Color.Color) -> MsgClass -> AttrLine -> Msg
toMsg mem msgClass l =
  let findColorInConfig = EM.findWithDefault Color.White msgClass
      color = maybe (msgColor msgClass) findColorInConfig mem
      msgLine = if color == Color.White then l else map (colorAttrChar color) l
  in Msg {..}

colorAttrChar :: Color.Color -> Color.AttrCharW32 -> Color.AttrCharW32
colorAttrChar color w =
  -- Only @White@ color gets replaced.
  -- For speed and simplicity we always keep the space @White@.
  let acChar = Color.charFromW32 w
  in if Color.fgFromW32 w == Color.White && acChar /= ' '
     then Color.attrChar2ToW32 color acChar
     else w

data MsgClass =
    MsgAdmin
  | MsgBecome
  | MsgNoLonger
  | MsgLonger
  | MsgItemCreation
  | MsgItemDestruction
  | MsgDeathGood
  | MsgDeathBad
  | MsgDeath
  | MsgDeathThreat
  | MsgLeader
  | MsgDiplomacy
  | MsgOutcome
  | MsgPlot
  | MsgLandscape
  | MsgTileDisco
  | MsgItemDisco
  | MsgActorSpot
  | MsgFirstEnemySpot
  | MsgItemSpot
  | MsgItemMove
  | MsgAction
  | MsgActionMinor
  | MsgEffectMajor
  | MsgEffect
  | MsgEffectMinor
  | MsgMisc
  | MsgHeardClose
  | MsgHeard
  | MsgFocus
  | MsgWarning
  | MsgRangedPowerfulGood
  | MsgRangedPowerfulBad
  | MsgRanged
  | MsgRare
  | MsgVeryRare
  | MsgMeleePowerfulGood
  | MsgMeleePowerfulBad
  | MsgMeleeInterestingGood
  | MsgMeleeInterestingBad
  | MsgMelee
  | MsgDone
  | MsgAtFeet
  | MsgNumeric
  | MsgSpam
  | MsgMacro
  | MsgRunStop
  | MsgPrompt
  | MsgPromptFocus
  | MsgAlert
 deriving (Show, Read, Eq, Enum, Generic)

instance NFData MsgClass

instance Binary MsgClass

isSavedToHistory :: MsgClass -> Bool
isSavedToHistory MsgNumeric = False
isSavedToHistory MsgSpam = False
isSavedToHistory MsgMacro = False
isSavedToHistory MsgRunStop = False
isSavedToHistory MsgPrompt = False
isSavedToHistory MsgPromptFocus = False
isSavedToHistory MsgAlert = False
isSavedToHistory _ = True

isDisplayed :: MsgClass -> Bool
isDisplayed MsgNumeric = False
isDisplayed MsgSpam = False
isDisplayed MsgMacro = False
isDisplayed MsgRunStop = False
isDisplayed _ = True

interruptsRunning :: MsgClass -> Bool
interruptsRunning MsgHeard = False
  -- MsgHeardClose interrupts, even if running started while hearing close
interruptsRunning MsgEffectMinor = False
interruptsRunning MsgActionMinor = False
interruptsRunning MsgNumeric = False
interruptsRunning MsgSpam = False
interruptsRunning MsgMacro = False
interruptsRunning MsgRunStop = False
interruptsRunning MsgPrompt = False
interruptsRunning MsgPromptFocus = False
  -- MsgAlert means something went wrong, so alarm
interruptsRunning _ = True

disturbsResting :: MsgClass -> Bool
disturbsResting MsgHeard = False
disturbsResting MsgHeardClose = False -- handled separately
disturbsResting MsgLeader = False -- handled separately
disturbsResting MsgEffectMinor = False
disturbsResting MsgActionMinor = False
disturbsResting MsgNumeric = False
disturbsResting MsgSpam = False
disturbsResting MsgMacro = False
disturbsResting MsgRunStop = False
disturbsResting MsgPrompt = False
disturbsResting MsgPromptFocus = False
  -- MsgAlert means something went wrong, so alarm
disturbsResting _ = True

-- Only @White@ color gets replaced by this one.
msgColor :: MsgClass -> Color.Color
msgColor MsgAdmin = Color.White
msgColor MsgBecome = Color.BrBlue  -- similar color to cyan and role to Effect
msgColor MsgNoLonger = Color.Blue
msgColor MsgLonger = Color.White  -- not important enough
msgColor MsgItemCreation = Color.BrBlue
msgColor MsgItemDestruction = Color.Blue
msgColor MsgDeathGood = Color.BrGreen
msgColor MsgDeathBad = Color.BrRed
msgColor MsgDeath = Color.White
msgColor MsgDeathThreat = Color.BrRed
msgColor MsgLeader = Color.White
msgColor MsgDiplomacy = Color.BrYellow
msgColor MsgOutcome = Color.BrWhite
msgColor MsgPlot = Color.White
msgColor MsgLandscape = Color.White
msgColor MsgTileDisco = Color.Magenta
msgColor MsgItemDisco = Color.BrMagenta
msgColor MsgActorSpot = Color.White  -- too common
msgColor MsgFirstEnemySpot = Color.Red
msgColor MsgItemSpot = Color.White
msgColor MsgItemMove = Color.White
msgColor MsgAction = Color.White
msgColor MsgActionMinor = Color.White
msgColor MsgEffectMajor = Color.BrCyan
msgColor MsgEffect = Color.Cyan
msgColor MsgEffectMinor = Color.White
msgColor MsgMisc = Color.White
msgColor MsgHeardClose = Color.BrYellow
msgColor MsgHeard = Color.Brown
msgColor MsgFocus = Color.Green
msgColor MsgWarning = Color.BrYellow
msgColor MsgRangedPowerfulGood = Color.Green
msgColor MsgRangedPowerfulBad = Color.Red
msgColor MsgRanged = Color.White
msgColor MsgRare = Color.Cyan
msgColor MsgVeryRare = Color.BrCyan
msgColor MsgMeleePowerfulGood = Color.Green
msgColor MsgMeleePowerfulBad = Color.Red
msgColor MsgMeleeInterestingGood = Color.Green
msgColor MsgMeleeInterestingBad = Color.Red
msgColor MsgMelee = Color.White
msgColor MsgDone = Color.White
msgColor MsgAtFeet = Color.White
msgColor MsgNumeric = Color.White
msgColor MsgSpam = Color.White
msgColor MsgMacro = Color.White
msgColor MsgRunStop = Color.White
msgColor MsgPrompt = Color.White
msgColor MsgPromptFocus = Color.Green
msgColor MsgAlert = Color.BrYellow

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

-- | Render a report as a (possibly very long) 'AttrLine'. Filter out
-- messages not meant for display.
renderReport :: Report -> AttrLine
renderReport (Report r) =
  let rep = Report $ filter (isDisplayed . msgClass . repMsg) r
  in renderWholeReport rep

-- | Render a report as a (possibly very long) 'AttrLine'.
renderWholeReport :: Report -> AttrLine
renderWholeReport (Report []) = []
renderWholeReport (Report (x : xs)) =
  renderWholeReport (Report xs) <+:> renderRepetition x

renderRepetition :: RepMsgN -> AttrLine
renderRepetition (RepMsgN s 0) = msgLine s
renderRepetition (RepMsgN s 1) = msgLine s
renderRepetition (RepMsgN s n) = msgLine s ++ stringToAL ("<x" ++ show n ++ ">")

anyInReport :: (MsgClass -> Bool) -> Report -> Bool
anyInReport f (Report xns) = any (f . msgClass . repMsg) xns

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
emptyHistory size =
  let ringBufferSize = size - 1  -- a report resides outside the buffer
  in History emptyReport timeZero emptyReport timeZero
             (RB.empty ringBufferSize U.empty)

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
addToReport :: History -> Msg -> Int -> Time -> (History, Bool)
addToReport History{..} msg n time =
  let newH = History{newReport = snocReport newReport msg n, newTime = time, ..}
  in case scrapRepetition newH of
    Just scrappedH -> (scrappedH, True)
    Nothing -> (newH, False)

-- | Archive old report to history, filtering out messages with 0 duplicates
-- and prompts. Set up new report with a new timestamp.
archiveReport :: History -> History
archiveReport History{newReport=Report newMsgs, ..} =
  let f (RepMsgN _ n) = n > 0
      newReportNon0 = Report $ filter f newMsgs
  in if nullReport newReportNon0
     then -- Drop empty new report.
          History emptyReport timeZero oldReport oldTime archivedHistory
     else let lU = map attrLineToU $ renderTimeReport oldTime oldReport
          in History emptyReport timeZero newReportNon0 newTime
             $ foldl' (\ !h !v -> RB.cons v h) archivedHistory (reverse lU)

renderTimeReport :: Time -> Report -> [AttrLine]
renderTimeReport !t (Report r) =
  let turns = t `timeFitUp` timeTurn
      rep = Report $ filter (isSavedToHistory . msgClass . repMsg) r
  in if nullReport rep
     then []
     else [stringToAL (show turns ++ ": ") ++ renderReport rep]

lengthHistory :: History -> Int
lengthHistory History{oldReport, archivedHistory} =
  RB.length archivedHistory
  + length (renderTimeReport timeZero oldReport)
      -- matches @renderHistory@

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: History -> [AttrLine]
renderHistory History{..} = map uToAttrLine (RB.toList archivedHistory)
                            ++ renderTimeReport oldTime oldReport
