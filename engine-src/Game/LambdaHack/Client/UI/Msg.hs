{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, toMsg
  , MsgClass(..), interruptsRunning, disturbsResting
    -- * Report
  , Report, nullReport, consReport, addEolToNewReport
  , renderReport, anyInReport
    -- * History
  , History, newReport, emptyHistory, addToReport, archiveReport, lengthHistory
  , renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , isSavedToHistory, isDisplayed, bindsPronouns, msgColor
  , UAttrString, RepMsgN, uToAttrString, attrLineToU
  , emptyReport, nullFilteredReport, snocReport
  , renderWholeReport, renderRepetition
  , scrapRepetition, renderTimeReport
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U
import           GHC.Generics (Generic)

import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.RingBuffer as RB
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Definition.Color as Color

-- * UAttrString

type UAttrString = U.Vector Word32

uToAttrString :: UAttrString -> AttrString
uToAttrString v = map Color.AttrCharW32 $ U.toList v

attrLineToU :: AttrString -> UAttrString
attrLineToU l = U.fromList $ map Color.attrCharW32 l

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgLine  :: AttrString  -- ^ the colours and characters of the message;
                            --   not just text, in case there was some colour
                            --   unrelated to msg class
  , msgClass :: MsgClass    -- ^ whether message should be displayed,
                            --   recorded in history, with what color, etc.
  }
  deriving (Show, Eq, Generic)

instance Binary Msg

toMsg :: Maybe (EM.EnumMap MsgClass Color.Color) -> MsgClass -> Text -> Msg
toMsg mem msgClass l =
  let findColorInConfig = EM.findWithDefault Color.White msgClass
      color = maybe (msgColor msgClass) findColorInConfig mem
      msgLine = textFgToAS color l
  in Msg {..}

data MsgClass =
    MsgAdmin
  | MsgBecome
  | MsgNoLonger
  | MsgLongerUs
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
  | MsgRangedPowerfulWe
  | MsgRangedPowerfulUs
  | MsgRanged  -- our non-projectile actors are not hit
  | MsgRangedUs
  | MsgRare
  | MsgVeryRare
  | MsgMeleePowerfulWe
  | MsgMeleePowerfulUs
  | MsgMeleeInterestingWe
  | MsgMeleeInterestingUs
  | MsgMelee  -- our non-projectile actors are not hit
  | MsgMeleeUs
  | MsgDone
  | MsgAtFeetMajor
  | MsgAtFeet
  | MsgNumeric
  | MsgSpam
  | MsgMacro
  | MsgRunStop
  | MsgPrompt
  | MsgPromptFocus
  | MsgAlert
  | MsgStopPlayback
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
isSavedToHistory MsgStopPlayback = False
isSavedToHistory _ = True

isDisplayed :: MsgClass -> Bool
isDisplayed MsgRunStop = False
isDisplayed MsgNumeric = False
isDisplayed MsgSpam = False
isDisplayed MsgMacro = False
isDisplayed MsgStopPlayback = False
isDisplayed _ = True

interruptsRunning :: MsgClass -> Bool
interruptsRunning MsgHeard = False
  -- MsgHeardClose interrupts, even if running started while hearing close
interruptsRunning MsgEffectMinor = False
interruptsRunning MsgItemDisco = False
interruptsRunning MsgItemMove = False
interruptsRunning MsgActionMinor = False
interruptsRunning MsgAtFeet = False
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
disturbsResting MsgItemDisco = False
disturbsResting MsgItemMove = False
disturbsResting MsgActionMinor = False
disturbsResting MsgAtFeet = False
disturbsResting MsgNumeric = False
disturbsResting MsgSpam = False
disturbsResting MsgMacro = False
disturbsResting MsgRunStop = False
disturbsResting MsgPrompt = False
disturbsResting MsgPromptFocus = False
  -- MsgAlert means something went wrong, so alarm
disturbsResting _ = True

-- Only player's non-projectile actors getting hit introduce subjects,
-- because only such hits are guaranteed to be perceived.
-- Here we also mark friends being hit, but that's a safe approximation.
-- We also mark the messages that use the introduced subjects
-- by referring to them via pronouns. They can't be moved freely either.
bindsPronouns :: MsgClass -> Bool
bindsPronouns MsgRangedPowerfulUs = True
bindsPronouns MsgRangedUs = True
bindsPronouns MsgMeleePowerfulUs = True
bindsPronouns MsgMeleeInterestingUs = True
bindsPronouns MsgMeleeUs = True
bindsPronouns MsgLongerUs = True
bindsPronouns _ = False

-- Only @White@ color gets replaced by this one.
msgColor :: MsgClass -> Color.Color
msgColor MsgAdmin = Color.White
msgColor MsgBecome = Color.BrBlue  -- similar color to cyan and role to Effect
msgColor MsgNoLonger = Color.Blue
msgColor MsgLongerUs = Color.White  -- not important enough
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
msgColor MsgRangedPowerfulWe = Color.Green
msgColor MsgRangedPowerfulUs = Color.Red
msgColor MsgRanged = Color.White
msgColor MsgRangedUs = Color.White
msgColor MsgRare = Color.Cyan
msgColor MsgVeryRare = Color.BrCyan
msgColor MsgMeleePowerfulWe = Color.Green
msgColor MsgMeleePowerfulUs = Color.Red
msgColor MsgMeleeInterestingWe = Color.Green
msgColor MsgMeleeInterestingUs = Color.Red
msgColor MsgMelee = Color.White
msgColor MsgMeleeUs = Color.White
msgColor MsgDone = Color.White
msgColor MsgAtFeetMajor = Color.White
msgColor MsgAtFeet = Color.White
msgColor MsgNumeric = Color.White
msgColor MsgSpam = Color.White
msgColor MsgMacro = Color.White
msgColor MsgRunStop = Color.White
msgColor MsgPrompt = Color.White
msgColor MsgPromptFocus = Color.Green
msgColor MsgAlert = Color.BrYellow
msgColor MsgStopPlayback = Color.BrYellow

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

nullFilteredReport :: Report -> Bool
nullFilteredReport (Report l) =
  null $ filter (\(RepMsgN msg n) -> n > 0 && isDisplayed (msgClass msg)) l

-- | Add a message to the end of the report.
snocReport :: Report -> Msg -> Int -> Report
snocReport (Report !r) y n =
  if null $ msgLine y then Report r else Report $ RepMsgN y n : r

-- | Add a message to the start of report.
consReport :: Msg -> Report -> Report
consReport Msg{msgLine=[]} rep = rep
consReport y (Report r) = Report $ r ++ [RepMsgN y 1]

-- | Render a report as a (possibly very long) 'AttrString'. Filter out
-- messages not meant for display.
renderReport :: Report -> AttrString
renderReport (Report r) =
  let rep = Report $ filter (isDisplayed . msgClass . repMsg) r
  in renderWholeReport rep

-- | Render a report as a (possibly very long) 'AttrString'.
renderWholeReport :: Report -> AttrString
renderWholeReport (Report []) = []
renderWholeReport (Report (x : xs)) =
  renderWholeReport (Report xs) <+:> renderRepetition x

renderRepetition :: RepMsgN -> AttrString
renderRepetition (RepMsgN s 0) = msgLine s
renderRepetition (RepMsgN s 1) = msgLine s
renderRepetition (RepMsgN s n) = msgLine s ++ stringToAS ("<x" ++ show n ++ ">")

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
  , archivedHistory :: RB.RingBuffer UAttrString }
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
    -- along many reports.
    RepMsgN s1 n1 : rest1 ->
      let commutative s = not $ bindsPronouns $ msgClass s
          butLastEOL [] = []
          butLastEOL s = if last s == Color.attrChar1ToW32 '\n' then init s else s
          f (RepMsgN s2 _) = butLastEOL (msgLine s1) == butLastEOL (msgLine s2)
      in case break f rest1 of
        (_, []) | commutative s1 -> case break f oldMsgs of
          (noDup, RepMsgN s2 n2 : rest2) ->
            -- We keep the occurence of the message in the new report only.
            let newReport = Report $ RepMsgN s2 (n1 + n2) : rest1
                oldReport = Report $ noDup ++ rest2
            in Just History{..}
          _ -> Nothing
        (noDup, RepMsgN s2 n2 : rest2) | commutative s1
                                         || all (commutative . repMsg) noDup ->
          -- We keep the older (and so, oldest) occurence of the message,
          -- to avoid visual disruption by moving the message around.
          let newReport = Report $ noDup ++ RepMsgN s2 (n1 + n2) : rest2
              oldReport = Report oldMsgs
          in Just History{..}
        _ -> Nothing
    _ -> Nothing  -- empty new report

-- | Add a message to the new report of history, eliminating a possible
-- duplicate and noting its existence in the result.
addToReport :: History -> Msg -> Int -> Time -> (History, Bool)
addToReport History{..} msg n time =
  let newH = History{newReport = snocReport newReport msg n, newTime = time, ..}
  in case scrapRepetition newH of
    Just scrappedH -> (scrappedH, True)
    Nothing -> (newH, False)

-- | Add a newline to end of the new report of history, unless empty.
addEolToNewReport :: History -> History
addEolToNewReport hist =
  if nullFilteredReport $ newReport hist
  then hist
  else let addEolToReport (Report []) = error "addEolToReport: empty report"
           addEolToReport (Report (hd : tl)) = Report $ addEolToRepMsgN hd : tl
           addEolToRepMsgN rm = rm {repMsg = addEolToMsg $ repMsg rm}
           addEolToMsg msg = msg {msgLine = msgLine msg ++ stringToAS "\n"}
       in hist {newReport = addEolToReport $ newReport hist}

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

renderTimeReport :: Time -> Report -> [AttrString]
renderTimeReport !t (Report r) =
  let turns = t `timeFitUp` timeTurn
      rep = Report $ filter (isSavedToHistory . msgClass . repMsg) r
  in if nullReport rep
     then []
     else [stringToAS (show turns ++ ": ") ++ renderReport rep]

lengthHistory :: History -> Int
lengthHistory History{oldReport, archivedHistory} =
  RB.length archivedHistory
  + length (renderTimeReport timeZero oldReport)
      -- matches @renderHistory@

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: History -> [AttrString]
renderHistory History{..} = renderTimeReport oldTime oldReport
                            ++ map uToAttrString (RB.toList archivedHistory)
