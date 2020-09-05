{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, KindSignatures, StandaloneDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    MsgShowAndSave, MsgShow, MsgSave, MsgIgnore, MsgDifferent
  , MsgClass(..), MsgCreate, MsgSingle(..)
  , Msg(..), toMsg
    -- * Report
  , Report, nullReport, consReport, renderReport, anyInReport
    -- * History
  , History, newReport, emptyHistory, addToReport, addEolToNewReport
  , archiveReport, lengthHistory, renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , UAttrString, uToAttrString, attrStringToU
  , RepMsgNK, nullRepMsgNK
  , interruptsRunning, disturbsResting, bindsPronouns, msgColor
  , emptyReport, snocReport, renderWholeReport, renderRepetition
  , scrapRepetitionSingle, scrapRepetition, renderTimeReport
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Char as Char
import           Data.Kind (Type)
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

attrStringToU :: AttrString -> UAttrString
attrStringToU l = U.fromList $ map Color.attrCharW32 l

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgShow              :: AttrString
      -- ^ the colours and characters of the message
      --   to be shown on the screen; not just text,
      --   in case there was some colour not coming
      --   from the message class
  , msgSave              :: AttrString
      -- ^ the same to be saved in the message log only
  , msgInterruptsRunning :: Bool
  , msgDisturbsResting   :: Bool
  , msgBindsPronouns     :: Bool
  }
  deriving (Show, Generic)

instance Binary Msg

toMsg :: MsgCreate a => [(String, Color.Color)] -> MsgClass a -> a -> Msg
toMsg prefixColors msgClass a =
  let (tShow, tSave) = msgCreateConvert a
      mprefixColor = find ((`isPrefixOf` show msgClass) . fst) prefixColors
      color = maybe (msgColor msgClass) snd mprefixColor
      msgShow = textFgToAS color tShow
      msgSave = textFgToAS color tSave
      msgInterruptsRunning = interruptsRunning msgClass
      msgDisturbsResting = disturbsResting msgClass
      msgBindsPronouns = bindsPronouns msgClass
  in Msg {..}

class MsgCreate a where
  msgCreateConvert :: a -> (Text, Text)

instance MsgCreate MsgShowAndSave where
  msgCreateConvert t = (t, t)

instance MsgCreate MsgShow where
  msgCreateConvert (t, ()) = (t, "")

instance MsgCreate MsgSave where
  msgCreateConvert ((), t) = ("", t)

instance MsgCreate MsgIgnore where
  msgCreateConvert () = ("", "")

instance MsgCreate MsgDifferent where
  msgCreateConvert tt = tt

class MsgCreate a => MsgSingle a where
  msgSameInject :: Text -> a

instance MsgSingle MsgShowAndSave where
  msgSameInject t = t

instance MsgSingle MsgShow where
  msgSameInject t = (t, ())

instance MsgSingle MsgSave where
  msgSameInject t = ((), t)

instance MsgSingle MsgIgnore where
  msgSameInject _ = ()

type MsgShowAndSave = Text

type MsgShow = (Text, ())

type MsgSave = ((), Text)

type MsgIgnore = ()

type MsgDifferent = (Text, Text)

data MsgClass :: Type -> Type where
  MsgAdmin :: MsgClass MsgShowAndSave
  MsgBecomeSleep :: MsgClass MsgShowAndSave
  MsgBecomeBeneficialUs :: MsgClass MsgShowAndSave
  MsgBecomeHarmfulUs :: MsgClass MsgShowAndSave
  MsgBecome :: MsgClass MsgShowAndSave
  MsgNoLongerSleep :: MsgClass MsgShowAndSave
  MsgNoLongerUs :: MsgClass MsgShowAndSave
  MsgNoLonger :: MsgClass MsgShowAndSave
  MsgLongerUs :: MsgClass MsgShowAndSave
  MsgLonger :: MsgClass MsgShowAndSave
  MsgItemCreation :: MsgClass MsgShowAndSave
  MsgItemDestruction :: MsgClass MsgShowAndSave
  MsgDeathGood :: MsgClass MsgShowAndSave
  MsgDeathBad :: MsgClass MsgShowAndSave
  MsgDeathBoring :: MsgClass MsgShowAndSave
  MsgNearDeath :: MsgClass MsgShowAndSave
  MsgLeader :: MsgClass MsgShowAndSave
  MsgDiplomacy :: MsgClass MsgShowAndSave
  MsgOutcome :: MsgClass MsgShowAndSave
  MsgPlot :: MsgClass MsgShowAndSave
  MsgLandscape :: MsgClass MsgShowAndSave
  MsgDiscoTile :: MsgClass MsgShowAndSave
  MsgItemDisco :: MsgClass MsgShowAndSave
  MsgSpotActor :: MsgClass MsgShowAndSave
  MsgSpotThreat :: MsgClass MsgShowAndSave
  MsgItemMove :: MsgClass MsgShowAndSave
  MsgItemMoveDifferent :: MsgClass MsgDifferent
  MsgAction :: MsgClass MsgShowAndSave
  MsgActionMinor :: MsgClass MsgShowAndSave
  MsgEffectMajor :: MsgClass MsgShowAndSave
  MsgEffect :: MsgClass MsgShowAndSave
  MsgEffectMinor :: MsgClass MsgShowAndSave
  MsgMisc :: MsgClass MsgShowAndSave
  MsgHeardElsewhere :: MsgClass MsgShowAndSave
  MsgHeardClose :: MsgClass MsgShowAndSave
  MsgHeard :: MsgClass MsgShowAndSave
  MsgFocus :: MsgClass MsgShowAndSave
  MsgWarning :: MsgClass MsgShowAndSave
  MsgRangedPowerfulWe :: MsgClass MsgShowAndSave
  MsgRangedPowerfulUs :: MsgClass MsgShowAndSave
  MsgRanged :: MsgClass MsgShowAndSave  -- not ours or projectiles are hit
  MsgRangedUs :: MsgClass MsgShowAndSave
  MsgNeutralEvent :: MsgClass MsgShowAndSave
  MsgNeutralEventRare :: MsgClass MsgShowAndSave
  MsgMeleePowerfulWe :: MsgClass MsgShowAndSave
  MsgMeleePowerfulUs :: MsgClass MsgShowAndSave
  MsgMeleeInterestingWe :: MsgClass MsgShowAndSave
  MsgMeleeInterestingUs :: MsgClass MsgShowAndSave
  MsgMelee :: MsgClass MsgShowAndSave  -- not ours or projectiles are hit
  MsgMeleeUs :: MsgClass MsgShowAndSave
  MsgDone :: MsgClass MsgShowAndSave
  MsgAtFeetMajor :: MsgClass MsgShowAndSave
  MsgAtFeet :: MsgClass MsgShowAndSave
  MsgNumeric :: MsgClass MsgSave
  MsgSpam :: MsgClass MsgIgnore
  MsgMacro :: MsgClass MsgIgnore
  MsgRunStop :: MsgClass MsgIgnore
  MsgPrompt :: MsgClass MsgShow
  MsgPromptFocus :: MsgClass MsgShow
  MsgPromptMention :: MsgClass MsgShow
  MsgPromptWarning :: MsgClass MsgShow
  MsgPromptThreat :: MsgClass MsgShow
  MsgPromptItem :: MsgClass MsgShow
  MsgAlert :: MsgClass MsgShow
  MsgStopPlayback :: MsgClass MsgIgnore

deriving instance Show (MsgClass a)

interruptsRunning :: MsgClass a -> Bool
interruptsRunning MsgAdmin = False
interruptsRunning MsgBecome = False
interruptsRunning MsgNoLonger = False
interruptsRunning MsgLonger = False
interruptsRunning MsgItemDisco = False
interruptsRunning MsgItemMove = False
interruptsRunning MsgItemMoveDifferent = False
interruptsRunning MsgActionMinor = False
interruptsRunning MsgEffectMinor = False
interruptsRunning MsgHeard = False
  -- MsgHeardClose interrupts, even if running started while hearing close
interruptsRunning MsgRanged = False
interruptsRunning MsgAtFeet = False
interruptsRunning MsgNumeric = False
interruptsRunning MsgSpam = False
interruptsRunning MsgMacro = False
interruptsRunning MsgRunStop = False
interruptsRunning MsgPrompt = False
interruptsRunning MsgPromptFocus = False
interruptsRunning MsgPromptMention = False
interruptsRunning MsgPromptWarning = False
interruptsRunning MsgPromptThreat = False
interruptsRunning MsgPromptItem = False
  -- MsgAlert means something went wrong, so alarm
interruptsRunning _ = True

disturbsResting :: MsgClass a -> Bool
disturbsResting MsgAdmin = False
disturbsResting MsgBecome = False
disturbsResting MsgNoLonger = False
disturbsResting MsgLonger = False
disturbsResting MsgLeader = False -- handled separately
disturbsResting MsgItemDisco = False
disturbsResting MsgItemMove = False
disturbsResting MsgItemMoveDifferent = False
disturbsResting MsgActionMinor = False
disturbsResting MsgEffectMinor = False
disturbsResting MsgHeardElsewhere = False
disturbsResting MsgHeardClose = False -- handled separately
disturbsResting MsgHeard = False
disturbsResting MsgRanged = False
disturbsResting MsgAtFeet = False
disturbsResting MsgNumeric = False
disturbsResting MsgSpam = False
disturbsResting MsgMacro = False
disturbsResting MsgRunStop = False
disturbsResting MsgPrompt = False
disturbsResting MsgPromptFocus = False
disturbsResting MsgPromptMention = False
disturbsResting MsgPromptWarning = False
disturbsResting MsgPromptThreat = False
disturbsResting MsgPromptItem = False
  -- MsgAlert means something went wrong, so alarm
disturbsResting _ = True

-- Only player's non-projectile actors getting hit introduce subjects,
-- because only such hits are guaranteed to be perceived.
-- Here we also mark friends being hit, but that's a safe approximation.
-- We also mark the messages that use the introduced subjects
-- by referring to them via pronouns. They can't be moved freely either.
bindsPronouns :: MsgClass a -> Bool
bindsPronouns MsgLongerUs = True
bindsPronouns MsgRangedPowerfulUs = True
bindsPronouns MsgRangedUs = True
bindsPronouns MsgMeleePowerfulUs = True
bindsPronouns MsgMeleeInterestingUs = True
bindsPronouns MsgMeleeUs = True
bindsPronouns _ = False

-- Only initially @White@ colour in text (e.g., not highlighted @BrWhite@)
-- gets replaced by the one indicated.
--
-- See the discussion of colours and the table of colours at
-- https://github.com/LambdaHack/LambdaHack/wiki/Display#colours
-- Another mention of colours, concerning terrain, is in PLAYING.md manual.
-- The manual and this code should follow the wiki.
cVeryBadEvent, cBadEvent, cRisk, cGraveRisk, cVeryGoodEvent, cGoodEvent, cVista, cSleep, cWakeUp, cGreed, cNeutralEvent, cRareNeutralEvent, cIdentification, cMeta, cBoring, cGameOver :: Color.Color
cVeryBadEvent = Color.Red
cBadEvent = Color.BrRed
cRisk = Color.Magenta
cGraveRisk = Color.BrMagenta
cVeryGoodEvent = Color.Green
cGoodEvent = Color.BrGreen
cVista = Color.BrGreen
cSleep = Color.Blue
cWakeUp = Color.BrBlue
cGreed = Color.BrBlue
cNeutralEvent = Color.Cyan
cRareNeutralEvent = Color.BrCyan
cIdentification = Color.Brown
cMeta = Color.BrYellow
cBoring = Color.White
cGameOver = Color.BrWhite

msgColor :: MsgClass a -> Color.Color
msgColor MsgAdmin = cBoring
msgColor MsgBecomeSleep = cSleep
msgColor MsgBecomeBeneficialUs = cGoodEvent
msgColor MsgBecomeHarmfulUs = cBadEvent
msgColor MsgBecome = cBoring
msgColor MsgNoLongerSleep = cWakeUp
msgColor MsgNoLongerUs = cBoring
msgColor MsgNoLonger = cBoring
msgColor MsgLongerUs = cBoring  -- not important enough
msgColor MsgLonger = cBoring  -- not important enough, no disturb even
msgColor MsgItemCreation = cGreed
msgColor MsgItemDestruction = cBoring  -- common, colourful components created
msgColor MsgDeathGood = cVeryGoodEvent
msgColor MsgDeathBad = cVeryBadEvent
msgColor MsgDeathBoring = cBoring
msgColor MsgNearDeath = cGraveRisk
msgColor MsgLeader = cBoring
msgColor MsgDiplomacy = cMeta  -- good or bad
msgColor MsgOutcome = cGameOver
msgColor MsgPlot = cBoring
msgColor MsgLandscape = cBoring
msgColor MsgDiscoTile = cIdentification
msgColor MsgItemDisco = cIdentification
msgColor MsgSpotActor = cBoring  -- too common; warning in @MsgSpotThreat@
msgColor MsgSpotThreat = cGraveRisk
msgColor MsgItemMove = cBoring
msgColor MsgItemMoveDifferent = cBoring
msgColor MsgAction = cBoring
msgColor MsgActionMinor = cBoring
msgColor MsgEffectMajor = cRareNeutralEvent
msgColor MsgEffect = cNeutralEvent
msgColor MsgEffectMinor = cBoring
msgColor MsgMisc = cBoring
msgColor MsgHeardElsewhere = cBoring
msgColor MsgHeardClose = cGraveRisk
msgColor MsgHeard = cRisk
msgColor MsgFocus = cVista
msgColor MsgWarning = cMeta
msgColor MsgRangedPowerfulWe = cGoodEvent
msgColor MsgRangedPowerfulUs = cBadEvent
msgColor MsgRanged = cBoring
msgColor MsgRangedUs = cRisk
msgColor MsgNeutralEvent = cNeutralEvent
msgColor MsgNeutralEventRare = cRareNeutralEvent
msgColor MsgMeleePowerfulWe = cGoodEvent
msgColor MsgMeleePowerfulUs = cBadEvent
msgColor MsgMeleeInterestingWe = cGoodEvent
msgColor MsgMeleeInterestingUs = cBadEvent
msgColor MsgMelee = cBoring
msgColor MsgMeleeUs = cRisk
msgColor MsgDone = cBoring
msgColor MsgAtFeetMajor = cBoring
msgColor MsgAtFeet = cBoring
msgColor MsgNumeric = cBoring
msgColor MsgSpam = cBoring
msgColor MsgMacro = cBoring
msgColor MsgRunStop = cBoring
msgColor MsgPrompt = cBoring
msgColor MsgPromptFocus = cVista
msgColor MsgPromptMention = cNeutralEvent
msgColor MsgPromptWarning = cMeta
msgColor MsgPromptThreat = cRisk
msgColor MsgPromptItem = cGreed
msgColor MsgAlert = cMeta
msgColor MsgStopPlayback = cMeta

-- * Report

data RepMsgNK = RepMsgNK {repMsg :: Msg, _repShow :: Int, _repSave :: Int}
  deriving (Show, Generic)

instance Binary RepMsgNK

-- | If only one of the message components is non-empty and non-whitespace,
-- but its count is zero, the message is considered empty.
nullRepMsgNK :: RepMsgNK -> Bool
nullRepMsgNK (RepMsgNK Msg{..} n k) =
  (all (Char.isSpace . Color.charFromW32) msgShow || n <= 0)
  && (all (Char.isSpace . Color.charFromW32) msgSave || k <= 0)

-- | The set of messages, with repetitions, to show at the screen at once.
newtype Report = Report [RepMsgNK]
  deriving (Show, Binary)

-- | Empty set of messages.
emptyReport :: Report
emptyReport = Report []

-- | Test if the list of messages is empty.
nullReport :: Report -> Bool
nullReport (Report l) = null l

-- | Add a message to the end of the report with the given repetition.
snocReport :: Report -> Msg -> Int -> Report
snocReport (Report r) msg n = Report $ RepMsgNK msg n n : r

-- | Add a message to the start of report.
consReport :: Msg -> Report -> Report
consReport msg (Report r) = Report $ r ++ [RepMsgNK msg 1 1]

-- | Render a report as a (possibly very long) 'AttrString'. Filter out
-- messages not meant for display, unless not showing, but saving to history.
renderReport :: Bool -> Report -> AttrString
renderReport displaying (Report r) =
  let rep = map (\(RepMsgNK msg n k) -> if displaying
                                        then (msgShow msg, n)
                                        else (msgSave msg, k)) r
  in renderWholeReport rep

-- | Render a report as a (possibly very long) 'AttrString'.
renderWholeReport :: [(AttrString, Int)] -> AttrString
renderWholeReport [] = []
renderWholeReport (x : xs) =
  renderWholeReport xs <+:> renderRepetition x

renderRepetition :: (AttrString, Int) -> AttrString
renderRepetition (as, n) =
  if n <= 1 || all (Char.isSpace . Color.charFromW32) as
  then as
  else as ++ stringToAS ("<x" ++ show n ++ ">")

anyInReport :: (Msg -> Bool) -> Report -> Bool
anyInReport f (Report xns) = any (f . repMsg) xns

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

scrapRepetitionSingle :: ((AttrString, Int), Bool)
                      -> [((AttrString, Int), Bool)] -> [(AttrString, Int)]
                      -> (Bool, [(AttrString, Int)], [(AttrString, Int)])
scrapRepetitionSingle ((s1, n1), commutative1) rest1 oldMsgs =
  let butLastEOLs = dropWhileEnd ((== '\n') . Color.charFromW32)
      eqs1 (s2, _) = butLastEOLs s1 == butLastEOLs s2
      noChange = (False, (s1, n1) : map fst rest1, oldMsgs)
  in case break (eqs1 . fst) rest1 of
    (_, []) | commutative1 -> case break eqs1 oldMsgs of
      (noDup, (_, n2) : rest2) ->
        -- We keep the occurence of the message in the new report only.
        let newReport = (s1, n1 + n2) : map fst rest1
            oldReport = noDup ++ ([], 0) : rest2
        in (True, newReport, oldReport)
      _ -> noChange
    (noDup, ((s2, n2), _) : rest3) | commutative1 || all snd noDup ->
      -- We keep the older (and so, oldest) occurence of the message,
      -- to avoid visual disruption by moving the message around.
      let newReport = ([], 0) : map fst noDup ++ (s2, n1 + n2) : map fst rest3
          oldReport = oldMsgs
      in (True, newReport, oldReport)
    _ -> noChange

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
    RepMsgNK msg1 n1 k1 : rest1 ->
      let commutative = not . msgBindsPronouns
          commutative1 = commutative msg1
          makeShow = map (\(RepMsgNK msg n _) -> (msgShow msg, n))
          makeShowC = map (\(RepMsgNK msg n _) -> ( (msgShow msg, n)
                                                  , commutative msg ))
          makeSave = map (\(RepMsgNK msg _ k) -> (msgSave msg, k))
          makeSaveC = map (\(RepMsgNK msg _ k) -> ( (msgSave msg, k)
                                                  , commutative msg ))
          (scrapShowNeeded, scrapShowNew, scrapShowOld) =
            scrapRepetitionSingle ((msgShow msg1, n1), commutative1)
                                  (makeShowC rest1)
                                  (makeShow oldMsgs)
          (scrapSaveNeeded, scrapSaveNew, scrapSaveOld) =
            scrapRepetitionSingle ((msgSave msg1, k1), commutative1)
                                  (makeSaveC rest1)
                                  (makeSave oldMsgs)
      in if scrapShowNeeded || scrapSaveNeeded
         then let combineMsg msg (s, n) (t, k) =
                    RepMsgNK msg{msgShow = s, msgSave = t} n k
                  zipMsg l1 l2 l3 =
                    Report $ zipWith3 combineMsg (map repMsg l1) l2 l3
                  newReport = zipMsg newMsgs scrapShowNew scrapSaveNew
                  oldReport = zipMsg oldMsgs scrapShowOld scrapSaveOld
              in Just History{..}
         else Nothing
    _ -> Nothing  -- empty new report

-- | Add a message to the new report of history, eliminating a possible
-- duplicate and noting its existence in the result.
addToReport :: History -> Msg -> Int -> Time -> (History, Bool)
addToReport History{..} msg n time =
  let newH = History { newReport = snocReport newReport msg n
                     , newTime = time
                     , .. }
  in case scrapRepetition newH of
    Just scrappedH -> (scrappedH, True)
    Nothing -> (newH, False)

-- | Add a newline to end of the new report of history, unless empty.
addEolToNewReport :: History -> History
addEolToNewReport hist =
  let addEolToReport (Report []) = Report []
      addEolToReport (Report (hd : tl)) = Report $ addEolToRepMsgNK hd : tl
      addEolToRepMsgNK rm = rm {repMsg = addEolToMsg $ repMsg rm}
      addEolToMsg msg = msg { msgShow = addEolToAS $ msgShow msg
                            , msgSave = addEolToAS $ msgSave msg }
      addEolToAS as = as ++ stringToAS "\n"
  in hist {newReport = addEolToReport $ newReport hist}

-- | Archive old report to history, filtering out messages with 0 duplicates
-- and prompts. Set up new report with a new timestamp.
archiveReport :: History -> History
archiveReport History{newReport=Report newMsgs, ..} =
  let newReportFiltered = Report $ filter (not . nullRepMsgNK) newMsgs
  in if nullReport newReportFiltered
     then -- Drop empty new report.
          History emptyReport timeZero oldReport oldTime archivedHistory
     else let lU = map attrStringToU $ renderTimeReport oldTime oldReport
          in History emptyReport timeZero newReportFiltered newTime
             $ foldl' (\ !h !v -> RB.cons v h) archivedHistory (reverse lU)

renderTimeReport :: Time -> Report -> [AttrString]
renderTimeReport t rep =
  let turns = t `timeFitUp` timeTurn
      as = renderReport False rep
  in [ stringToAS (show turns ++ ": ") ++ as
     | not $ all (Char.isSpace . Color.charFromW32) as ]

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
