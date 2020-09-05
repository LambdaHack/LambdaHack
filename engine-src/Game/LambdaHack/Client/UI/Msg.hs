{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, KindSignatures, StandaloneDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    MsgShowAndSave, MsgShow, MsgSave, MsgIgnore, MsgDifferent
  , MsgClass(..), MsgCreate, MsgSingle(..)
  , Msg(msgInterruptsRunning, msgDisturbsResting), toMsg
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
  , msgClassName         :: String
  , msgInterruptsRunning :: Bool
  , msgDisturbsResting   :: Bool
  , msgBindsPronouns     :: Bool
  }
  deriving (Show, Generic)

instance Binary Msg

nullMsg :: Msg -> Bool
nullMsg Msg{..} = null msgShow && null msgSave

toMsg :: MsgCreate a => [(String, Color.Color)] -> MsgClass a -> a -> Msg
toMsg prefixColors msgClass a =
  let (tShow, tSave) = msgCreateConvert a
      mprefixColor = find ((`isPrefixOf` msgClassName) . fst) prefixColors
      color = maybe (msgColor msgClass) snd mprefixColor
      msgShow = textFgToAS color tShow
      msgSave = textFgToAS color tSave
      msgClassName = show msgClass
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

-- Each constructor should have length between 14 and 17.
data MsgClass :: Type -> Type where
  MsgBookkeeping :: MsgClass MsgShowAndSave
  MsgStatusSleep :: MsgClass MsgShowAndSave
  MsgStatusGoodUs :: MsgClass MsgShowAndSave
  MsgStatusBadUs :: MsgClass MsgShowAndSave
  MsgStatusOthers :: MsgClass MsgShowAndSave
  MsgStatusWakeup :: MsgClass MsgShowAndSave
  MsgStatusStopUs :: MsgClass MsgShowAndSave
  MsgStatusStopThem :: MsgClass MsgShowAndSave
  MsgStatusLongerUs :: MsgClass MsgShowAndSave
  MsgStatusLongThem :: MsgClass MsgShowAndSave
  MsgItemCreation :: MsgClass MsgShowAndSave
  MsgItemRuination :: MsgClass MsgShowAndSave
  MsgDeathVictory :: MsgClass MsgShowAndSave
  MsgDeathDeafeat :: MsgClass MsgShowAndSave
  MsgDeathBoring :: MsgClass MsgShowAndSave
  MsgRiskOfDeath :: MsgClass MsgShowAndSave
  MsgPointmanSwap :: MsgClass MsgShowAndSave
  MsgFactionIntel :: MsgClass MsgShowAndSave
  MsgFinalOutcome :: MsgClass MsgShowAndSave
  MsgPlotExposition :: MsgClass MsgShowAndSave
  MsgBackdropInfo :: MsgClass MsgShowAndSave
  MsgTerrainReveal :: MsgClass MsgShowAndSave
  MsgItemDiscovery :: MsgClass MsgShowAndSave
  MsgSpottedActor :: MsgClass MsgShowAndSave
  MsgSpottedThreat :: MsgClass MsgShowAndSave
  MsgSpottedItem :: MsgClass MsgDifferent
  MsgItemMovement :: MsgClass MsgShowAndSave
  MsgActionMajor :: MsgClass MsgShowAndSave
  MsgActionMinor :: MsgClass MsgShowAndSave
  MsgEffectMajor :: MsgClass MsgShowAndSave
  MsgEffectMedium :: MsgClass MsgShowAndSave
  MsgEffectMinor :: MsgClass MsgShowAndSave
  MsgMiscellanous :: MsgClass MsgShowAndSave
  MsgHeardOutside :: MsgClass MsgShowAndSave
  MsgHeardNearby :: MsgClass MsgShowAndSave
  MsgHeardFaraway :: MsgClass MsgShowAndSave
  MsgBackdropFocus :: MsgClass MsgShowAndSave
  MsgActionWarning :: MsgClass MsgShowAndSave
  MsgRangedMightyWe :: MsgClass MsgShowAndSave
  MsgRangedMightyUs :: MsgClass MsgShowAndSave
  MsgRangedOthers :: MsgClass MsgShowAndSave  -- not ours or projectiles are hit
  MsgRangedNormalUs :: MsgClass MsgShowAndSave
  MsgNeutralEvent :: MsgClass MsgShowAndSave
  MsgSpecialEvent :: MsgClass MsgShowAndSave
  MsgMeleeMightyWe :: MsgClass MsgShowAndSave
  MsgMeleeMightyUs :: MsgClass MsgShowAndSave
  MsgMeleeComplexWe :: MsgClass MsgShowAndSave
  MsgMeleeComplexUs :: MsgClass MsgShowAndSave
  MsgMeleeOthers :: MsgClass MsgShowAndSave  -- not ours or projectiles are hit
  MsgMeleeNormalUs :: MsgClass MsgShowAndSave
  MsgActionComplete :: MsgClass MsgShowAndSave
  MsgAtFeetMajor :: MsgClass MsgShowAndSave
  MsgAtFeetMinor :: MsgClass MsgShowAndSave
  MsgNumericReport :: MsgClass MsgSave
  MsgInnerWorkSpam :: MsgClass MsgIgnore
  MsgMacroOperation :: MsgClass MsgIgnore
  MsgRunStopReason :: MsgClass MsgIgnore
  MsgPromptNearby :: MsgClass MsgShow
  MsgPromptFocus :: MsgClass MsgShow
  MsgPromptMention :: MsgClass MsgShow
  MsgPromptWarning :: MsgClass MsgShow
  MsgPromptThreat :: MsgClass MsgShow
  MsgPromptItems :: MsgClass MsgShow
  MsgActionAlert :: MsgClass MsgShow
  MsgStopPlayback :: MsgClass MsgIgnore

deriving instance Show (MsgClass a)

interruptsRunning :: MsgClass a -> Bool
interruptsRunning MsgBookkeeping = False
interruptsRunning MsgStatusOthers = False
interruptsRunning MsgStatusStopThem = False
interruptsRunning MsgStatusLongThem = False
interruptsRunning MsgItemDiscovery = False
interruptsRunning MsgSpottedItem = False
interruptsRunning MsgItemMovement = False
interruptsRunning MsgActionMinor = False
interruptsRunning MsgEffectMinor = False
interruptsRunning MsgHeardFaraway = False
  -- MsgHeardNearby interrupts, even if running started while hearing close
interruptsRunning MsgRangedOthers = False
interruptsRunning MsgAtFeetMinor = False
interruptsRunning MsgNumericReport = False
interruptsRunning MsgInnerWorkSpam = False
interruptsRunning MsgMacroOperation = False
interruptsRunning MsgRunStopReason = False
interruptsRunning MsgPromptNearby = False
interruptsRunning MsgPromptFocus = False
interruptsRunning MsgPromptMention = False
interruptsRunning MsgPromptWarning = False
interruptsRunning MsgPromptThreat = False
interruptsRunning MsgPromptItems = False
  -- MsgActionAlert means something went wrong, so alarm
interruptsRunning _ = True

disturbsResting :: MsgClass a -> Bool
disturbsResting MsgBookkeeping = False
disturbsResting MsgStatusOthers = False
disturbsResting MsgStatusStopThem = False
disturbsResting MsgStatusLongThem = False
disturbsResting MsgPointmanSwap = False -- handled separately
disturbsResting MsgItemDiscovery = False
disturbsResting MsgSpottedItem = False
disturbsResting MsgItemMovement = False
disturbsResting MsgActionMinor = False
disturbsResting MsgEffectMinor = False
disturbsResting MsgHeardOutside = False
disturbsResting MsgHeardNearby = False -- handled separately
disturbsResting MsgHeardFaraway = False
disturbsResting MsgRangedOthers = False
disturbsResting MsgAtFeetMinor = False
disturbsResting MsgNumericReport = False
disturbsResting MsgInnerWorkSpam = False
disturbsResting MsgMacroOperation = False
disturbsResting MsgRunStopReason = False
disturbsResting MsgPromptNearby = False
disturbsResting MsgPromptFocus = False
disturbsResting MsgPromptMention = False
disturbsResting MsgPromptWarning = False
disturbsResting MsgPromptThreat = False
disturbsResting MsgPromptItems = False
  -- MsgActionAlert means something went wrong, so alarm
disturbsResting _ = True

-- Only player's non-projectile actors getting hit introduce subjects,
-- because only such hits are guaranteed to be perceived.
-- Here we also mark friends being hit, but that's a safe approximation.
-- We also mark the messages that use the introduced subjects
-- by referring to them via pronouns. They can't be moved freely either.
bindsPronouns :: MsgClass a -> Bool
bindsPronouns MsgStatusLongerUs = True
bindsPronouns MsgRangedMightyUs = True
bindsPronouns MsgRangedNormalUs = True
bindsPronouns MsgMeleeMightyUs = True
bindsPronouns MsgMeleeComplexUs = True
bindsPronouns MsgMeleeNormalUs = True
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
msgColor MsgBookkeeping = cBoring
msgColor MsgStatusSleep = cSleep
msgColor MsgStatusGoodUs = cGoodEvent
msgColor MsgStatusBadUs = cBadEvent
msgColor MsgStatusOthers = cBoring
msgColor MsgStatusWakeup = cWakeUp
msgColor MsgStatusStopUs = cBoring
msgColor MsgStatusStopThem = cBoring
msgColor MsgStatusLongerUs = cBoring  -- not important enough
msgColor MsgStatusLongThem = cBoring  -- not important enough, no disturb even
msgColor MsgItemCreation = cGreed
msgColor MsgItemRuination = cBoring  -- common, colourful components created
msgColor MsgDeathVictory = cVeryGoodEvent
msgColor MsgDeathDeafeat = cVeryBadEvent
msgColor MsgDeathBoring = cBoring
msgColor MsgRiskOfDeath = cGraveRisk
msgColor MsgPointmanSwap = cBoring
msgColor MsgFactionIntel = cMeta  -- good or bad
msgColor MsgFinalOutcome = cGameOver
msgColor MsgPlotExposition = cBoring
msgColor MsgBackdropInfo = cBoring
msgColor MsgTerrainReveal = cIdentification
msgColor MsgItemDiscovery = cIdentification
msgColor MsgSpottedActor = cBoring  -- too common; warning in @MsgSpottedThreat@
msgColor MsgSpottedThreat = cGraveRisk
msgColor MsgSpottedItem = cBoring
msgColor MsgItemMovement = cBoring
msgColor MsgActionMajor = cBoring
msgColor MsgActionMinor = cBoring
msgColor MsgEffectMajor = cRareNeutralEvent
msgColor MsgEffectMedium = cNeutralEvent
msgColor MsgEffectMinor = cBoring
msgColor MsgMiscellanous = cBoring
msgColor MsgHeardOutside = cBoring
msgColor MsgHeardNearby = cGraveRisk
msgColor MsgHeardFaraway = cRisk
msgColor MsgBackdropFocus = cVista
msgColor MsgActionWarning = cMeta
msgColor MsgRangedMightyWe = cGoodEvent
msgColor MsgRangedMightyUs = cVeryBadEvent
msgColor MsgRangedOthers = cBoring
msgColor MsgRangedNormalUs = cBadEvent
msgColor MsgNeutralEvent = cNeutralEvent
msgColor MsgSpecialEvent = cRareNeutralEvent
msgColor MsgMeleeMightyWe = cGoodEvent
msgColor MsgMeleeMightyUs = cVeryBadEvent
msgColor MsgMeleeComplexWe = cGoodEvent
msgColor MsgMeleeComplexUs = cBadEvent
msgColor MsgMeleeOthers = cBoring
msgColor MsgMeleeNormalUs = cBadEvent
msgColor MsgActionComplete = cBoring
msgColor MsgAtFeetMajor = cBoring
msgColor MsgAtFeetMinor = cBoring
msgColor MsgNumericReport = cBoring
msgColor MsgInnerWorkSpam = cBoring
msgColor MsgMacroOperation = cBoring
msgColor MsgRunStopReason = cBoring
msgColor MsgPromptNearby = cBoring
msgColor MsgPromptFocus = cVista
msgColor MsgPromptMention = cNeutralEvent
msgColor MsgPromptWarning = cMeta
msgColor MsgPromptThreat = cRisk
msgColor MsgPromptItems = cGreed
msgColor MsgActionAlert = cMeta
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

-- | Add a message to the start of report.
--
-- Empty messages are not added to make checking report emptiness easier.
consReport :: Msg -> Report -> Report
consReport msg rep | nullMsg msg = rep
consReport msg (Report r) = Report $ r ++ [RepMsgNK msg 1 1]

-- | Render a report as a (possibly very long) 'AttrString'. Filter out
-- messages not meant for display, unless not showing, but saving to history.
renderReport :: Bool -> Report -> [AttrString]
renderReport displaying (Report r) =
  let rep = map (\(RepMsgNK msg n k) -> if displaying
                                        then (msgShow msg, n)
                                        else (msgSave msg, k)) r
  in renderWholeReport rep []

-- | Render a report as a (possibly very long) 'AttrString'.
renderWholeReport :: [(AttrString, Int)] -> [AttrString] -> [AttrString]
renderWholeReport [] acc = acc
renderWholeReport (x : xs) acc = renderWholeReport xs (renderRepetition x : acc)

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
         then let combineMsg _ ([], _) ([], _) = Nothing
                  combineMsg msg (s, n) (t, k) = Just $
                    RepMsgNK msg{msgShow = s, msgSave = t} n k
                  zipMsg l1 l2 l3 = Report $ catMaybes $
                    zipWith3 combineMsg (map repMsg l1) l2 l3
                  newReport = zipMsg newMsgs scrapShowNew scrapSaveNew
                  oldReport = zipMsg oldMsgs scrapShowOld scrapSaveOld
              in Just History{..}
         else Nothing
    _ -> error "scrapRepetition: empty new report for scrapping"

-- | Add a message to the end of the report with the given repetition.
snocReport :: Report -> Msg -> Int -> Report
snocReport (Report r) msg n = Report $ RepMsgNK msg n n : r

-- | Add a message to the new report of history, eliminating a possible
-- duplicate and noting its existence in the result.
--
-- Empty messages are not added to make checking report emptiness easier.
addToReport :: History -> Msg -> Int -> Time -> (History, Bool)
addToReport hist msg _ _ | nullMsg msg = (hist, False)
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
archiveReport :: Bool -> History -> History
archiveReport uHistory1PerLine History{newReport=Report newMsgs, ..} =
  let newReportFiltered = Report $ filter (not . nullRepMsgNK) newMsgs
  in if nullReport newReportFiltered
     then -- Drop empty new report.
          History emptyReport timeZero oldReport oldTime archivedHistory
     else let lU = map attrStringToU
                   $ renderTimeReport uHistory1PerLine oldTime oldReport
          in History emptyReport timeZero newReportFiltered newTime
             $ foldl' (\ !h !v -> RB.cons v h) archivedHistory (reverse lU)

renderTimeReport :: Bool -> Time -> Report -> [AttrString]
renderTimeReport uHistory1PerLine t rep@(Report r) =
  let turns = t `timeFitUp` timeTurn
      repMsgs = renderReport False rep
      mgsClasses = reverse $ map (msgClassName . repMsg) r
      rederAS as = stringToAS (show turns ++ ": ") ++ as
      rederASClass (as, msgClassString) =
        stringToAS (show turns ++ ":")
        ++ map (Color.attrChar2ToW32 Color.BrBlack)
               ("[" ++ msgClassString ++ "]")
        ++ [Color.spaceAttrW32] ++ as
      worthSaving = not . all (Char.isSpace . Color.charFromW32)
  in if uHistory1PerLine
     then map rederASClass $ filter (worthSaving . fst) $ zip repMsgs mgsClasses
     else map rederAS $ filter worthSaving $ [foldr (<+:>) [] repMsgs]

lengthHistory :: Bool -> History -> Int
lengthHistory uHistory1PerLine History{oldReport, archivedHistory} =
  RB.length archivedHistory
  + length (renderTimeReport uHistory1PerLine timeZero oldReport)
    -- matches @renderHistory@

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: Bool -> History -> [AttrString]
renderHistory uHistory1PerLine History{..} =
  renderTimeReport uHistory1PerLine oldTime oldReport
  ++ map uToAttrString (RB.toList archivedHistory)
