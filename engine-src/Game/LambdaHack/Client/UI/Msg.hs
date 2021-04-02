{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read
-- and then saved to player history.
module Game.LambdaHack.Client.UI.Msg
  ( -- * Msg
    Msg, MsgShared, toMsgShared, toMsgDistinct
  , MsgClassShowAndSave(..), MsgClassShow(..), MsgClassSave(..)
  , MsgClassIgnore(..), MsgClassDistinct(..)
  , MsgClass, interruptsRunning, disturbsResting
    -- * Report
  , Report, nullVisibleReport, consReport, renderReport, anyInReport
    -- * History
  , History, newReport, emptyHistory, addToReport, addEolToNewReport
  , archiveReport, lengthHistory, renderHistory
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , UAttrString, uToAttrString, attrStringToU
  , toMsg, MsgPrototype, tripleFromProto
  , scrapsRepeats, tutorialHint, msgColor
  , RepMsgNK, nullRepMsgNK
  , emptyReport, renderWholeReport, renderRepetition
  , scrapRepetitionSingle, scrapRepetition, renderTimeReport
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Char as Char
import qualified Data.Set as S
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
  { msgShow  :: AttrString  -- ^ the colours and characters of the message
                            --   to be shown on the screen; not just text,
                            --   in case there was some colour not coming
                            --   from the message class
  , msgSave  :: AttrString  -- ^ the same to be saved in the message log only
  , msgClass :: MsgClass
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Msg

toMsg :: [(String, Color.Color)] -> MsgPrototype -> Msg
toMsg prefixColors msgProto =
  let (tShow, tSave, msgClass) = tripleFromProto msgProto
      msgClassName = showSimpleMsgClass msgClass
      mprefixColor = find ((`isPrefixOf` msgClassName) . fst) prefixColors
      color = maybe (msgColor msgClass) snd mprefixColor
      msgShow = textFgToAS color tShow
      msgSave = textFgToAS color tSave
  in Msg {..}

data MsgPrototype =
    MsgProtoShowAndSave MsgClassShowAndSave Text
  | MsgProtoShow MsgClassShow Text
  | MsgProtoSave MsgClassSave Text
  | MsgProtoIgnore MsgClassIgnore
  | MsgProtoDistinct MsgClassDistinct Text Text

tripleFromProto :: MsgPrototype -> (Text, Text, MsgClass)
tripleFromProto = \case
  MsgProtoShowAndSave x t -> (t, t, MsgClassShowAndSave x)
  MsgProtoShow x t -> (t, "", MsgClassShow x)
  MsgProtoSave x t -> ("", t, MsgClassSave x)
  MsgProtoIgnore x -> ("", "", MsgClassIgnore x)
  MsgProtoDistinct x t1 t2 -> (t1, t2, MsgClassDistinct x)

class MsgShared a where
  toMsgShared :: [(String, Color.Color)] -> a -> Text -> Msg

instance MsgShared MsgClassShowAndSave where
  toMsgShared prefixColors msgClass t =
    toMsg prefixColors $ MsgProtoShowAndSave msgClass t

instance MsgShared MsgClassShow where
  toMsgShared prefixColors msgClass t =
    toMsg prefixColors $ MsgProtoShow msgClass t

instance MsgShared MsgClassSave where
  toMsgShared prefixColors msgClass t =
    toMsg prefixColors $ MsgProtoSave msgClass t

instance MsgShared MsgClassIgnore where
  toMsgShared prefixColors msgClass _ =
    toMsg prefixColors $ MsgProtoIgnore msgClass

toMsgDistinct :: [(String, Color.Color)] -> MsgClassDistinct -> Text -> Text
              -> Msg
toMsgDistinct prefixColors msgClass t1 t2 =
  toMsg prefixColors $ MsgProtoDistinct msgClass t1 t2

-- Each constructor name should have length as asserted in @emptyReport@,
-- so that the message log with message classes (if set in config) looks tidy.
data MsgClass =
    MsgClassShowAndSave MsgClassShowAndSave
  | MsgClassShow MsgClassShow
  | MsgClassSave MsgClassSave
  | MsgClassIgnore MsgClassIgnore
  | MsgClassDistinct MsgClassDistinct
  deriving (Show, Eq, Ord, Generic)

instance Binary MsgClass

showSimpleMsgClass :: MsgClass -> String
showSimpleMsgClass = \case
  MsgClassShowAndSave x -> show x
  MsgClassShow x -> show x
  MsgClassSave x -> show x
  MsgClassIgnore x -> show x
  MsgClassDistinct x -> show x

data MsgClassShowAndSave =
    MsgBookKeeping
  | MsgStatusWakeup
  | MsgStatusStopUs
  | MsgStatusStopThem
  | MsgItemCreation
  | MsgItemRuination
  | MsgDeathVictory
  | MsgDeathDeafeat
  | MsgDeathBoring
  | MsgRiskOfDeath
  | MsgPointmanSwap
  | MsgFactionIntel
  | MsgFinalOutcome
  | MsgPlotExposition
  | MsgBackdropInfo
  | MsgTerrainReveal
  | MsgItemDiscovery
  | MsgSpottedActor
  | MsgItemMovement
  | MsgActionMajor
  | MsgActionMinor
  | MsgEffectMajor
  | MsgEffectMedium
  | MsgEffectMinor
  | MsgMiscellanous
  | MsgHeardOutside
  | MsgHeardNearby
  | MsgHeardFaraway
  | MsgBackdropFocus
  | MsgActionWarning
  | MsgRangedMightyWe
  | MsgRangedMightyUs
  | MsgRangedOthers  -- not ours or projectiles are hit
  | MsgRangedNormalUs
  | MsgGoodMiscEvent
  | MsgBadMiscEvent
  | MsgNeutralEvent
  | MsgSpecialEvent
  | MsgMeleeMightyWe
  | MsgMeleeMightyUs
  | MsgMeleeComplexWe
  | MsgMeleeComplexUs
  | MsgMeleeOthers  -- not ours or projectiles are hit
  | MsgMeleeNormalUs
  | MsgActionComplete
  | MsgAtFeetMajor
  | MsgAtFeetMinor
  | MsgTutorialHint
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary MsgClassShowAndSave

data MsgClassShow =
    MsgPromptGeneric
  | MsgPromptFocus
  | MsgPromptMention
  | MsgPromptModify
  | MsgPromptActors
  | MsgPromptItems
  | MsgPromptAction
  | MsgActionAlert
  | MsgSpottedThreat
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary MsgClassShow

data MsgClassSave =
    MsgInnerWorkSpam
  | MsgNumericReport
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary MsgClassSave

data MsgClassIgnore =
    MsgMacroOperation
  | MsgRunStopReason
  | MsgStopPlayback
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary MsgClassIgnore

data MsgClassDistinct =
    MsgSpottedItem
  | MsgStatusSleep
  | MsgStatusGoodUs
  | MsgStatusBadUs
  | MsgStatusOthers
  | MsgStatusBenign
  | MsgStatusWarning
  | MsgStatusLongerUs
  | MsgStatusLongThem
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary MsgClassDistinct

interruptsRunning :: MsgClass -> Bool
interruptsRunning = \case
  MsgClassShowAndSave x -> case x of
    MsgBookKeeping -> False
    MsgStatusStopThem -> False
    MsgItemMovement -> False
    MsgActionMinor -> False
    MsgEffectMinor -> False
    MsgMiscellanous -> False  -- taunts are colourful, but spammy
    MsgHeardOutside -> False  -- cause must be 'profound', but even taunts are
    MsgHeardFaraway -> False
    -- MsgHeardNearby interrupts, even if running started while hearing close
    MsgRangedOthers -> False
    MsgAtFeetMinor -> False
    _ -> True
  MsgClassShow x -> case x of
    MsgPromptGeneric -> False
    MsgPromptFocus -> False
    MsgPromptMention -> False
    MsgPromptModify -> False
    MsgPromptActors -> False
    MsgPromptItems -> False
    MsgPromptAction -> False
    MsgActionAlert -> True  -- action alerts or questions cause alarm
    MsgSpottedThreat -> True
  MsgClassSave x -> case x of
    MsgInnerWorkSpam -> False
    MsgNumericReport -> False
  MsgClassIgnore x -> case x of
    MsgMacroOperation -> False
    MsgRunStopReason -> True
    MsgStopPlayback -> True
  MsgClassDistinct x -> case x of
    MsgSpottedItem -> False
    MsgStatusLongThem -> False
    MsgStatusOthers -> False
    MsgStatusBenign -> False
    MsgStatusWarning -> False
    _ -> True

disturbsResting :: MsgClass -> Bool
disturbsResting = \case
  MsgClassShowAndSave x -> case x of
    MsgPointmanSwap -> False  -- handled separately
    MsgItemDiscovery -> False  -- medium importance
    MsgHeardNearby -> False  -- handled separately; no disturbance if old
    _ -> interruptsRunning $ MsgClassShowAndSave x
  msgClass -> interruptsRunning msgClass

scrapsRepeats :: MsgClass -> Bool
scrapsRepeats = \case
  MsgClassShowAndSave x -> case x of
    MsgBookKeeping -> False  -- too important to scrap
    MsgDeathDeafeat -> False
    MsgRiskOfDeath -> False
    MsgFinalOutcome -> False
    _ -> True
  MsgClassShow x -> case x of
    MsgPromptGeneric -> False
    MsgPromptFocus -> False
    MsgPromptMention -> False
    MsgPromptModify -> False
    MsgPromptActors -> False
    MsgPromptItems -> False
    MsgPromptAction -> False
    MsgActionAlert -> False
    MsgSpottedThreat -> True
  MsgClassSave x -> case x of
    MsgInnerWorkSpam -> True
    MsgNumericReport -> True
  MsgClassIgnore _ -> False  -- ignored, so no need to scrap
  MsgClassDistinct x -> case x of
    _ -> True

tutorialHint :: MsgClass -> Bool
tutorialHint = \case
  MsgClassShowAndSave x -> case x of  -- show and save: least surprise
    MsgTutorialHint -> True
    _ -> False
  MsgClassShow _ -> False
  MsgClassSave _ -> False
  MsgClassIgnore _ -> False
  MsgClassDistinct _ -> False

-- Only initially @White@ colour in text (e.g., not highlighted @BrWhite@)
-- gets replaced by the one indicated.
msgColor :: MsgClass -> Color.Color
msgColor = \case
  MsgClassShowAndSave x -> case x of
    MsgBookKeeping -> Color.cBoring
    MsgStatusWakeup -> Color.cWakeUp
    MsgStatusStopUs -> Color.cBoring
    MsgStatusStopThem -> Color.cBoring
    MsgItemCreation -> Color.cGreed
    MsgItemRuination -> Color.cBoring  -- common, colourful components created
    MsgDeathVictory -> Color.cVeryGoodEvent
    MsgDeathDeafeat -> Color.cVeryBadEvent
    MsgDeathBoring -> Color.cBoring
    MsgRiskOfDeath -> Color.cGraveRisk
    MsgPointmanSwap -> Color.cBoring
    MsgFactionIntel -> Color.cMeta  -- good or bad
    MsgFinalOutcome -> Color.cGameOver
    MsgPlotExposition -> Color.cBoring
    MsgBackdropInfo -> Color.cBoring
    MsgTerrainReveal -> Color.cIdentification
    MsgItemDiscovery -> Color.cIdentification
    MsgSpottedActor -> Color.cBoring  -- common; warning in @MsgSpottedThreat@
    MsgItemMovement -> Color.cBoring
    MsgActionMajor -> Color.cBoring
    MsgActionMinor -> Color.cBoring
    MsgEffectMajor -> Color.cRareNeutralEvent
    MsgEffectMedium -> Color.cNeutralEvent
    MsgEffectMinor -> Color.cBoring
    MsgMiscellanous -> Color.cBoring
    MsgHeardOutside -> Color.cBoring
    MsgHeardNearby -> Color.cGraveRisk
    MsgHeardFaraway -> Color.cRisk
    MsgBackdropFocus -> Color.cVista
    MsgActionWarning -> Color.cMeta
    MsgRangedMightyWe -> Color.cGoodEvent
    MsgRangedMightyUs -> Color.cVeryBadEvent
    MsgRangedOthers -> Color.cBoring
    MsgRangedNormalUs -> Color.cBadEvent
    MsgGoodMiscEvent -> Color.cGoodEvent
    MsgBadMiscEvent -> Color.cBadEvent
    MsgNeutralEvent -> Color.cNeutralEvent
    MsgSpecialEvent -> Color.cRareNeutralEvent
    MsgMeleeMightyWe -> Color.cGoodEvent
    MsgMeleeMightyUs -> Color.cVeryBadEvent
    MsgMeleeComplexWe -> Color.cGoodEvent
    MsgMeleeComplexUs -> Color.cBadEvent
    MsgMeleeOthers -> Color.cBoring
    MsgMeleeNormalUs -> Color.cBadEvent
    MsgActionComplete -> Color.cBoring
    MsgAtFeetMajor -> Color.cBoring
    MsgAtFeetMinor -> Color.cBoring
    MsgTutorialHint -> Color.cTutorialHint
  MsgClassShow x -> case x of
    MsgPromptGeneric -> Color.cBoring
    MsgPromptFocus -> Color.cVista
    MsgPromptMention -> Color.cNeutralEvent
    MsgPromptModify -> Color.cRareNeutralEvent
    MsgPromptActors -> Color.cRisk
    MsgPromptItems -> Color.cGreed
    MsgPromptAction -> Color.cMeta
    MsgActionAlert -> Color.cMeta
    MsgSpottedThreat -> Color.cGraveRisk
  MsgClassSave x -> case x of
    MsgInnerWorkSpam -> Color.cBoring
    MsgNumericReport -> Color.cBoring
  MsgClassIgnore x -> case x of
    MsgMacroOperation -> Color.cBoring
    MsgRunStopReason -> Color.cBoring
    MsgStopPlayback -> Color.cMeta
  MsgClassDistinct x -> case x of
    MsgSpottedItem -> Color.cBoring
    MsgStatusSleep -> Color.cSleep
    MsgStatusGoodUs -> Color.cGoodEvent
    MsgStatusBadUs -> Color.cBadEvent
    MsgStatusOthers -> Color.cBoring
    MsgStatusBenign -> Color.cBoring
    MsgStatusWarning -> Color.cMeta
    MsgStatusLongerUs -> Color.cBoring  -- not important enough
    MsgStatusLongThem -> Color.cBoring  -- not important enough, no disturb even

-- * Report

data RepMsgNK = RepMsgNK {repMsg :: Msg, _repShow :: Int, _repSave :: Int}
  deriving (Show, Generic)

instance Binary RepMsgNK

-- | If only one of the message components is non-empty and non-whitespace,
-- but its count is zero, the message is considered empty.
nullRepMsgNK :: RepMsgNK -> Bool
nullRepMsgNK (RepMsgNK Msg{..} _ _) =
  all (Char.isSpace . Color.charFromW32) msgShow
  && all (Char.isSpace . Color.charFromW32) msgSave

-- | The set of messages, with repetitions, to show at the screen at once.
newtype Report = Report [RepMsgNK]
  deriving (Show, Binary)

-- | Empty set of messages.
emptyReport :: Report
emptyReport = assert (let checkLen msgClass =
                            let len = length (showSimpleMsgClass msgClass)
                            in len >= 14 && len <= 17
                          l = map MsgClassShowAndSave [minBound .. maxBound]
                              ++ map MsgClassShow [minBound .. maxBound]
                              ++ map MsgClassSave [minBound .. maxBound]
                              ++ map MsgClassIgnore [minBound .. maxBound]
                              ++ map MsgClassDistinct [minBound .. maxBound]
                      in allB checkLen l)
              $ Report []  -- as good place as any to verify display lengths

-- | Test if the list of non-whitespace messages is empty.
nullVisibleReport :: Report -> Bool
nullVisibleReport (Report l) =
  all (all (Char.isSpace . Color.charFromW32) . msgShow . repMsg) l

-- | Add a message to the start of report.
consReport :: Msg -> Report -> Report
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
renderRepetition (asRaw, n) =
  let as = dropWhileEnd (Char.isSpace . Color.charFromW32) asRaw
  in if n <= 1 || null as
     then as
     else as ++ stringToAS ("<x" ++ show n ++ ">")

anyInReport :: (MsgClass -> Bool) -> Report -> Bool
anyInReport f (Report xns) = any (f . msgClass. repMsg) xns

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

scrapRepetitionSingle :: (AttrString, Int)
                      -> [(AttrString, Int)]
                      -> [(AttrString, Int)]
                      -> (Bool, [(AttrString, Int)], [(AttrString, Int)])
scrapRepetitionSingle (s1, n1) rest1 oldMsgs =
  let butLastEOLs = dropWhileEnd ((== '\n') . Color.charFromW32)
      eqs1 (s2, _) = butLastEOLs s1 == butLastEOLs s2
  in case break eqs1 rest1 of
    (_, []) -> case break eqs1 oldMsgs of
      (noDup, (_, n2) : rest2) ->
        -- We keep the occurence of the message in the new report only.
        let newReport = (s1, n1 + n2) : rest1
            oldReport = noDup ++ ([], 0) : rest2
        in (True, newReport, oldReport)
      _ -> (False, (s1, n1) : rest1, oldMsgs)
    (noDup, (s2, n2) : rest3) ->
      -- We keep the older (and so, oldest) occurence of the message,
      -- to avoid visual disruption by moving the message around.
      let newReport = ([], 0) : noDup ++ (s2, n1 + n2) : rest3
          oldReport = oldMsgs
      in (True, newReport, oldReport)

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
      let -- We ignore message classes and scrap even if same strings
          -- come from different classes. Otherwise user would be confused.
          makeShow = map (\(RepMsgNK msg n _) -> (msgShow msg, n))
          makeSave = map (\(RepMsgNK msg _ k) -> (msgSave msg, k))
          (scrapShowNeeded, scrapShowNew, scrapShowOld) =
            scrapRepetitionSingle (msgShow msg1, n1)
                                  (makeShow rest1)
                                  (makeShow oldMsgs)
          (scrapSaveNeeded, scrapSaveNew, scrapSaveOld) =
            scrapRepetitionSingle (msgSave msg1, k1)
                                  (makeSave rest1)
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

-- | Add a message to the new report of history, eliminating a possible
-- duplicate and noting its existence in the result.
addToReport :: S.Set Msg -> Bool -> Bool -> History -> Msg -> Time
            -> (S.Set Msg, History, Bool)
addToReport usedHints displayHints inMelee
            oldHistory@History{newReport = Report r, ..} msgRaw time =
  -- When each turn we lose HP, stuff that wouldn't interrupt
  -- running should go at most to message log, not onto the screen,
  -- unless it goes only onto screen, so the message would be lost.
  let isMsgClassShow = \case
        MsgClassShow{} -> True
        _ -> False
      msg = if inMelee
               && not (interruptsRunning (msgClass msgRaw))
               && not (isMsgClassShow $ msgClass msgRaw)
            then msgRaw {msgShow = []}
            else msgRaw
      repMsgNK = RepMsgNK msg 1 1
      newH = History { newReport = Report $ repMsgNK : r
                     , newTime = time
                     , .. }
      nusedHints = S.insert msg usedHints
  in -- Tutorial hint shown only when tutorial enabled and hint not yet shown.
     if | tutorialHint (msgClass msg)
          && (not displayHints || S.member msg usedHints) ->
          (usedHints, oldHistory, False)
        | not (scrapsRepeats $ msgClass msg)
          || nullRepMsgNK repMsgNK ->
          -- Don't waste time on never shown messages.
          (nusedHints, newH, False)
        | otherwise -> case scrapRepetition newH of
            Just scrappedH -> (nusedHints, scrappedH, True)
            Nothing -> (nusedHints, newH, False)

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
  let newFiltered@(Report r) = Report $ filter (not . nullRepMsgNK) newMsgs
  in if null r
     then -- Drop empty new report.
          History emptyReport timeZero oldReport oldTime archivedHistory
     else let lU = map attrStringToU
                   $ renderTimeReport uHistory1PerLine oldTime oldReport
          in History emptyReport timeZero newFiltered newTime
             $ foldl' (\ !h !v -> RB.cons v h) archivedHistory lU

renderTimeReport :: Bool -> Time -> Report -> [AttrString]
renderTimeReport uHistory1PerLine t rep@(Report r) =
  let turns = t `timeFitUp` timeTurn
      repMsgs = renderReport False rep
      mgsClasses = reverse $ map (showSimpleMsgClass . msgClass . repMsg) r
      turnsString = show turns
      isSpace32 = Char.isSpace . Color.charFromW32
      worthSaving = not . all isSpace32
      renderAS as = stringToAS (turnsString ++ ": ") ++ dropWhile isSpace32 as
      renderClass (as, msgClassString) =
        let lenUnderscore = 17 - length msgClassString
                            + max 0 (3 - length turnsString)
        in stringToAS (turnsString ++ ":")
           ++ map (Color.attrChar2ToW32 Color.BrBlack)
                  ("[" ++ replicate lenUnderscore '_' ++ msgClassString ++ "]")
           ++ [Color.spaceAttrW32]
           ++ dropWhile isSpace32 as
  in if uHistory1PerLine
     then map renderClass $ filter (worthSaving . fst) $ zip repMsgs mgsClasses
     else map renderAS $ filter worthSaving [foldr (<+:>) [] repMsgs]

lengthHistory :: Bool -> History -> Int
lengthHistory uHistory1PerLine History{oldReport, archivedHistory} =
  RB.length archivedHistory
  + length (renderTimeReport uHistory1PerLine timeZero oldReport)
    -- matches @renderHistory@

-- | Render history as many lines of text. New report is not rendered.
-- It's expected to be empty when history is shown.
renderHistory :: Bool -> History -> [AttrString]
renderHistory uHistory1PerLine History{..} =
  map uToAttrString (RB.toList archivedHistory)
  ++ renderTimeReport uHistory1PerLine oldTime oldReport
