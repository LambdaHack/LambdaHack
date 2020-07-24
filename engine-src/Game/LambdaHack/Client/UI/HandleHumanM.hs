-- | Semantics of human player commands.
module Game.LambdaHack.Client.UI.HandleHumanM
  ( restrictedCmdSemInCxtOfKM, updateKeyLast
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , noRemoteHumanCmd, cmdSemInCxtOfKM, cmdSemantics, addNoError
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanGlobalM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.SessionUI

-- | The semantics of human player commands in terms of the client monad,
-- in context of the given @km@ as the last action.
--
-- Some time cosuming commands are enabled even in aiming mode, but cannot be
-- invoked in aiming mode on a remote level (level different than
-- the level of the leader), which is caught here.
restrictedCmdSemInCxtOfKM :: (MonadClient m, MonadClientUI m)
                          => K.KM -> HumanCmd -> m (Either MError ReqUI)
restrictedCmdSemInCxtOfKM km cmd =
  if noRemoteHumanCmd cmd then do
    -- If in aiming mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevelUI
    if arena /= lidV then
      weaveJust <$> failWith
        "command disabled on a remote level, press ESC to switch back"
    else cmdSemInCxtOfKM km cmd
  else cmdSemInCxtOfKM km cmd

-- | Commands that are forbidden on a remote level, because they
-- would usually take time when invoked on one, but not necessarily do
-- what the player expects. Note that some commands that normally take time
-- are not included, because they don't take time in aiming mode
-- or their individual sanity conditions include a remote level check.
noRemoteHumanCmd :: HumanCmd -> Bool
noRemoteHumanCmd cmd = case cmd of
  Wait          -> True
  Wait10        -> True
  MoveItem{}    -> True
  Apply{}       -> True
  AlterDir{}    -> True
  AlterWithPointer{} -> True
  MoveOnceToXhair -> True
  RunOnceToXhair -> True
  ContinueToXhair -> True
  _ -> False

updateKeyLast :: K.KM -> HumanCmd -> KeyMacroFrame -> KeyMacroFrame
updateKeyLast km cmd macroFrame = case cmd of
  RepeatLast{} -> macroFrame
  Record{} -> macroFrame
  _ -> macroFrame {keyLast = Just km}

-- Semantics of the command in context of the given @km@ as the last action.
cmdSemInCxtOfKM :: (MonadClient m, MonadClientUI m)
                => K.KM -> HumanCmd -> m (Either MError ReqUI)
cmdSemInCxtOfKM km cmd = do
  modifySession $ \sess ->
    sess {smacroFrame = updateKeyLast km cmd $ smacroFrame sess}
  cmdSemantics cmd

cmdSemantics :: (MonadClient m, MonadClientUI m)
             => HumanCmd -> m (Either MError ReqUI)
cmdSemantics cmd = case cmd of
  Macro kms -> addNoError $ macroHuman kms
  ByArea l -> byAreaHuman cmdSemInCxtOfKM l
  ByAimMode AimModeCmd{..} ->
    byAimModeHuman (cmdSemantics exploration) (cmdSemantics aiming)
  ComposeIfLocal cmd1 cmd2 ->
    composeIfLocalHuman (cmdSemantics cmd1) (cmdSemantics cmd2)
  ComposeUnlessError cmd1 cmd2 ->
    composeUnlessErrorHuman (cmdSemantics cmd1) (cmdSemantics cmd2)
  Compose2ndLocal cmd1 cmd2 ->
    compose2ndLocalHuman (cmdSemantics cmd1) (cmdSemantics cmd2)
  LoopOnNothing cmd1 -> loopOnNothingHuman (cmdSemantics cmd1)
  ExecuteIfClear cmd1 -> executeIfClearHuman (cmdSemantics cmd1)

  Wait -> weaveJust <$> (ReqUITimed <$$> waitHuman)
  Wait10 -> weaveJust <$> (ReqUITimed <$$> waitHuman10)
  Yell -> weaveJust <$> (ReqUITimed <$$> yellHuman)
  MoveDir v ->
    weaveJust <$> (ReqUITimed <$$> moveRunHuman True True False False v)
  RunDir v -> weaveJust <$> (ReqUITimed <$$> moveRunHuman True True True True v)
  RunOnceAhead -> ReqUITimed <$$> runOnceAheadHuman
  MoveOnceToXhair -> weaveJust <$> (ReqUITimed <$$> moveOnceToXhairHuman)
  RunOnceToXhair  -> weaveJust <$> (ReqUITimed <$$> runOnceToXhairHuman)
  ContinueToXhair -> weaveJust <$> (ReqUITimed <$$> continueToXhairHuman)
  MoveItem stores toCStore mverb auto ->
    weaveJust <$> (ReqUITimed <$$> moveItemHuman stores toCStore mverb auto)
  Project -> weaveJust <$> (ReqUITimed <$$> projectHuman)
  Apply -> weaveJust <$> (ReqUITimed <$$> applyHuman)
  AlterDir -> weaveJust <$> (ReqUITimed <$$> alterDirHuman)
  AlterWithPointer -> weaveJust <$> (ReqUITimed <$$> alterWithPointerHuman)
  CloseDir -> weaveJust <$> (ReqUITimed <$$> closeDirHuman)
  Help -> helpHuman cmdSemInCxtOfKM
  Hint -> hintHuman cmdSemInCxtOfKM
  ItemMenu -> itemMenuHuman cmdSemInCxtOfKM
  ChooseItemMenu dialogMode -> chooseItemMenuHuman cmdSemInCxtOfKM dialogMode
  MainMenu -> mainMenuHuman cmdSemInCxtOfKM
  MainMenuAutoOn -> mainMenuAutoOnHuman cmdSemInCxtOfKM
  MainMenuAutoOff -> mainMenuAutoOffHuman cmdSemInCxtOfKM
  Dashboard -> dashboardHuman cmdSemInCxtOfKM
  GameDifficultyIncr -> gameDifficultyIncr >> challengesMenuHuman cmdSemInCxtOfKM
  GameWolfToggle -> gameWolfToggle >> challengesMenuHuman cmdSemInCxtOfKM
  GameFishToggle -> gameFishToggle >> challengesMenuHuman cmdSemInCxtOfKM
  GameScenarioIncr -> gameScenarioIncr >> challengesMenuHuman cmdSemInCxtOfKM

  GameRestart -> weaveJust <$> gameRestartHuman
  GameQuit -> weaveJust <$> gameQuitHuman
  GameDrop -> weaveJust <$> fmap Right gameDropHuman
  GameExit -> weaveJust <$> fmap Right gameExitHuman
  GameSave -> weaveJust <$> fmap Right gameSaveHuman
  Doctrine -> weaveJust <$> doctrineHuman
  Automate -> weaveJust <$> automateHuman
  AutomateToggle -> weaveJust <$> automateToggleHuman
  AutomateBack -> automateBackHuman

  ChooseItem dialogMode -> Left <$> chooseItemHuman dialogMode
  ChooseItemProject ts -> Left <$> chooseItemProjectHuman ts
  ChooseItemApply ts -> Left <$> chooseItemApplyHuman ts
  PickLeader k -> Left <$> pickLeaderHuman k
  PickLeaderWithPointer -> Left <$> pickLeaderWithPointerHuman
  MemberCycle -> Left <$> memberCycleHuman
  MemberBack -> Left <$> memberBackHuman
  SelectActor -> addNoError selectActorHuman
  SelectNone -> addNoError selectNoneHuman
  SelectWithPointer -> Left <$> selectWithPointerHuman
  Repeat n -> addNoError $ repeatHuman n
  RepeatLast n -> addNoError $ repeatLastHuman n
  Record -> addNoError recordHuman
  AllHistory -> addNoError allHistoryHuman
  LastHistory -> addNoError lastHistoryHuman
  MarkVision -> markVisionHuman >> settingsMenuHuman cmdSemInCxtOfKM
  MarkSmell -> markSmellHuman >> settingsMenuHuman cmdSemInCxtOfKM
  MarkSuspect -> markSuspectHuman >> settingsMenuHuman cmdSemInCxtOfKM
  MarkAnim -> markAnimHuman >> settingsMenuHuman cmdSemInCxtOfKM
  SettingsMenu -> settingsMenuHuman cmdSemInCxtOfKM
  ChallengesMenu -> challengesMenuHuman cmdSemInCxtOfKM
  PrintScreen -> addNoError printScreenHuman

  Cancel -> addNoError cancelHuman
  Accept -> addNoError acceptHuman
  ClearTargetIfItemClear -> addNoError clearTargetIfItemClearHuman
  ItemClear -> addNoError itemClearHuman
  MoveXhair v k -> Left <$> moveXhairHuman v k
  AimTgt -> addNoError aimTgtHuman
  AimFloor -> addNoError aimFloorHuman
  AimEnemy -> addNoError aimEnemyHuman
  AimItem -> addNoError aimItemHuman
  AimAscend k -> Left <$> aimAscendHuman k
  EpsIncr b -> addNoError $ epsIncrHuman b
  XhairUnknown -> Left <$> xhairUnknownHuman
  XhairItem -> Left <$> xhairItemHuman
  XhairStair up -> Left <$> xhairStairHuman up
  XhairPointerFloor -> addNoError xhairPointerFloorHuman
  XhairPointerEnemy -> addNoError xhairPointerEnemyHuman
  AimPointerFloor -> addNoError aimPointerFloorHuman
  AimPointerEnemy -> addNoError aimPointerEnemyHuman

addNoError :: Monad m => m () -> m (Either MError ReqUI)
addNoError cmdCli = cmdCli >> return (Left Nothing)
