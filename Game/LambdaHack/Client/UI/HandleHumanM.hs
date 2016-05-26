-- | Semantics of human player commands.
module Game.LambdaHack.Client.UI.HandleHumanM
  ( cmdHumanSem
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Functor.Infix ((<$$>))

import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HandleHumanGlobalM
import Game.LambdaHack.Client.UI.HandleHumanLocalM
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.Request

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Some time cosuming commands are enabled in aiming mode, but cannot be
-- invoked in aiming mode on a remote level (level different than
-- the level of the leader).
cmdHumanSem :: MonadClientUI m => HumanCmd -> m (Either MError RequestUI)
cmdHumanSem cmd =
  if noRemoteHumanCmd cmd then do
    -- If in aiming mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevelUI
    if arena /= lidV then
      weaveJust <$> failWith
        "command disabled on a remote level, press ESC to switch back"
    else cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m
          => HumanCmd -> m (Either MError RequestUI)
cmdAction cmd = case cmd of
  ReplaceFail failureMsg cmd1 ->
    cmdAction cmd1 >>= either (const $ weaveJust <$> failWith failureMsg)
                              (return . Right)
  Macro kms -> addNoError $ macroHuman kms
  ByArea l -> byAreaHuman cmdAction l
  ByAimMode{..} ->
    byAimModeHuman (cmdAction notAiming) (cmdAction aiming)
  ByItemMode{..} ->
    byItemModeHuman (cmdAction notChosen) (cmdAction chosen)
  ComposeIfLocal cmd1 cmd2 ->
    composeIfLocalHuman (cmdAction cmd1) (cmdAction cmd2)
  ComposeUnlessError cmd1 cmd2 ->
    composeUnlessErrorHuman (cmdAction cmd1) (cmdAction cmd2)

  Wait -> weaveJust <$> Right <$> fmap timedToUI waitHuman
  MoveDir v -> weaveJust <$> (ReqUITimed <$$> moveRunHuman True True False False v)
  RunDir v -> weaveJust <$> (ReqUITimed <$$> moveRunHuman True True True True v)
  RunOnceAhead -> runOnceAheadHuman
  MoveOnceToXhair -> weaveJust <$> (ReqUITimed <$$> moveOnceToXhairHuman)
  RunOnceToXhair  -> weaveJust <$> (ReqUITimed <$$> runOnceToXhairHuman)
  ContinueToXhair -> weaveJust <$> (ReqUITimed <$$> continueToXhairHuman)
  MoveItem cLegalRaw toCStore mverb _ auto ->
    weaveJust <$> (timedToUI <$$> moveItemHuman cLegalRaw toCStore mverb auto)
  Project ts -> weaveJust <$> (timedToUI <$$> projectHuman ts)
  Apply ts -> weaveJust <$> (timedToUI <$$> applyHuman ts)
  AlterDir ts -> weaveJust <$> (timedToUI <$$> alterDirHuman ts)
  TriggerTile ts -> weaveJust <$> (timedToUI <$$> triggerTileHuman ts)
  Help -> helpHuman cmdAction
  MainMenu -> mainMenuHuman cmdAction
  GameDifficultyIncr -> gameDifficultyIncr >> mainMenuHuman cmdAction

  GameRestart t -> weaveJust <$> gameRestartHuman t
  GameExit -> weaveJust <$> gameExitHuman
  GameSave -> weaveJust <$> fmap Right gameSaveHuman
  Tactic -> weaveJust <$> tacticHuman
  Automate -> weaveJust <$> automateHuman

  Clear -> addNoError clearHuman
  ChooseItem cstore -> Left <$> chooseItemHuman cstore
  ChooseItemProject ts -> Left <$> chooseItemProjectHuman ts
  ChooseItemApply ts -> Left <$> chooseItemApplyHuman ts
  PickLeader k -> Left <$> pickLeaderHuman k
  PickLeaderWithPointer -> Left <$> pickLeaderWithPointerHuman
  MemberCycle -> Left <$> memberCycleHuman
  MemberBack -> Left <$> memberBackHuman
  SelectActor -> addNoError selectActorHuman
  SelectNone -> addNoError selectNoneHuman
  SelectWithPointer -> addNoError selectWithPointerHuman
  Repeat n -> addNoError $ repeatHuman n
  Record -> addNoError recordHuman
  History -> addNoError historyHuman
  MarkVision -> markVisionHuman >> settingsMenuHuman cmdAction
  MarkSmell -> markSmellHuman >> settingsMenuHuman cmdAction
  MarkSuspect -> markSuspectHuman >> settingsMenuHuman cmdAction
  SettingsMenu -> settingsMenuHuman cmdAction

  Cancel -> addNoError cancelHuman
  Accept -> addNoError acceptHuman
  TgtClear -> addNoError tgtClearHuman
  MoveXhair v k -> Left <$> moveXhairHuman v k
  AimTgt -> addNoError aimTgtHuman
  AimFloor -> addNoError aimFloorHuman
  AimEnemy -> addNoError aimEnemyHuman
  AimAscend k -> Left <$> aimAscendHuman k
  EpsIncr b -> Left <$> epsIncrHuman b
  XhairUnknown -> Left <$> xhairUnknownHuman
  XhairItem -> Left <$> xhairItemHuman
  XhairStair up -> Left <$> xhairStairHuman up
  XhairPointerFloor -> addNoError xhairPointerFloorHuman
  XhairPointerEnemy -> addNoError xhairPointerEnemyHuman
  AimPointerFloor -> addNoError aimPointerFloorHuman
  AimPointerEnemy -> addNoError aimPointerEnemyHuman

addNoError :: Monad m => m () -> m (Either MError RequestUI)
addNoError cmdCli = cmdCli >> return (Left Nothing)
