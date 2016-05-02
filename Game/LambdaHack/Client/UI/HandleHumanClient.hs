-- | Semantics of human player commands.
module Game.LambdaHack.Client.UI.HandleHumanClient
  ( cmdHumanSem
  ) where

import Prelude ()
import Prelude.Compat

import Data.Functor.Infix ((<$$>))

import Game.LambdaHack.Client.UI.HandleHumanGlobalClient
import Game.LambdaHack.Client.UI.HandleHumanLocalClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
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
    lidV <- viewedLevel
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
    cmdAction cmd1 >>= either (const $ return $ Left $ Just failureMsg)
                              (return . Right)
  Macro kms -> addNoSlides $ macroHuman kms
  ByArea l -> byAreaHuman cmdAction l
  ByAimMode{..} ->
    byAimModeHuman (cmdAction notAiming) (cmdAction aiming)
  ByItemMode{..} ->
    byItemModeHuman (cmdAction notChosen) (cmdAction chosen)
  ComposeIfLeft cmd1 cmd2 ->
    composeIfLeftHuman (cmdAction cmd1) (cmdAction cmd2)
  ComposeIfEmpty cmd1 cmd2 ->
    composeIfEmptyHuman (cmdAction cmd1) (cmdAction cmd2)

  Wait -> weaveJust <$> Right <$> fmap timedToUI waitHuman
  MoveDir v -> weaveJust <$> (ReqUITimed <$$> moveRunHuman True True False False v)
  RunDir v -> weaveJust <$> (ReqUITimed <$$> moveRunHuman True True True True v)
  RunOnceAhead -> weaveJust <$> (ReqUITimed <$$> runOnceAheadHuman)
  MoveOnceToXhair -> weaveJust <$> (ReqUITimed <$$> moveOnceToXhairHuman)
  RunOnceToXhair  -> weaveJust <$> (ReqUITimed <$$> runOnceToXhairHuman)
  ContinueToXhair -> weaveJust <$> (ReqUITimed <$$> continueToXhairHuman)
  MoveItem cLegalRaw toCStore mverb _ auto ->
    weaveJust <$> (timedToUI <$$> moveItemHuman cLegalRaw toCStore mverb auto)
  Project ts -> weaveJust <$> (timedToUI <$$> projectHuman ts)
  Apply ts -> weaveJust <$> (timedToUI <$$> applyHuman ts)
  AlterDir ts -> weaveJust <$> (timedToUI <$$> alterDirHuman ts)
  TriggerTile ts -> weaveJust <$> (timedToUI <$$> triggerTileHuman ts)
  Help mstart -> helpHuman cmdAction mstart
  MainMenu -> mainMenuHuman cmdAction
  GameDifficultyIncr -> gameDifficultyIncr >> mainMenuHuman cmdAction

  GameRestart t -> weaveJust <$> gameRestartHuman t
  GameExit -> weaveJust <$> gameExitHuman
  GameSave -> weaveJust <$> fmap Right gameSaveHuman
  Tactic -> weaveJust <$> tacticHuman
  Automate -> weaveJust <$> automateHuman

  Clear -> addNoSlides clearHuman
  ChooseItem cstore -> Left <$> chooseItemHuman cstore
  ChooseItemProject ts -> Left <$> chooseItemProjectHuman ts
  ChooseItemApply ts -> Left <$> chooseItemApplyHuman ts
  PickLeader k -> Left <$> pickLeaderHuman k
  PickLeaderWithPointer -> Left <$> pickLeaderWithPointerHuman
  MemberCycle -> Left <$> memberCycleHuman
  MemberBack -> Left <$> memberBackHuman
  SelectActor -> addNoSlides selectActorHuman
  SelectNone -> addNoSlides selectNoneHuman
  SelectWithPointer -> addNoSlides selectWithPointerHuman
  Repeat n -> addNoSlides $ repeatHuman n
  Record -> addNoSlides recordHuman
  History -> addNoSlides historyHuman
  MarkVision -> markVisionHuman >> settingsMenuHuman cmdAction
  MarkSmell -> markSmellHuman >> settingsMenuHuman cmdAction
  MarkSuspect -> markSuspectHuman >> settingsMenuHuman cmdAction
  SettingsMenu -> settingsMenuHuman cmdAction

  Cancel -> addNoSlides cancelHuman
  Accept -> addNoSlides acceptHuman
  TgtClear -> addNoSlides tgtClearHuman
  MoveXhair v k -> Left <$> moveXhairHuman v k
  AimTgt -> addNoSlides aimTgtHuman
  AimFloor -> addNoSlides aimFloorHuman
  AimEnemy -> addNoSlides aimEnemyHuman
  AimAscend k -> Left <$> aimAscendHuman k
  EpsIncr b -> Left <$> epsIncrHuman b
  XhairUnknown -> Left <$> xhairUnknownHuman
  XhairItem -> Left <$> xhairItemHuman
  XhairStair up -> Left <$> xhairStairHuman up
  XhairPointerFloor -> addNoSlides xhairPointerFloorHuman
  XhairPointerEnemy -> addNoSlides xhairPointerEnemyHuman
  AimPointerFloor -> addNoSlides aimPointerFloorHuman
  AimPointerEnemy -> addNoSlides aimPointerEnemyHuman

addNoSlides :: Monad m => m () -> m (Either MError RequestUI)
addNoSlides cmdCli = cmdCli >> return (Left Nothing)
