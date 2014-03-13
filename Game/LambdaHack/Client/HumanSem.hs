-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Data.Monoid

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.HumanGlobal
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Common.HumanCmd
import Game.LambdaHack.Common.Request

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Some time cosuming commands are enabled in targeting mode, but cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the leader).
cmdHumanSem :: MonadClientUI m => HumanCmd -> m (SlideOrCmd Request)
cmdHumanSem cmd = do
  if noRemoteHumanCmd cmd then do
    -- If in targeting mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevel
    if (arena /= lidV) then
      failWith $ "command disabled on a remote level, press ESC to switch back"
    else cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m => HumanCmd -> m (SlideOrCmd Request)
cmdAction cmd = case cmd of
  Move v -> fmap (fmap ReqTimed) $ moveRunHuman False v
  Run v -> fmap (fmap ReqTimed) $ moveRunHuman True v
  Wait -> fmap Right $ fmap ReqTimed waitHuman
  MoveItem cLegalRaw toCStore verbRaw _ auto ->
    fmap (fmap ReqTimed) $ moveItemHuman cLegalRaw toCStore verbRaw auto
  Project ts -> fmap (fmap ReqTimed) $ projectHuman ts
  Apply ts -> fmap (fmap ReqTimed) $ applyHuman ts
  AlterDir ts -> fmap (fmap ReqTimed) $ alterDirHuman ts
  TriggerTile ts -> fmap (fmap ReqTimed) $ triggerTileHuman ts
  StepToTarget -> fmap (fmap ReqTimed) stepToTargetHuman
  Resend -> fmap (fmap ReqTimed) resendHuman

  GameRestart t -> gameRestartHuman t
  GameExit -> gameExitHuman
  GameSave -> fmap Right gameSaveHuman
  Automate -> automateHuman

  GameDifficultyCycle -> addNoSlides gameDifficultyCycle
  PickLeader k -> fmap Left $ pickLeaderHuman k
  MemberCycle -> fmap Left memberCycleHuman
  MemberBack -> fmap Left memberBackHuman
  Inventory -> fmap Left inventoryHuman
  Equipment -> fmap Left equipmentHuman
  AllOwned -> fmap Left allOwnedHuman
  SelectActor -> fmap Left selectActorHuman
  SelectNone -> addNoSlides selectNoneHuman
  Clear -> addNoSlides clearHuman
  Repeat n -> addNoSlides $ repeatHuman n
  Record -> fmap Left recordHuman
  History -> fmap Left historyHuman
  MarkVision -> addNoSlides markVisionHuman
  MarkSmell -> addNoSlides markSmellHuman
  MarkSuspect -> addNoSlides markSuspectHuman
  Help -> fmap Left helpHuman
  MainMenu -> fmap Left mainMenuHuman
  Macro _ kms -> addNoSlides $ macroHuman kms

  MoveCursor v k -> fmap Left $ moveCursorHuman v k
  TgtFloor -> fmap Left tgtFloorHuman
  TgtEnemy -> fmap Left tgtEnemyHuman
  TgtUnknown -> fmap Left tgtUnknownHuman
  TgtItem -> fmap Left tgtItemHuman
  TgtStair up -> fmap Left $ tgtStairHuman up
  TgtAscend k -> fmap Left $ tgtAscendHuman k
  EpsIncr b -> fmap Left $ epsIncrHuman b
  TgtClear -> addNoSlides tgtClearHuman
  Cancel -> fmap Left $ cancelHuman mainMenuHuman
  Accept -> fmap Left $ acceptHuman helpHuman

addNoSlides :: Monad m => m () -> m (SlideOrCmd Request)
addNoSlides cmdCli = cmdCli >> return (Left mempty)
