-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Data.Monoid

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanGlobal
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Common.ServerCmd

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the leader).
cmdHumanSem :: MonadClientUI m => HumanCmd -> m (SlideOrCmd CmdSer)
cmdHumanSem cmd = do
  if noRemoteHumanCmd cmd then do
    -- | If in targeting mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevel
    if (arena /= lidV) then
      failWith $ "command disabled on a remote level, press ESC to switch back"
    else cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m => HumanCmd -> m (SlideOrCmd CmdSer)
cmdAction cmd = case cmd of
  Move v -> fmap (fmap CmdTakeTimeSer) $ moveRunHuman False v
  Run v -> fmap (fmap CmdTakeTimeSer) $ moveRunHuman True v
  Wait -> fmap Right $ fmap CmdTakeTimeSer waitHuman
  Pickup -> fmap (fmap CmdTakeTimeSer) pickupHuman
  Drop -> fmap (fmap CmdTakeTimeSer) dropHuman
  Project ts -> fmap (fmap CmdTakeTimeSer) $ projectHuman ts
  Apply ts -> fmap (fmap CmdTakeTimeSer) $ applyHuman ts
  AlterDir ts -> fmap (fmap CmdTakeTimeSer) $ alterDirHuman ts
  TriggerTile ts -> fmap (fmap CmdTakeTimeSer) $ triggerTileHuman ts
  StepToTarget -> fmap (fmap CmdTakeTimeSer) stepToTargetHuman
  Resend -> fmap (fmap CmdTakeTimeSer) resendHuman

  GameRestart t -> gameRestartHuman t
  GameExit -> gameExitHuman
  GameSave -> fmap Right gameSaveHuman
  GameDifficultyCycle -> fmap Right gameDifficultyCycle

  PickLeader k -> fmap Left $ pickLeaderHuman k
  MemberCycle -> fmap Left memberCycleHuman
  MemberBack -> fmap Left memberBackHuman
  Inventory -> fmap Left inventoryHuman
  SelectActor -> fmap Left selectActorHuman
  SelectNone -> addNoSlides selectNoneHuman
  Clear -> addNoSlides clearHuman
  Repeat n -> addNoSlides $ repeatHuman n
  Record -> fmap Left recordHuman
  Macro _ kms -> addNoSlides $ macroHuman kms
  History -> fmap Left historyHuman
  MarkVision -> addNoSlides humanMarkVision
  MarkSmell -> addNoSlides humanMarkSmell
  MarkSuspect -> addNoSlides humanMarkSuspect
  Help -> fmap Left displayMainMenu

  TgtFloor -> fmap Left tgtFloorHuman
  TgtEnemy -> fmap Left tgtEnemyHuman
  TgtUnknown -> fmap Left tgtUnknownHuman
  TgtItem -> fmap Left tgtItemHuman
  TgtStair up -> fmap Left $ tgtStairHuman up
  TgtAscend k -> fmap Left $ tgtAscendHuman k
  EpsIncr b -> fmap Left $ epsIncrHuman b
  TgtClear -> addNoSlides tgtClearHuman
  Cancel -> fmap Left $ cancelHuman displayMainMenu
  Accept -> fmap Left $ acceptHuman helpHuman

addNoSlides :: Monad m => m () -> m (SlideOrCmd CmdSer)
addNoSlides cmdCli = cmdCli >> return (Left mempty)
