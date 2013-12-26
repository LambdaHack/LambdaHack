-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.EnumSet as ES
import Data.Maybe

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanGlobal
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Common.VectorXY

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the leader).
cmdHumanSem :: (MonadClientAbort m, MonadClientUI m)
            => HumanCmd -> WriterT Slideshow m (Abort CmdSer)
cmdHumanSem cmd = do
  arena <- getArenaUI
  when (noRemoteHumanCmd cmd) $ checkCursor arena
  cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: (MonadClientAbort m, MonadClientUI m)
          => HumanCmd -> WriterT Slideshow m (Abort CmdSer)
cmdAction cmd = case cmd of
  Move v -> moveRunHuman False v
  Run v -> moveRunHuman True v
  Wait -> fmap (Right . TakeTimeSer) waitHuman
  Pickup -> fmap (fmap TakeTimeSer) pickupHuman
  Drop -> fmap (fmap TakeTimeSer) dropHuman
  Project ts -> projectHuman ts
  Apply ts -> fmap (fmap TakeTimeSer) $ applyHuman ts
  AlterDir ts -> fmap (fmap TakeTimeSer) $ alterDirHuman ts
  TriggerTile ts ->  fmap (fmap TakeTimeSer) $ triggerTileHuman ts

  GameRestart t -> gameRestartHuman t
  GameExit -> gameExitHuman
  GameSave -> fmap Right gameSaveHuman

  PickLeader k -> pickLeaderHuman k >> return (Left "")
  MemberCycle -> memberCycleHuman >> return (Left "")
  MemberBack -> memberBackHuman >> return (Left "")
  Inventory -> inventoryHuman >> return (Left "")
  TgtFloor -> tgtFloorHuman
  TgtEnemy -> tgtEnemyHuman
  TgtAscend k -> tgtAscendHuman k >> return (Left "")
  EpsIncr b -> epsIncrHuman b >> return (Left "")
  SelectActor -> selectActorHuman >> return (Left "")
  SelectNone -> selectNoneHuman >> return (Left "")
  Cancel -> cancelHuman displayMainMenu >> return (Left "")
  Accept -> acceptHuman helpHuman >> return (Left "")
  Clear -> clearHuman >> return (Left "")
  History -> historyHuman >> return (Left "")
  MarkVision -> humanMarkVision >> return (Left "")
  MarkSmell -> humanMarkSmell >> return (Left "")
  MarkSuspect -> humanMarkSuspect >> return (Left "")
  Help -> displayMainMenu >> return (Left "")

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: (MonadClientAbort m, MonadClientUI m) => LevelId -> m ()
checkCursor arena = do
  (lid, _) <- viewedLevel
  when (arena /= lid) $
    abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

moveRunHuman :: (MonadClientAbort m, MonadClientUI m)
             => Bool -> VectorXY -> WriterT Slideshow m (Abort CmdSer)
moveRunHuman run v = do
  tgtMode <- getsClient stgtMode
  (arena, Level{lxsize}) <- viewedLevel
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  let dir = toDir lxsize v
  if isJust tgtMode then do
    moveCursor dir (if run then 10 else 1) >> return (Left "")
  else do
    let tpos = bpos sb `shift` dir
    -- We start by checking actors at the the target position,
    -- which gives a partial information (actors can be invisible),
    -- as opposed to accessibility (and items) which are always accurate
    -- (tiles can't be invisible).
    tgt <- getsState $ posToActor tpos arena
    case tgt of
      Nothing -> do  -- move or search or alter
        -- Start running in the given direction. The first turn of running
        -- succeeds much more often than subsequent turns, because we ignore
        -- most of the disturbances, since the player is mostly aware of them
        -- and still explicitly requests a run, knowing how it behaves.
        runStopOrCmd <- moveRunAid leader dir
        case runStopOrCmd of
          Left stopMsg -> abortWith stopMsg
          Right runCmd -> do
            sel <- getsClient sselected
            let runMembers = ES.toList (ES.delete leader sel) ++ [leader]
                runParams = RunParams { runLeader = leader
                                      , runMembers
                                      , runDist = 0
                                      , runStopMsg = Nothing
                                      , runInitDir = Just dir }
            when run $ modifyClient $ \cli -> cli {srunning = Just runParams}
            return $ Right $ TakeTimeSer runCmd
        -- When running, the invisible actor is hit (not displaced!),
        -- so that running in the presence of roving invisible
        -- actors is equivalent to moving (with visible actors
        -- this is not a problem, since runnning stops early enough).
        -- TODO: stop running at invisible actor
      Just target | run ->
        -- Displacing requires accessibility, but it's checked later on.
        fmap (fmap TakeTimeSer) $ displaceAid leader target
      Just target -> do
        tb <- getsState $ getActorBody target
        -- We always see actors from our own faction.
        if bfid tb == bfid sb && not (bproj tb) then do
          -- Select adjacent actor by bumping into him. Takes no time.
          success <- pickLeader target
          assert (success `blame` "bump self"
                          `twith` (leader, target, tb)) skip
          return (Left "")
        else
          -- Attacking does not require full access, adjacency is enough.
          fmap (fmap TakeTimeSer) $ meleeAid leader target

projectHuman :: (MonadClientAbort m, MonadClientUI m)
             => [Trigger] -> WriterT Slideshow m (Abort CmdSer)
projectHuman ts = do
  tgtLoc <- targetToPos
  if isNothing tgtLoc
    then retargetLeader >> return (Left "")
    else do
      leader <- getLeaderUI
      fmap (fmap TakeTimeSer) $ projectAid leader ts

tgtFloorHuman :: MonadClientUI m => WriterT Slideshow m (Abort CmdSer)
tgtFloorHuman = do
  arena <- getArenaUI
  tgtFloorLeader (TgtExplicit arena) >> return (Left "")

tgtEnemyHuman :: MonadClientUI m => WriterT Slideshow m (Abort CmdSer)
tgtEnemyHuman = do
  arena <- getArenaUI
  tgtEnemyLeader (TgtExplicit arena) >> return (Left "")
