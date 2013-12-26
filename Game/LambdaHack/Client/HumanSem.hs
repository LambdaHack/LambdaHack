-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumSet as ES
import Data.Maybe
import Data.Monoid hiding ((<>))

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
cmdHumanSem :: MonadClientUI m
            => HumanCmd -> m (Abort CmdSer)
cmdHumanSem cmd = do
  arena <- getArenaUI
  if noRemoteHumanCmd cmd then do
    mmsg <- checkCursor arena
    case mmsg of
      Just msg -> failWith msg
      Nothing -> cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m
          => HumanCmd -> m (Abort CmdSer)
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

  PickLeader k -> convertMsg $ pickLeaderHuman k
  MemberCycle -> convertMsg memberCycleHuman
  MemberBack -> convertMsg memberBackHuman
  Inventory -> convertMsg inventoryHuman
  TgtFloor -> tgtFloorHuman
  TgtEnemy -> tgtEnemyHuman
  TgtAscend k -> convertMsg $ tgtAscendHuman k
  EpsIncr b -> convertMsg $ epsIncrHuman b
  SelectActor -> convertMsg selectActorHuman
  SelectNone -> addNoMsg selectNoneHuman
  Cancel -> convertMsg $ cancelHuman displayMainMenu
  Accept -> convertMsg $ acceptHuman helpHuman
  Clear -> addNoMsg clearHuman
  History -> convertMsg historyHuman
  MarkVision -> addNoMsg humanMarkVision
  MarkSmell -> addNoMsg humanMarkSmell
  MarkSuspect -> addNoMsg humanMarkSuspect
  Help -> convertMsg displayMainMenu

addNoMsg :: Monad m => m () -> m (Abort CmdSer)
addNoMsg cmdCli = cmdCli >> return (Left mempty)

convertMsg :: Monad m => m (Maybe Slideshow) -> m (Abort CmdSer)
convertMsg cmdCli = do
  mmsg <- cmdCli
  return . Left $ fromMaybe mempty mmsg

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadClientUI m => LevelId -> m (Maybe Msg)
checkCursor arena = do
  (lid, _) <- viewedLevel
  if (arena /= lid) then
    return $ Just "[targeting] command disabled on a remote level, press ESC to switch back"
  else return Nothing

moveRunHuman :: MonadClientUI m
             => Bool -> VectorXY -> m (Abort CmdSer)
moveRunHuman run v = do
  tgtMode <- getsClient stgtMode
  (arena, Level{lxsize}) <- viewedLevel
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  let dir = toDir lxsize v
  if isJust tgtMode then do
    moveCursor dir (if run then 10 else 1) >> return (Left mempty)
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
          Left stopMsg -> failWith stopMsg
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
          return (Left mempty)
        else
          -- Attacking does not require full access, adjacency is enough.
          fmap (fmap TakeTimeSer) $ meleeAid leader target

projectHuman :: MonadClientUI m
             => [Trigger] -> m (Abort CmdSer)
projectHuman ts = do
  tgtLoc <- targetToPos
  if isNothing tgtLoc
    then retargetLeader >> return (Left mempty)
    else do
      leader <- getLeaderUI
      fmap (fmap TakeTimeSer) $ projectAid leader ts

tgtFloorHuman :: MonadClientUI m => m (Abort CmdSer)
tgtFloorHuman = do
  arena <- getArenaUI
  tgtFloorLeader (TgtExplicit arena) >> return (Left mempty)

tgtEnemyHuman :: MonadClientUI m => m (Abort CmdSer)
tgtEnemyHuman = do
  arena <- getArenaUI
  tgtEnemyLeader (TgtExplicit arena) >> return (Left mempty)
