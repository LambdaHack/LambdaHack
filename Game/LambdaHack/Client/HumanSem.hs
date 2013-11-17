-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import Data.Maybe

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanGlobal
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Common.VectorXY
import Game.LambdaHack.Utils.Assert

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdHumanSem :: (MonadClientAbort m, MonadClientUI m)
            => HumanCmd -> WriterT Slideshow m (Maybe CmdSer)
cmdHumanSem cmd = do
  arena <- getArenaUI
  when (noRemoteHumanCmd cmd) $ checkCursor arena
  cmdAction cmd

-- | The basic action for a command and whether it takes time.
cmdAction :: (MonadClientAbort m, MonadClientUI m)
          => HumanCmd -> WriterT Slideshow m (Maybe CmdSer)
cmdAction cmd = case cmd of
  Move v -> moveRunHuman False v
  Run v -> moveRunHuman True v
  Wait -> fmap (Just . TakeTimeSer) waitHuman
  Pickup -> fmap (Just . TakeTimeSer) pickupHuman
  Drop -> fmap (Just . TakeTimeSer) dropHuman
  Project ts -> projectHuman ts
  Apply ts -> fmap (Just . TakeTimeSer) $ applyHuman ts
  AlterDir ts -> fmap (Just . TakeTimeSer) $ alterDirHuman ts
  TriggerTile ts -> fmap (Just . TakeTimeSer) $ triggerTileHuman ts

  GameRestart t -> fmap Just $ gameRestartHuman t
  GameExit -> fmap Just gameExitHuman
  GameSave -> fmap Just gameSaveHuman
  CfgDump -> fmap Just cfgDumpHuman

  SelectHero k -> selectHeroHuman k >> return Nothing
  MemberCycle -> memberCycleHuman >> return Nothing
  MemberBack -> memberBackHuman >> return Nothing
  Inventory -> inventoryHuman >> return Nothing
  TgtFloor -> tgtFloorHuman
  TgtEnemy -> tgtEnemyHuman
  TgtAscend k -> tgtAscendHuman k >> return Nothing
  EpsIncr b -> epsIncrHuman b >> return Nothing
  Cancel -> cancelHuman displayMainMenu >> return Nothing
  Accept -> acceptHuman helpHuman >> return Nothing
  Clear -> clearHuman >> return Nothing
  History -> historyHuman >> return Nothing
  MarkVision -> humanMarkVision >> return Nothing
  MarkSmell -> humanMarkSmell >> return Nothing
  MarkSuspect -> humanMarkSuspect >> return Nothing
  Help -> displayMainMenu >> return Nothing

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: (MonadClientAbort m, MonadClientUI m) => LevelId -> m ()
checkCursor arena = do
  (lid, _) <- viewedLevel
  when (arena /= lid) $
    abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

moveRunHuman :: (MonadClientAbort m, MonadClientUI m)
             => Bool -> VectorXY -> WriterT Slideshow m (Maybe CmdSer)
moveRunHuman run v = do
  tgtMode <- getsClient stgtMode
  (arena, Level{lxsize}) <- viewedLevel
  source <- getLeaderUI
  sb <- getsState $ getActorBody source
  let dir = toDir lxsize v
  if isJust tgtMode then do
    moveCursor dir (if run then 10 else 1) >> return Nothing
  else do
    let tpos = bpos sb `shift` dir
    -- We start by checking actors at the the target position,
    -- which gives a partial information (actors can be invisible),
    -- as opposed to accessibility (and items) which are always accurate
    -- (tiles can't be invisible).
    tgt <- getsState $ posToActor tpos arena
    case tgt of
      Nothing -> do  -- move or search or alter
        when run $ modifyClient $ \cli -> cli {srunning = Just (dir, 1)}
        fmap (Just . TakeTimeSer) $ moveRunAid source dir
        -- When running, the invisible actor is hit (not displaced!),
        -- so that running in the presence of roving invisible
        -- actors is equivalent to moving (with visible actors
        -- this is not a problem, since runnning stops early enough).
        -- TODO: stop running at invisible actor
      Just target | run ->
        -- Displacing requires accessibility, but it's checked later on.
        fmap (Just . TakeTimeSer) $ displaceAid source target
      Just target -> do
        tb <- getsState $ getActorBody target
        -- We always see actors from our own faction.
        if bfid tb == bfid sb && not (bproj tb) then do
          -- Select adjacent actor by bumping into him. Takes no time.
          success <- selectLeader target
          assert (success `blame` "bump self" `with` (source, target, tb)) skip
          return Nothing
        else
          -- Attacking does not require full access, adjacency is enough.
          fmap (Just . TakeTimeSer) $ meleeAid source target

projectHuman :: (MonadClientAbort m, MonadClientUI m)
             => [Trigger] -> WriterT Slideshow m (Maybe CmdSer)
projectHuman ts = do
  tgtLoc <- targetToPos
  if isNothing tgtLoc
    then retargetLeader >> return Nothing
    else do
      leader <- getLeaderUI
      fmap (Just . TakeTimeSer) $ projectAid leader ts

tgtFloorHuman :: MonadClientUI m => WriterT Slideshow m (Maybe CmdSer)
tgtFloorHuman = do
  arena <- getArenaUI
  tgtFloorLeader (TgtExplicit arena) >> return Nothing

tgtEnemyHuman :: MonadClientUI m => WriterT Slideshow m (Maybe CmdSer)
tgtEnemyHuman = do
  arena <- getArenaUI
  tgtEnemyLeader (TgtExplicit arena) >> return Nothing
