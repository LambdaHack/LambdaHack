{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of human player commands.
module Game.LambdaHack.Client.HumanSem
  ( cmdHumanSem
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.EnumMap.Strict as EM
import Data.Maybe

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanGlobal
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
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
  Move v -> moveHuman v
  Run v -> runHuman v
  Wait -> fmap Just waitHuman
  Pickup -> fmap Just pickupHuman
  Drop -> fmap Just dropHuman
  Project ts -> projectHuman ts
  Apply ts -> fmap Just $ applyHuman ts
  TriggerDir ts -> fmap Just $ triggerDirHuman ts
  TriggerTile ts -> fmap Just $ triggerTileHuman ts

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
  MarkVision -> modifyClient toggleMarkVision >> return Nothing
  MarkSmell -> modifyClient toggleMarkSmell >> return Nothing
  MarkSuspect -> modifyClient toggleMarkSuspect >> return Nothing
  Help -> displayMainMenu >> return Nothing

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: (MonadClientAbort m, MonadClientUI m) => LevelId -> m ()
checkCursor arena = do
  (lid, _) <- viewedLevel
  when (arena /= lid) $
    abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

moveHuman :: (MonadClientAbort m, MonadClientUI m)
          => VectorXY -> WriterT Slideshow m (Maybe CmdSer)
moveHuman v = do
  Kind.COps{cotile} <- getsState scops
  tgtMode <- getsClient stgtMode
  (arena, lvl@Level{lxsize}) <- viewedLevel
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  if isJust tgtMode then do
    let dir = toDir lxsize v
    moveCursor dir 1 >> return Nothing
  else do
    let dir = toDir lxsize v
        tpos = bpos sb `shift` dir
        t = lvl `at` tpos
    -- We always see actors from our own faction.
    tgt <- getsState $ posToActor tpos arena
    case tgt of
      Just target -> do
        tb <- getsState $ getActorBody target
        sfact <- getsState $ (EM.! bfid sb) . sfactionD
        if bfid tb == bfid sb && not (bproj tb) then do
          -- Select adjacent actor by bumping into him. Takes no time.
          success <- selectLeader target
          assert (success `blame` (leader, target, tb)) skip
          return Nothing
        else do
          unless (bproj tb || isAtWar sfact (bfid tb)) $ do
            go <- displayYesNo "This attack will start a war. Are you sure?"
            unless go $ abortWith "Attack canceled."
          when (not (bproj tb) && isAllied sfact (bfid tb)) $ do
            go <- displayYesNo "You are bound by an alliance. Really attack?"
            unless go $ abortWith "Attack canceled."
          fmap Just $ moveLeader dir
      _ -> fmap Just $
        if Tile.hasFeature cotile F.Suspect t
           || Tile.hasFeature cotile F.Openable t
        then -- Explore, because the suspect feature is not yet revealed
             -- or because we want to act on the feature.
             exploreLeader dir
        else -- Don't explore, because the suspect feature is known boring.
             moveLeader dir

runHuman :: MonadClientUI m => VectorXY -> WriterT Slideshow m (Maybe CmdSer)
runHuman v = do
  tgtMode <- getsClient stgtMode
  (_, Level{lxsize}) <- viewedLevel
  if isJust tgtMode then
    let dir = toDir lxsize v
    in moveCursor dir 10 >> return Nothing
  else
    let dir = toDir lxsize v
    in fmap Just $ runLeader dir

projectHuman :: (MonadClientAbort m, MonadClientUI m)
             => [Trigger] -> WriterT Slideshow m (Maybe CmdSer)
projectHuman ts = do
  tgtLoc <- targetToPos
  if isNothing tgtLoc
    then retargetLeader >> return Nothing
    else fmap Just $ projectLeader ts

tgtFloorHuman :: MonadClientUI m => WriterT Slideshow m (Maybe CmdSer)
tgtFloorHuman = do
  arena <- getArenaUI
  tgtFloorLeader (TgtExplicit arena) >> return Nothing

tgtEnemyHuman :: MonadClientUI m => WriterT Slideshow m (Maybe CmdSer)
tgtEnemyHuman = do
  arena <- getArenaUI
  tgtEnemyLeader (TgtExplicit arena) >> return Nothing
