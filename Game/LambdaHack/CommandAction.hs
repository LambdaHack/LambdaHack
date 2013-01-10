{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics, cmdSer
  ) where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict (WriterT, lift)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.ClientAction
import Game.LambdaHack.Command
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.MixedAction
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.ServerAction
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadAction m => StateClient -> State -> Cmd
          -> (Bool, WriterT Slideshow m ())
cmdAction cli s cmd =
  let tgtMode = stgtMode cli
      leader = sleader s
      arena = sarena s
      sm = getLeaderBody s
      ppos = bpos sm
      tgtLoc = targetToPos cli s
      Level{lxsize} =
        case tgtMode of
          TgtOff -> getArena s
          _ -> (sdungeon s M.! tgtLevelId tgtMode)
  in case cmd of
    Apply{..} -> (True, cmdSerAction $ playerApplyGroupItem verb object syms)
    Project{} | isNothing tgtLoc -> (False, retarget)
    Project{..} ->
      (True, cmdSerAction $ playerProjectGroupItem verb object syms)
    TriggerDir{..} -> (True, cmdSerAction $ playerTriggerDir feature verb)
    TriggerTile{..} -> (True, cmdSerAction $ playerTriggerTile feature)
    Pickup -> (True, cmdSerAction $ pickupItem)
    Drop   -> (True, cmdSerAction $ dropItem)
    Wait   -> (True, cmdSerAction $ waitBlock)
    Move v | tgtMode /= TgtOff ->
      let dir = toDir lxsize v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir lxsize v
          tpos = ppos `shift` dir
          -- We always see actors from our own faction.
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActorBody target s) == sside s
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent actor by bumping into him. Takes no time.
          (False,
           selectLeader arena target
             >>= assert `trueM`
                   (leader, target, "leader bumps into himself" :: Text))
        _ -> (True, cmdSerAction $ movePl dir)
    Run v | tgtMode /= TgtOff ->
      let dir = toDir lxsize v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir lxsize v
      in (True, cmdSerAction $ runPl dir)
    GameExit    -> (True, cmdSerAction $ gameExit)     -- rewinds time
    GameRestart -> (True, cmdSerAction $ gameRestart)  -- resets state

    GameSave    -> (False, cmdSerAction $ gameSave)
    Inventory   -> (False, inventory)
    TgtFloor    -> (False, targetFloor   $ TgtExplicit arena)
    TgtEnemy    -> (False, targetMonster $ TgtExplicit arena)
    TgtAscend k -> (False, tgtAscend k)
    EpsIncr b   -> (False, lift $ epsIncr b)
    Cancel      -> (False, cancelCurrent displayMainMenu)
    Accept      -> (False, acceptCurrent displayHelp)
    Clear       -> (False, lift $ clearCurrent)
    History     -> (False, displayHistory)
    CfgDump     -> (False, cmdSerAction $ dumpConfig)
    HeroCycle   -> (False, lift $ cycleHero)
    HeroBack    -> (False, lift $ backCycleHero)
    Help        -> (False, displayHelp)
    SelectHero k -> (False, lift $ selectHero k)
    DebugArea   -> (False, modifyClient toggleMarkVision)
    DebugOmni   -> (False, modifyClient toggleOmniscient)  -- TODO: Server
    DebugSmell  -> (False, modifyClient toggleMarkSmell)
    DebugVision -> (False, modifyServer cycleTryFov)

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadAction m => Cmd -> WriterT Slideshow m Bool
cmdSemantics cmd = do
  arenaOld <- getsGlobal sarena
  posOld <- getsGlobal (bpos . getLeaderBody)
  cli <- getClient
  loc <- getLocal
  let (timed, sem) = cmdAction cli loc cmd
  if timed
    then checkCursor sem
    else sem
  leader <- getsLocal sleader
  arena <- getsLocal sarena
  modifyGlobal $ updateSelected leader arena
  pos <- getsGlobal (bpos . getLeaderBody)
  tgtMode <- getsClient stgtMode
  when (tgtMode == TgtOff  -- targeting performs it's own, more extensive look
        && (posOld /= pos
            || arenaOld /= arena)) $ do
    lookMsg <- lookAt False True pos ""
    msgAdd lookMsg
  return timed

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionRO m => WriterT Slideshow m () -> WriterT Slideshow m ()
checkCursor h = do
  arena <- getsLocal sarena
  (lid, _) <- viewedLevel
  if arena == lid
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

-- TODO: make it MonadServer
-- | The semantics of server commands.
cmdSer :: MonadAction m => CmdSer -> m ()
cmdSer cmd = case cmd of
  ApplySer aid v item -> applySer aid v item
  ProjectSer aid p v i -> projectSer aid p v i
  TriggerSer p -> triggerSer p
  PickupSer aid i l -> pickupSer aid i l >>= sendToPlayers
  DropSer aid item -> dropSer aid item
  WaitSer aid -> waitSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  GameExitSer -> gameExitSer
  GameRestartSer -> gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer

-- | The semantics of client commands.
cmdCli :: MonadClient m => CmdCli -> m ()
cmdCli cmd = case cmd of
  PickupCli aid i ni -> pickupCli aid i ni

cmdSerAction :: MonadAction m => m CmdSer -> WriterT Slideshow m ()
cmdSerAction m = lift $ m >>= cmdSer

sendToPlayers :: MonadAction m => (Point, CmdCli) -> m ()
sendToPlayers (pos, cmd) = do
  arena <- getsGlobal sarena
  glo <- getGlobal
  let f (fid, perF) = when (isPlayerFaction glo fid) $ do
        let perceived = pos `IS.member` totalVisible (perF M.! arena)
        when perceived $ sendToClient fid cmd
  pers <- ask
  mapM_ f $ IM.toList pers

sendToClient :: MonadAction m => FactionId -> CmdCli -> m ()
sendToClient fid cmd = do
  side <- getsGlobal sside
  switchGlobalSelectedSide fid
  cmdCli cmd
  switchGlobalSelectedSide side
