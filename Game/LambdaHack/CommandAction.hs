{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics, cmdSer
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift)
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.ClientAction
import Game.LambdaHack.Command
import Game.LambdaHack.Level
import Game.LambdaHack.MixedAction
import Game.LambdaHack.Msg
import Game.LambdaHack.ServerAction
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadAction m => StateClient -> State -> Cmd
          -> (Bool, WriterT Slideshow m ())
cmdAction cli s cmd =
  let targeting = stgtMode cli
      pl = splayer s
      arena = sarena s
      sm = getPlayerBody s
      ppos = bpos sm
      tgtLoc = targetToPos cli s
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
    Move v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir (lxsize (getArena s)) v
          tpos = ppos `shift` dir
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActorBody target s) == sside s
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent hero by bumping into him. Takes no time.
          (False,
           selectPlayer arena target
             >>= assert `trueM` (pl, target, "player bumps himself" :: Text))
        _ -> (True, cmdSerAction $ movePl dir)
    Run v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir (lxsize (getArena s)) v
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
  cli <- getClient
  loc <- getLocal
  posOld <- getsLocal (bpos . getPlayerBody)
  let (timed, sem) = cmdAction cli loc cmd
  posNew <- getsLocal (bpos . getPlayerBody)
  when (posOld /= posNew) $ do
    lookMsg <- lookAt False True posNew ""
    msgAdd lookMsg
  -- TODO: verify the invariant
  State{splayer, sarena} <- getLocal
  modifyGlobal (\s -> s {splayer})
  modifyGlobal (\s -> s {sarena})
  if timed
    then checkCursor sem
    else sem
  return timed

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionRO m => WriterT Slideshow m () -> WriterT Slideshow m ()
checkCursor h = do
  sarena <- getsLocal sarena
  if sarena == sarena  -- TODO: use the Tgt level
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

-- TODO: make it MonadServer
-- | The semantics of server commands.
cmdSer :: MonadAction m => CmdSer -> m ()
cmdSer cmd = case cmd of
  ApplySer aid item pos -> applySer aid item pos
  ProjectSer aid p v i -> projectSer aid p v i
  TriggerSer p -> triggerSer p
  PickupSer aid i l -> pickupSer aid i l
  DropSer aid item -> dropSer aid item
  WaitSer aid -> waitSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  GameExitSer -> gameExitSer
  GameRestartSer -> gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer

cmdSerAction :: MonadAction m => m CmdSer -> WriterT Slideshow m ()
cmdSerAction m = lift $ m >>= cmdSer
