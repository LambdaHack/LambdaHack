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
  let targeting = ctargeting (scursor cli)
      pl = splayer s
      sm = getActor pl s
      ppos = bpos sm
      tgtLoc = targetToPos cli s
  in case cmd of
    Apply{..} -> (True, cmdSerAction $ playerApplyGroupItem verb object syms)
    Project{} | isNothing tgtLoc -> (False, retarget)
    Project{..} -> (True, lift $ playerProjectGroupItem verb object syms)
    TriggerDir{..} -> (True, lift $ playerTriggerDir feature verb)
    TriggerTile{..} -> (True, lift $ playerTriggerTile feature)
    Pickup -> (True, lift $ pickupItem)
    Drop   -> (True, cmdSerAction $ dropItem)
    Wait   -> (True, lift $ waitBlock)
    Move v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir (lxsize (getArena s)) v
          tpos = ppos `shift` dir
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActor target s) == sside s
                      && not (bproj (getActor target s)) ->
          -- Select adjacent hero by bumping into him. Takes no time.
          (False,
           selectPlayer target
           >>= assert `trueM` (pl, target, "player bumps himself" :: Text))
        _ -> (True, lift $ actorAttack pl dir)
    Run v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir (lxsize (getArena s)) v
      in (True, cmdSerAction $ runPl dir)
    GameExit    -> (True, lift $ gameExit)     -- takes time, then rewinds time
    GameRestart -> (True, lift $ gameRestart)  -- takes time, then resets state

    GameSave    -> (False, cmdSerAction $ gameSave)
    Inventory   -> (False, inventory)
    TgtFloor    -> (False, targetFloor   TgtExplicit)
    TgtEnemy    -> (False, targetMonster TgtExplicit)
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
    locNew <- getLocal
    msgAdd $ lookAt False True locNew posNew ""
  -- TODO: verify the player belongs to the faction
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
  pl <- getsLocal splayer
  (creturnLn, _, _) <- getsLocal (findActorAnyLevel pl)
  sarena <- getsLocal sarena
  if creturnLn == sarena
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

-- TODO: make it MonadServer
-- | The semantics of server commands.
cmdSer :: MonadAction m => CmdSer -> m ()
cmdSer cmd = case cmd of
  ApplySer aid item pos -> applySer aid item pos
  ProjectSer -> undefined
  TriggerDirSer -> undefined
  TriggerTileSer -> undefined
  PickupSer -> undefined
  DropSer aid item -> dropSer aid item
  WaitSer -> undefined
  MoveSer aid dir -> moveSer aid dir
  RunSer -> undefined
  GameExitSer -> undefined
  GameRestartSer -> undefined
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer

cmdSerAction :: MonadAction m => m CmdSer -> WriterT Slideshow m ()
cmdSerAction m = lift $ m >>= cmdSer
