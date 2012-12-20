{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics
  ) where

import Control.Monad.State hiding (State, get, gets, state)
import Control.Monad.Writer.Strict (WriterT)

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.Animation (Frames)
import Game.LambdaHack.Command
import Game.LambdaHack.EffectAction
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Level
import Game.LambdaHack.Running
import Game.LambdaHack.State
import Game.LambdaHack.Vector

-- | The basic component of a semantics of a command.
cmdAction ::  MonadAction m => State -> Cmd -> (Bool, WriterT Frames m ())
cmdAction s cmd = case cmd of
  Apply{..}       -> (True, lift $ playerApplyGroupItem verb object syms)
  Project{..}     -> (True, playerProjectGroupItem verb object syms)
  TriggerDir{..}  -> (True, lift $ playerTriggerDir feature verb)
  TriggerTile{..} -> (True, lift $ playerTriggerTile feature)
  Pickup      -> (True, lift $ pickupItem)
  Drop        -> (True, lift $ dropItem)
  Wait        -> (True, lift $ waitBlock)
  Move v      -> (True, move (toDir (lxsize (slevel s)) v))
  Run v       -> (True, run (toDir (lxsize (slevel s)) v, 0))
  GameExit    -> (True, lift $ gameExit)     -- takes time, then rewinds time
  GameRestart -> (True, lift $ gameRestart)  -- takes time, then resets state

  GameSave    -> (False, lift $ gameSave)
  Inventory   -> (False, inventory)
  TgtFloor    -> (False, targetFloor   TgtExplicit)
  TgtEnemy    -> (False, targetMonster TgtExplicit)
  TgtAscend k -> (False, tgtAscend k)
  EpsIncr b   -> (False, lift $ epsIncr b)
  Cancel      -> (False, cancelCurrent displayMainMenu)
  Accept      -> (False, acceptCurrent displayHelp)
  Clear       -> (False, lift $ clearCurrent)
  History     -> (False, displayHistory)
  CfgDump     -> (False, lift $ dumpConfig)
  HeroCycle   -> (False, lift $ cycleHero)
  HeroBack    -> (False, lift $ backCycleHero)
  Help        -> (False, displayHelp)
  SelectHero k -> (False, lift $ selectHero k)
  DebugVision -> (False, modify cycleMarkVision)
  DebugOmni   -> (False, modify toggleOmniscient)
  DebugCave   -> (False, gets (lmeta . slevel) >>= abortWith)

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadAction m => State -> Cmd
              -> (Bool, WriterT Frames m ())
cmdSemantics s cmd =
  let (timed, sem) = cmdAction s cmd
  in if timed
     then (timed, checkCursor sem)
     else (timed, sem)

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionRO m => WriterT Frames m () -> WriterT Frames m ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "[targeting] you inspect a remote level, press ESC to switch back"
