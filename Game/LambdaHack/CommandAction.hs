{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( semanticsCmds, actionBinding
  ) where

import Control.Monad
import Control.Monad.State hiding (State, get, gets, state)
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map as M

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (Frames)
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import Game.LambdaHack.EffectAction
import Game.LambdaHack.ItemAction
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Level
import Game.LambdaHack.Running
import Game.LambdaHack.State

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdAction ::  MonadAction m => State -> Cmd -> (Bool, WriterT Frames m ())
cmdAction s cmd = case cmd of
  Apply{..}       -> (True, lift $ playerApplyGroupItem verb object syms)
  Project{..}     -> (True, playerProjectGroupItem verb object syms)
  TriggerDir{..}  -> (True, lift $ playerTriggerDir feature verb)
  TriggerTile{..} -> (True, lift $ playerTriggerTile feature)
  Pickup      -> (True, lift $ pickupItem)
  Drop        -> (True, lift $ dropItem)
  Wait        -> (True, lift $ waitBlock)
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

-- | The list of semantics and other info for all commands from config.
semanticsCmds :: MonadAction m => State -> [(K.Key, Cmd)]
              -> [((K.Key, K.Modifier), (Bool, WriterT Frames m ()))]
semanticsCmds s cmdList =
  let mkSem cmd =
        let (timed, sem) = cmdAction s cmd
            semantics = if timed
                        then checkCursor sem
                        else sem
        in (timed, semantics)
      mkCommand (key, def) = ((key, K.NoModifier), mkSem def)
  in L.map mkCommand cmdList

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionRO m => WriterT Frames m () -> WriterT Frames m ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "[targeting] you inspect a remote level, press ESC to switch back"



-- TODO: clean up: probably add Command.Cmd for each operation

actionBinding :: MonadAction m
              => State -> ConfigUI -> (K.Key, K.Modifier)
              -> Maybe (Bool, WriterT Frames m ())
actionBinding s !config km =
  let cmdList = configCmds config
      semList = semanticsCmds s cmdList
      moveWidth f = do
        lxsize <- gets (lxsize . slevel)
        move $ f lxsize
      runWidth f = do
        lxsize <- gets (lxsize . slevel)
        run (f lxsize, 0)
      -- Targeting cursor movement and others are wrongly marked as timed;
      -- fixed in their definitions by rewinding time.
      cmdDir = K.moveBinding moveWidth runWidth
  in M.lookup km $ M.fromList $
       cmdDir ++
       heroSelection ++
       semList ++
       [ -- Debug commands.
         ((K.Char 'r', K.Control), (False, modify cycleMarkVision)),
         ((K.Char 'o', K.Control), (False, modify toggleOmniscient)),
         ((K.Char 'i', K.Control), (False, gets (lmeta . slevel)
                                               >>= abortWith))
       ]

heroSelection :: MonadAction m
              => [((K.Key, K.Modifier), (Bool, WriterT Frames m ()))]
heroSelection =
  let select k = do
        s <- get
        case tryFindHeroK s k of
          Nothing  -> abortWith "No such member of the party."
          Just aid -> void $ selectPlayer aid
      heroSelect k = ( (K.Char (Char.intToDigit k), K.NoModifier)
                     , (False, select k)
                     )
  in fmap heroSelect [0..9]
