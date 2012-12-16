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
import Data.Text (Text)

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
cmdAction ::  MonadAction m => Cmd -> WriterT Frames m ()
cmdAction cmd = case cmd of
  Apply{..}       -> lift $ playerApplyGroupItem verb object syms
  Project{..}     -> playerProjectGroupItem verb object syms
  TriggerDir{..}  -> lift $ playerTriggerDir feature verb
  TriggerTile{..} -> lift $ playerTriggerTile feature
  Pickup    -> lift $ pickupItem
  Drop      -> lift $ dropItem
  Wait      -> lift $ waitBlock
  GameExit  -> lift $ gameExit
  GameRestart -> lift $ gameRestart
  GameSave  -> lift $ gameSave

  Inventory -> inventory
  TgtFloor  -> targetFloor   TgtExplicit
  TgtEnemy  -> targetMonster TgtExplicit
  TgtAscend k -> tgtAscend k
  EpsIncr b -> lift $ epsIncr b
  Cancel    -> cancelCurrent displayMainMenu
  Accept    -> acceptCurrent displayHelp
  Clear     -> lift $ clearCurrent
  History   -> displayHistory
  CfgDump   -> lift $ dumpConfig
  HeroCycle -> lift $ cycleHero
  HeroBack  -> lift $ backCycleHero
  Help      -> displayHelp

-- | The list of semantics and other info for all commands from config.
semanticsCmds :: MonadAction m => [(K.Key, Cmd)]
              -> [((K.Key, K.Modifier), (Text, Bool, WriterT Frames m ()))]
semanticsCmds cmdList =
  let mkDescribed cmd =
        let semantics = if timedCmd cmd
                        then checkCursor $ cmdAction cmd
                        else cmdAction cmd
        in (cmdDescription cmd, timedCmd cmd, semantics)
      mkCommand (key, def) = ((key, K.NoModifier), mkDescribed def)
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
              => ConfigUI   -- ^ game config
              -> M.Map (K.Key, K.Modifier) (Text, Bool, WriterT Frames m ())
actionBinding !config =
  let cmdList = configCmds config
      semList = semanticsCmds cmdList
      moveWidth f = do
        lxsize <- gets (lxsize . slevel)
        move $ f lxsize
      runWidth f = do
        lxsize <- gets (lxsize . slevel)
        run (f lxsize, 0)
      -- Targeting cursor movement and others are wrongly marked as timed;
      -- fixed in their definitions by rewinding time.
      cmdDir = K.moveBinding moveWidth runWidth
  in M.fromList $
       cmdDir ++
       heroSelection ++
       semList ++
       [ -- Debug commands.
         ((K.Char 'r', K.Control), ("", False, modify cycleMarkVision)),
         ((K.Char 'o', K.Control), ("", False, modify toggleOmniscient)),
         ((K.Char 'i', K.Control), ("", False, gets (lmeta . slevel)
                                               >>= abortWith))
       ]

heroSelection :: MonadAction m
              => [((K.Key, K.Modifier), (Text, Bool, WriterT Frames m ()))]
heroSelection =
  let select k = do
        s <- get
        case tryFindHeroK s k of
          Nothing  -> abortWith "No such member of the party."
          Just aid -> void $ selectPlayer aid
      heroSelect k = ( (K.Char (Char.intToDigit k), K.NoModifier)
                     , ("", False, select k)
                     )
  in fmap heroSelect [0..9]
