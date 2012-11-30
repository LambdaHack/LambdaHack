{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( configCmds, semanticsCmds
  ) where

import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ItemAction
import Game.LambdaHack.State
import Game.LambdaHack.Command
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Utils.Assert

-- | The semantics of player commands in terms of the @Action@ monad.
cmdAction :: Cmd -> ActionFrame ()
cmdAction cmd = case cmd of
  Apply{..}       -> inFrame $ playerApplyGroupItem verb object syms
  Project{..}     -> playerProjectGroupItem verb object syms
  TriggerDir{..}  -> inFrame $ playerTriggerDir feature verb
  TriggerTile{..} -> inFrame $ playerTriggerTile feature
  Pickup    -> inFrame $ pickupItem
  Drop      -> inFrame $ dropItem
  Wait      -> inFrame $ waitBlock
  GameExit  -> inFrame $ gameExit
  GameRestart -> inFrame $ gameRestart
  GameSave  -> inFrame $ gameSave

  Inventory -> inventory
  TgtFloor  -> targetFloor   TgtExplicit
  TgtEnemy  -> targetMonster TgtExplicit
  TgtAscend k -> tgtAscend k
  EpsIncr b -> inFrame $ epsIncr b
  Cancel    -> cancelCurrent displayMainMenu
  Accept    -> acceptCurrent displayHelp
  Clear     -> inFrame $ clearCurrent
  History   -> displayHistory
  CfgDump   -> inFrame $ dumpConfig
  HeroCycle -> inFrame $ cycleHero
  HeroBack  -> inFrame $ backCycleHero
  Help      -> displayHelp

-- | The associaction of commands to keys defined in config.
configCmds :: Config.CP -> [(K.Key, Cmd)]
configCmds config =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key <" ++ s ++ ">")
          key -> key
      mkCmd s = read s :: Cmd
      mkCommand (key, def) = (mkKey key, mkCmd def)
  in L.map mkCommand section

-- | The list of semantics and other info for all commands from config.
semanticsCmds :: [(K.Key, Cmd)]
              -> [((K.Key, K.Modifier), (Text, Bool, ActionFrame ()))]
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
checkCursor :: ActionFrame () -> ActionFrame ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "this command does not work on remote levels"
