{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( configCmds, semanticsCmds
  ) where

import Control.Monad.IO.Class
import Control.Monad.Writer.Strict (WriterT, lift)
import qualified Data.List as L
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import Game.LambdaHack.ItemAction
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.State

-- | The semantics of player commands in terms of the @Action@ monad.
cmdAction ::  (MonadIO m, MonadAction m) => Cmd -> WriterT Frames m ()
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

-- | The associaction of commands to keys defined in config.
configCmds :: ConfigUI -> [(K.Key, Cmd)]
configCmds ConfigUI{configCommands} =
  let mkCommand (key, def) = (key, read def :: Cmd)
  in L.map mkCommand configCommands

-- | The list of semantics and other info for all commands from config.
semanticsCmds :: (MonadIO m, MonadAction m) => [(K.Key, Cmd)]
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
