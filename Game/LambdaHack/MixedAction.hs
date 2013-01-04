{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.MixedAction where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action hiding (MonadAction, MonadActionRO, MonadServer,
                               MonadServerRO)
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Binding
import Game.LambdaHack.ClientAction
import Game.LambdaHack.Command
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Running
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

-- + Semantics of client commands that return a server command

-- ** Two commands that do not take time, but still return a server command

-- *** GameSave

gameSave :: MonadClient m => m CmdSer
gameSave = do
  msgAdd "Saving game to a backup file."
  -- Let the server save, while the client continues taking commands.
  return GameSaveSer

-- *** CfgDump

dumpConfig :: MonadClient m => m CmdSer
dumpConfig = return CfgDumpSer

-- ** Apply

playerApplyGroupItem :: MonadClient m => MU.Part -> MU.Part -> [Char]
                     -> m CmdSer
playerApplyGroupItem verb object syms = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsLocal scops
  is <- getsLocal getPlayerItem
  item <- getGroupItem is object syms
            (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
  pl <- getsLocal splayer
  disco <- getsLocal sdisco
  let verbApply = case jkind disco item of
        Nothing -> verb
        Just ik -> iverbApply $ okind ik
  applyGroupItem pl verbApply item

applyGroupItem :: MonadClient m
               => ActorId  -- ^ actor applying the item (is on current level)
               -> MU.Part  -- ^ how the applying is called
               -> Item     -- ^ the item to be applied
               -> m CmdSer
applyGroupItem actor verb item = do
  Kind.COps{coactor, coitem} <- getsLocal scops
  body <- getsLocal (getActor actor)
  per <- askPerception
  disco <- getsLocal sdisco
  -- Only one item consumed, even if several in inventory.
  let consumed = item { jcount = 1 }
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor body) verb
        , partItemNWs coitem disco consumed ]
      pos = bpos body
  when (pos `IS.member` totalVisible per) $ msgAdd msg
  return $! ApplySer actor item pos

-- ** Project
--playerProjectGroupItem
-- ** TriggerDir
--playerTriggerDir
-- ** TriggerTile
--playerTriggerTile
-- ** Pickup
--pickupItem
-- ** Drop

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.
-- | Drop a single item.
dropItem :: MonadClient m => m CmdSer
dropItem = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coactor, coitem} <- getsLocal scops
  pl    <- getsLocal splayer
  pbody <- getsLocal getPlayerBody
  ims   <- getsLocal getPlayerItem
  stack <- getAnyItem "What to drop?" ims "in inventory"
  disco <- getsLocal sdisco
  let item = stack { jcount = 1 }
  msgAdd $ makeSentence
    [ MU.SubjectVerbSg (partActor coactor pbody) "drop"
    , partItemNWs coitem disco item ]
  return $ DropSer pl item

-- ** Wait
--waitBlock
-- ** Move
--actorMove
-- ** Run

runPl :: MonadClient m => Vector -> m CmdSer
runPl dir = do
  pl <- getsLocal splayer
  dirR <- runDir (dir, 0)
  return $! MoveSer pl dirR

-- ** GameExit
--gameExit
-- ** GameRestart
--gameRestart
