-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSer
  , loopSer, executorSer, connServer
  ) where

import Control.Monad

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Level
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.SemAction
import Game.LambdaHack.Server.LoopAction
import Game.LambdaHack.State

-- | The semantics of server commands.
cmdSer :: MonadServerChan m => CmdSer -> m ()
cmdSer cmd = case cmd of
  ApplySer aid v item -> applySer aid v item
  ProjectSer aid p eps v i -> projectSer aid p eps v i
  TriggerSer aid p -> triggerSer aid p
  PickupSer aid i l -> pickupSer aid i l
  DropSer aid item -> dropSer aid item
  WaitSer aid -> waitSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  GameExitSer -> gameExitSer
  GameRestartSer -> gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer
  DirToAction actor allowAttacks dir -> do
    modifyState $ updateActorBody actor $ \ m -> m { bdirAI = Just (dir, 0) }
    if allowAttacks
      then moveSer actor dir
      else runSer actor dir
  ClearPath aid ->
    modifyState $ updateActorBody aid $ \m -> m {bpath = Nothing}
  FollowPath aid dir path darken -> do
    modifyState $ updateActorBody aid $ \m -> m {bpath = Just path}
    when darken $
      modifyState $ updateActorBody aid $ \m -> m {bcolor = Just Color.BrBlack}
    moveSer aid dir
  DieSer actor -> do  -- TODO: explode if a potion
    bitems <- getsState (getActorItem actor)
    Actor{bpos} <- getsState (getActorBody actor)
    modifyState (updateArena (dropItemsAt bitems bpos))
    modifyState (deleteActor actor)
