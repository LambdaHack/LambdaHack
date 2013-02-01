-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSer
  , loopSer, executorSer, waitForChildren, speedupCOps
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
  ApplySer aid v iid -> applySer aid v iid
  ProjectSer aid p eps v iid -> projectSer aid p eps v iid
  TriggerSer aid p -> triggerSer aid p
  PickupSer aid i k l -> pickupSer aid i k l
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
    bitems <- getsState $ getActorBag actor
    Actor{bpos} <- getsState (getActorBody actor)
    modifyState (updateArena (dropItemsAt bitems bpos))
    modifyState (deleteActor actor)
