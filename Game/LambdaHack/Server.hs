-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, execWriterT)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdAtomic
import Game.LambdaHack.Server.CmdAtomicSem
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: MonadServerChan m => CmdSer -> m ()
cmdSerSem cmd = do
  cmds <- execWriterT $ cmdSerWriterT cmd
  mapM_ cmdAtomicSem cmds

cmdSerWriterT :: MonadServerChan m => CmdSer -> WriterT [CmdAtomic] m ()
cmdSerWriterT cmd = case cmd of
  ApplySer aid v iid container -> applySer aid v iid container
  ProjectSer aid p eps v iid container -> projectSer aid p eps v iid container
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
--    bitems <- getsState $ getActorBag actor
--    Actor{bpos} <- getsState (getActorBody actor)
--    modifyState (updateArena (dropItemsAt bitems bpos))
    modifyState (deleteActor actor)
