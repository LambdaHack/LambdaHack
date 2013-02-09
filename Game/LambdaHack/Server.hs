-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad.Writer.Strict (WriterT, execWriterT)

import Game.LambdaHack.Action
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: (MonadAction m, MonadServerChan m) => CmdSer -> m ()
cmdSerSem cmd = do
  cmds <- execWriterT $ cmdSerWriterT cmd
  mapM_ cmdAtomicBroad cmds

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
  GameRestartSer fid -> gameRestartSer fid
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer
  ClearPathSer aid -> clearPathSer aid
  SetPathSer aid dir path -> setPathSer aid dir path
  DieSer aid -> dieSer aid
