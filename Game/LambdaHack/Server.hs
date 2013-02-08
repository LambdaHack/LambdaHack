-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad.Writer.Strict (WriterT, execWriterT)

import Game.LambdaHack.Action
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdAtomic
import Game.LambdaHack.Server.CmdAtomicSem
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: (MonadAction m, MonadServerChan m) => CmdSer -> m ()
cmdSerSem cmd = do
  cmds <- execWriterT $ cmdSerWriterT cmd
  let process cmd1 = do
        cmdAtomicBroad cmd1
        cmdAtomicSem cmd1
  mapM_ process cmds

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
  GameExitSer -> undefined -- gameExitSer
  GameRestartSer -> undefined -- gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer
  ClearPathSer aid -> clearPathSer aid
  SetPathSer aid dir path -> setPathSer aid dir path
  DieSer aid -> dieSer aid
