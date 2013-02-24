-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad.Writer.Strict (WriterT, execWriterT)

import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: MonadServer m => CmdSer -> m [Atomic]
cmdSerSem cmd = execWriterT $ cmdSerWriterT cmd

cmdSerWriterT :: MonadServer m => CmdSer -> WriterT [Atomic] m ()
cmdSerWriterT cmd = case cmd of
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  WaitSer aid -> waitSer aid
  PickupSer aid i k l -> pickupSer aid i k l
  DropSer aid iid -> dropSer aid iid
  ProjectSer aid p eps iid container -> projectSer aid p eps iid container
  ApplySer aid iid container -> applySer aid iid container
  TriggerSer aid p -> triggerSer aid p
  SetPathSer aid path -> setPathSer aid path
  GameRestartSer aid -> gameRestartSer aid
  GameExitSer _ -> gameExitSer
  GameSaveSer _ -> gameSaveSer
  CfgDumpSer aid -> cfgDumpSer aid
