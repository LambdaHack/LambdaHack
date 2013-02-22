-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad.Writer.Strict (WriterT, execWriterT)

import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Faction
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: MonadServer m => FactionId -> CmdSer -> m [Atomic]
cmdSerSem fid cmd = execWriterT $ cmdSerWriterT fid cmd

cmdSerWriterT :: MonadServer m => FactionId -> CmdSer -> WriterT [Atomic] m ()
cmdSerWriterT fid cmd = case cmd of
  DieSer aid -> dieSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  WaitSer aid -> waitSer aid
  PickupSer aid i k l -> pickupSer aid i k l
  DropSer aid iid -> dropSer aid iid
  ProjectSer aid p eps iid container -> projectSer aid p eps iid container
  ApplySer aid iid container -> applySer aid iid container
  TriggerSer aid p -> triggerSer aid p
  ClearPathSer aid -> clearPathSer aid
  SetPathSer aid path -> setPathSer aid path
  GameRestartSer -> gameRestartSer fid
  GameExitSer -> gameExitSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer fid
