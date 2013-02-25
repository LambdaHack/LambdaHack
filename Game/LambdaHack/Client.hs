-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdCliSem, cmdUISem
  , loopCli, loopUI, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Control.Monad
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdAtomicUI
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli

cmdCliSem :: (MonadAction m, MonadClientChan m) => CmdCli -> m ()
cmdCliSem cmd = case cmd of
  CmdAtomicCli cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ cmdAtomicSem cmds
  ContinueSavedCli sfper -> modifyClient $ \cli -> cli {sfper}
  GameSaveBkpCli -> clientGameSave True
  GameDisconnectCli -> clientDisconnect
  CmdHandleAICli aid -> do
    cmds <- handleAI aid
    writeChanToSer cmds

cmdUISem :: ( MonadActionAbort m, MonadAction m
            , MonadClientUI m, MonadClientChan m )
         => CmdUI -> m ()
cmdUISem cmd = case cmd of
  CmdAtomicUI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ cmdAtomicSem cmds
    mleader <- getsClient sleader
    when (isJust mleader) $
      mapM_ (drawCmdAtomicUI False) cmds
  DescAtomicUI desc -> do
    mleader <- getsClient sleader
    when (isJust mleader) $
      drawDescAtomicUI False desc
  CmdHandleHumanUI aid -> do
    cmdH <- handleHuman aid
    writeChanToSer [cmdH]
