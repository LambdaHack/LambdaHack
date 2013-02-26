-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdCliSem, cmdUISem
  , loopCli, loopUI, executorCli, exeFrontend
  , MonadClient, MonadClientUI, MonadClientChan
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
import Game.LambdaHack.Utils.Assert

cmdCliSem :: ( MonadAction m
             , MonadClient m, MonadClientChan c m )
          => CmdCli -> m ()
cmdCliSem cmd = case cmd of
  CmdAtomicCli cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ cmdAtomicSem cmds
  CmdHandleAICli aid -> do
    cmds <- handleAI aid
    writeChanToSer cmds

cmdUISem :: ( MonadActionAbort m, MonadAction m
            , MonadClientUI m, MonadClientChan c m )
         => CmdUI -> m ()
cmdUISem cmd = do
  mleader <- getsClient _sleader
  case cmd of
    CmdAtomicUI cmdA -> do
      cmds <- cmdAtomicFilterCli cmdA
      mapM_ cmdAtomicSemCli cmds
      mapM_ cmdAtomicSem cmds
      when (isJust mleader) $
        mapM_ (drawCmdAtomicUI False) cmds
    DescAtomicUI desc ->
      when (isJust mleader) $
        drawDescAtomicUI False desc
    CmdHandleHumanUI aid -> do
      assert (isJust mleader `blame` cmd) skip
      cmdH <- handleHuman aid
      writeChanToSer [cmdH]
