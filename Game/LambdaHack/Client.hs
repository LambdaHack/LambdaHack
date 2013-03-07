-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdClientAISem, cmdClientUISem
  , loopAI, loopUI, executorCli, exeFrontend
  , MonadClient, MonadClientUI, MonadClientConn
  ) where

import Control.Monad
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdAtomicCli
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Utils.Assert

cmdClientAISem :: ( MonadAction m
                  , MonadClient m, MonadClientConn c m )
               => CmdClientAI -> m ()
cmdClientAISem cmd = case cmd of
  CmdAtomicAI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ cmdAtomicSem cmds
  CmdQueryAI aid -> do
    cmds <- queryAI aid
    writeConnFromClient cmds

cmdClientUISem :: ( MonadActionAbort m, MonadAction m
                  , MonadClientUI m, MonadClientConn c m )
               => CmdClientUI -> m ()
cmdClientUISem cmd = do
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
    CmdQueryUI aid -> do
      assert (isJust mleader `blame` cmd) skip
      cmdH <- queryUI aid
      writeConnFromClient cmdH
