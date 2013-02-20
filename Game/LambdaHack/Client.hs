-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdCliSem, cmdUISem
  , loopCli, loopUI, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdAtomicUI
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli

cmdCliSem :: (MonadAction m, MonadClientChan m) => CmdCli -> m ()
cmdCliSem cmd2 = case cmd2 of
  CmdAtomicCli cmdA -> cmdAtomicCli cmdA
  ContinueSavedCli sfper -> modifyClient $ \cli -> cli {sfper}
  GameSaveBkpCli -> clientGameSave True
  GameDisconnectCli -> clientDisconnect
  CmdHandleAICli aid -> do
    a <- handleAI aid
    writeChanToSer a

cmdUISem :: ( MonadActionAbort m, MonadAction m
            , MonadClientUI m, MonadClientChan m )
         => CmdUI -> m ()
cmdUISem cmd2 = case cmd2 of
  CmdAtomicUI cmdA -> cmdAtomicUI cmdA
  DescAtomicUI desc -> descAtomicUI desc
  DisplayPushUI -> displayPush
  DisplayDelayUI -> displayFramesPush [Nothing]
  FlushFramesUI -> do
    srunning <- getsClient srunning
    case srunning of
      Just (_, k) | k > 1 -> return ()
      _ -> do
        displayPush
        flushFrames
  CmdHandleHumanUI aid -> do
    a <- handleHuman aid
    writeChanToSer a
