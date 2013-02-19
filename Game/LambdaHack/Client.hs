-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdCliSem, cmdUISem
  , loopCli, loopUI, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Control.Monad

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdAtomicUI
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli

cmdCliSem :: (MonadAction m, MonadClientChan m) => CmdCli -> m ()
cmdCliSem cmd2 = case cmd2 of
  CmdUpdateCli cmd -> cmdUpdateCli cmd
  CmdHandleAICli aid -> do
    a <- handleAI aid
    writeChanToSer a

cmdUISem :: (MonadActionAbort m, MonadAction m
            , MonadClientUI m, MonadClientChan m)
         => CmdUI -> m ()
cmdUISem cmd2 = case cmd2 of
  CmdUpdateUI cmd -> cmdUpdateUI cmd
  CmdHandleHumanUI aid -> do
    a <- handleHuman aid
    writeChanToSer a

-- TODO: show that all but CmdAtomicCli do not use MonadAction
cmdUpdateCli :: (MonadAction m, MonadClient m) => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  CmdAtomicCli cmdA -> cmdAtomicCli cmdA
  RestartCli sper locRaw -> restartCli sper locRaw
  ContinueSavedCli sfper -> modifyClient $ \cli -> cli {sfper}
  GameSaveBkpCli -> clientGameSave True
  GameDisconnectCli -> clientDisconnect

cmdUpdateUI :: (MonadAction m, MonadClientUI m) => CmdUpdateUI -> m ()
cmdUpdateUI cmd = case cmd of
  CmdAtomicUI cmdA -> cmdAtomicUI cmdA
  DescAtomicUI desc -> descAtomicUI desc
  ShowMsgUI msg -> msgAdd msg
  DisplayPushUI -> displayPush
  DisplayDelayUI -> displayFramesPush [Nothing]
  MoreBWUI msg -> do
    void $ displayMore ColorBW msg
    recordHistory
  MoreFullUI msg -> do
    void $ displayMore ColorFull msg
    recordHistory
  FlushFramesUI -> do
    srunning <- getsClient srunning
    case srunning of
      Just (_, k) | k > 1 -> return ()
      _ -> do
        displayPush
        flushFrames
