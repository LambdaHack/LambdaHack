-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdCliSem, cmdUISem
  , loopCli, loopUI, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Control.Monad

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
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

cmdUISem :: (MonadAction m, MonadClientUI m, MonadClientChan m)
         => CmdUI -> m ()
cmdUISem cmd2 = case cmd2 of
  CmdUpdateUI cmd -> cmdUpdateUI cmd
  CmdHandleHumanUI aid -> do
    a <- handleHuman aid
    writeChanToSer a

cmdUpdateCli :: (MonadAction m, MonadClient m) => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  CmdAtomicCli cmdA -> cmdAtomicCli cmdA
  ShowMsgCli msg -> msgAdd msg
  RememberCli lvl lid actorD itemD faction ->
    rememberCli lvl lid actorD itemD faction
  RememberPerCli per lvl lid actorD itemD faction ->
    rememberPerCli per lvl lid actorD itemD faction
  RestartCli sper locRaw -> restartCli sper locRaw
  ContinueSavedCli sper -> modifyClient $ \cli -> cli {sper}
  GameSaveBkpCli -> clientGameSave True
  GameDisconnectCli -> clientDisconnect

cmdUpdateUI :: (MonadAction m, MonadClientUI m) => CmdUpdateUI -> m ()
cmdUpdateUI cmd = case cmd of
  CmdAtomicUI cmdA -> cmdAtomicUI cmdA
  DescAtomicUI desc -> descAtomicUI desc
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
