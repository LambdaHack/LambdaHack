{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdUpdateCli, cmdUpdateUI, cmdQueryCli, cmdQueryUI
  , loopCli2, loopCli4, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Faction
import Game.LambdaHack.Msg
import Game.LambdaHack.State

cmdUpdateCli :: (MonadAction m, MonadClient m) => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  ShowMsgCli msg -> msgAdd msg
  RememberCli lvl lid actorD itemD faction ->
    rememberCli lvl lid actorD itemD faction
  RememberPerCli per lvl lid actorD itemD faction ->
    rememberPerCli per lvl lid actorD itemD faction
  RestartCli sper locRaw -> restartCli sper locRaw
  ContinueSavedCli sper -> modifyClient $ \cli -> cli {sper}
  GameSaveBkpCli isAI -> clientGameSave True isAI
  GameDisconnectCli isAI -> clientDisconnect isAI
  AtomicSeen catomic -> atomicSeen catomic

cmdUpdateUI :: (MonadAction m, MonadClientUI m) => CmdUpdateUI -> m ()
cmdUpdateUI cmd = case cmd of
  DisplayPushCli -> displayPush
  DisplayDelayCli -> displayFramesPush [Nothing]
  MoreBWCli msg -> do
    void $ displayMore ColorBW msg
    recordHistory
  MoreFullCli msg -> do
    void $ displayMore ColorFull msg
    recordHistory
  AtomicSeenUI catomic -> atomicSeenUI catomic

cmdQueryCli :: MonadClient m => CmdQueryCli a -> m a
cmdQueryCli cmd = case cmd of
  NullReportCli -> do
    StateClient{sreport} <- getClient
    return $! nullReport sreport
  HandleAI actor -> handleAI actor
  IsRunningCli -> do
    tryWith (\_ -> return False) $ do
      mleader <- getsClient sleader
      leader <- maybe abort return mleader
      srunning <- getsClient srunning
      maybe abort (void . continueRunDir leader) srunning
      return True

cmdQueryUI :: MonadClientUI m => CmdQueryUI a -> m a
cmdQueryUI cmd = case cmd of
  ShowSlidesCli slides -> getManyConfirms [] slides
  ConfirmMoreBWCli msg -> do
    go <- displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  HandleHumanCli -> handleHuman
  FlushFramesCli newSide -> do
    srunning <- getsClient srunning
    case srunning of
      Just (_, k) | k > 1 -> return False
      _ -> do
        faction <- getsState sfaction
        let factionName = gname $ faction EM.! newSide
            msg = "Switching to player" <+> factionName <> "."
        void $ displayMore ColorFull msg
        -- Messages shown, so update history and reset current report.
        recordHistory
        return True
