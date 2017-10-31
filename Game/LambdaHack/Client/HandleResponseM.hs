{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  , handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response

class MonadClient m => MonadClientReadResponse m where
  receiveResponse :: m Response

class MonadClient m => MonadClientWriteRequest m where
  sendRequestAI :: RequestAI -> m ()
  sendRequestUI :: RequestUI -> m ()
  clientHasUI   :: m Bool

-- Note that when clients over net are implemented, execUpdAtomic will be
-- brought back, because executing a single cmd is cheaper than sending
-- the whole state over the net. However, for the standalone exe mode,
-- a pointer to the state will still be passed and set with @putState@,
-- as below.
handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
--                  , MonadClientAtomic m
                  , MonadStateWrite m
                  , MonadClientWriteRequest m )
               => Response -> m ()
handleResponse cmd = case cmd of
  RespUpdAtomic newState cmdA -> do
    hasUI <- clientHasUI
    cmds <- cmdAtomicFilterCli cmdA
    oldState <- getState
    putState newState
    let handle !c = do
          cmdAtomicSemCli oldState c
          when hasUI $ displayRespUpdAtomicUI False oldState c
    mapM_ handle cmds
  RespUpdAtomicNoState cmdA -> do
    hasUI <- clientHasUI
    cmds <- cmdAtomicFilterCli cmdA
    -- We assume at most one of @cmds@ changes state, so there is only one
    -- initial and one final state.
    oldState <- getState
    let handle !c = do
          -- execUpdAtomic c
          cmdAtomicSemCli oldState c
          when hasUI $ displayRespUpdAtomicUI False oldState c
    mapM_ handle cmds
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequestAI cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI False sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequestUI cmdH
