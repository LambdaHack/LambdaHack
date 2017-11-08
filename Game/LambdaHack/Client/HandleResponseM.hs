{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  , handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
-- a pointer to the state will still be passed and set with @exexPutState@,
-- as below.
handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
                  , MonadClientAtomic m
                  , MonadClientWriteRequest m )
               => Response -> m ()
handleResponse cmd = case cmd of
  RespUpdAtomic newState cmdA -> do
    oldState <- getState
    exexPutState newState
    cmdAtomicSemCli oldState cmdA
    hasUI <- clientHasUI
    when hasUI $ displayRespUpdAtomicUI False oldState cmdA
  RespUpdAtomicNoState cmdA -> do
    oldState <- getState
    -- execUpdAtomic cmdA
    cmdAtomicSemCli oldState cmdA
    hasUI <- clientHasUI
    when hasUI $ displayRespUpdAtomicUI False oldState cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequestAI cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI False sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequestUI cmdH
