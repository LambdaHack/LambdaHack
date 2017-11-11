{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of responses sent by the server to clients.
module Game.LambdaHack.Client.HandleResponseM
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  , handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.MonadStateRead

-- | Client monad in which one can receive responses from the server.
class MonadClient m => MonadClientReadResponse m where
  receiveResponse :: m Response

-- | Client monad in which one can send requests to the client.
class MonadClient m => MonadClientWriteRequest m where
  sendRequestAI :: RequestAI -> m ()
  sendRequestUI :: RequestUI -> m ()
  clientHasUI   :: m Bool

-- | Handle server responses.
--
-- Note that for clients communicating with the server over the net,
-- @RespUpdAtomicNoState@ should be used, because executing a single command
-- is cheaper than sending the whole state over the net.
-- However, for the standalone exe mode, with clients in the same process
-- as the server, a pointer to the state set with @execPutState@ is cheaper.
handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
                  , MonadClientAtomic m
                  , MonadClientWriteRequest m )
               => Response -> m ()
handleResponse cmd = case cmd of
  RespUpdAtomic newState cmdA -> do
    oldState <- getState
    execPutState newState
    cmdAtomicSemCli oldState cmdA
    hasUI <- clientHasUI
    when hasUI $ displayRespUpdAtomicUI False oldState cmdA
  RespUpdAtomicNoState cmdA -> do
    oldState <- getState
    execUpdAtomic cmdA
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
