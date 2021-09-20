{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of responses sent by the server to clients.
module Game.LambdaHack.Client.HandleResponseM
  ( MonadClientAtomic(..)
  , handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Atomic (UpdAtomic)
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

-- | Monad for executing atomic game state transformations on a client.
class MonadClient m => MonadClientAtomic m where
  -- | Execute an atomic update that changes the client's 'State'.
  execUpdAtomic :: UpdAtomic -> m ()
  -- | Put state that is intended to be the result of performing
  -- an atomic update by the server on its copy of the client's 'State'.
  execPutState :: State -> m ()

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
    when hasUI $ displayRespUpdAtomicUI cmdA
  RespUpdAtomicNoState cmdA -> do
    oldState <- getState
    execUpdAtomic cmdA
    cmdAtomicSemCli oldState cmdA
    hasUI <- clientHasUI
    when hasUI $ displayRespUpdAtomicUI cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequestAI cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI sfx
  RespQueryUI -> do
    hasUI <- clientHasUI
    sreqExpected <- getsSession sreqExpected
    let !_A = assert (hasUI && not sreqExpected
                      `blame` "server expects a command from a client without UI or doesn't wait for its previously expected command"
                      `swith` (hasUI, sreqExpected)) ()
    modifySession $ \sess -> sess {sreqExpected = True}
