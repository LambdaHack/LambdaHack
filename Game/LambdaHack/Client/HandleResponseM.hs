{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  , handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response

class MonadClient m => MonadClientReadResponse m where
  receiveResponse :: m Response

class MonadClient m => MonadClientWriteRequest m where
  sendRequestAI :: RequestAI -> m ()
  sendRequestUI :: RequestUI -> m ()
  clientHasUI   :: m Bool

handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
                  , MonadAtomic m
                  , MonadClientWriteRequest m )
               => Response -> m ()
{-# INLINABLE handleResponse #-}
handleResponse cmd = case cmd of
  RespUpdAtomic cmdA -> do
    hasUI <- clientHasUI
    if not hasUI then handleSelfAI cmdA else handleSelfUI cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequestAI cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI False sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequestUI cmdH

handleSelfAI :: ( MonadClientSetup m
                , MonadAtomic m )
             => UpdAtomic -> m ()
{-# INLINABLE handleSelfAI #-}
handleSelfAI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  mapM_ (\ !c -> cmdAtomicSemCli c
                 >> execUpdAtomic c) cmds
  -- mapM_ (storeUndo . UpdAtomic) cmds

handleSelfUI :: ( MonadClientSetup m
                , MonadClientUI m
                , MonadAtomic m )
             => UpdAtomic -> m ()
{-# INLINABLE handleSelfUI #-}
handleSelfUI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  let handle !c = do
        -- Avoid leaking the whole client state.
        !StateClient{sdiscoKind, sdiscoAspect} <- getClient
        cmdAtomicSemCli c
        execUpdAtomic c
        displayRespUpdAtomicUI False sdiscoKind sdiscoAspect c
  mapM_ handle cmds
  -- mapM_ (storeUndo . UpdAtomic) cmds  -- TODO: only store cmdA?
