{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( handleResponse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Response

handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
                  , MonadAtomic m
                  , MonadClientWriteRequest m )
               => Response -> m ()
{-# INLINABLE handleResponse #-}
handleResponse cmd = case cmd of
  RespUpdAtomic noUI cmdA ->
    if noUI then handleSelfAI cmdA else handleSelfUI cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequest $ Left cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI False sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequest $ Right cmdH
