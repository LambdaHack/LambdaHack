{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( handleResponseAI, handleResponseUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response

storeUndo :: MonadClient m => CmdAtomic -> m ()
storeUndo _atomic =
  maybe (return ()) (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    Nothing   -- TODO: undoCmdAtomic atomic

handleResponseAI :: ( MonadClientSetup m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestAI m )
                 => ResponseAI -> m ()
handleResponseAI cmd = case cmd of
  RespUpdAtomicAI cmdA ->
    handleSelfAI cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequest cmdC

handleResponseUI :: ( MonadClientSetup m
                    , MonadClientUI m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestUI m )
                 => ResponseUI -> m ()
handleResponseUI cmd = case cmd of
  RespUpdAtomicUI cmdA ->
    handleSelfUI cmdA
  RespSfxAtomicUI sfx -> do
    displayRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequest cmdH
