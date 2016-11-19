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
{-# INLINABLE storeUndo #-}
storeUndo _atomic =
  maybe (return ()) (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    Nothing   -- TODO: undoCmdAtomic atomic

handleResponseAI :: ( MonadClientSetup m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestAI m )
                 => Response -> m ()
{-# INLINABLE handleResponseAI #-}
handleResponseAI cmd = case cmd of
  RespUpdAtomic cmdA ->
    handleSelfAI cmdA
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequest cmdC
  _ -> assert `failure` cmd

handleResponseUI :: ( MonadClientSetup m
                    , MonadClientUI m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestUI m )
                 => Response -> m ()
{-# INLINABLE handleResponseUI #-}
handleResponseUI cmd = case cmd of
  RespUpdAtomic cmdA ->
    handleSelfUI cmdA
  RespSfxAtomic sfx -> do
    displayRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequest cmdH
  _ -> assert `failure` cmd
