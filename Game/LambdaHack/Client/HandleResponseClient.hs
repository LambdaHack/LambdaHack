{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseClient
  ( handleResponseAI, handleResponseUI
  ) where

import Control.Exception.Assert.Sugar

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleCmdAtomicClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response

storeUndo :: MonadClient m => CmdAtomic -> m ()
storeUndo _atomic =
  maybe skip (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    $ Nothing   -- TODO: undoCmdAtomic atomic

handleResponseAI :: (MonadAtomic m, MonadClientWriteRequest RequestTimed m)
                 => ResponseAI -> m ()
handleResponseAI cmd = case cmd of
  RespUpdAtomicAI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ (\c -> cmdAtomicSemCli c
                 >> execUpdAtomic c) cmds
    mapM_ (storeUndo . UpdAtomic) cmds
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequest cmdC
  RespPingAI -> do
    pong <- pongAI
    sendRequest pong

handleResponseUI :: ( MonadAtomic m, MonadClientUI m
                    , MonadClientWriteRequest Request m )
                 => ResponseUI -> m ()
handleResponseUI cmd = case cmd of
  RespUpdAtomicUI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ (\c -> cmdAtomicSemCli c
                 >> execUpdAtomic c
                 >> displayRespUpdAtomicUI False c) cmds
    mapM_ (storeUndo . UpdAtomic) cmds  -- TODO: only store cmdA?
  RespSfxAtomicUI sfx -> do
    displayRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
  RespQueryUI aid -> do
    cmdH <- queryUI aid
    sendRequest cmdH
  RespPingUI -> do
    pong <- pongUI
    sendRequest pong
