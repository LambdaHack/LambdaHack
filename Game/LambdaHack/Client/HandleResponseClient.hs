{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseClient
  ( handleResponseAI, handleResponseUI
  ) where

import Control.Exception.Assert.Sugar
import Data.Maybe

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AtomicSemCli
import Game.LambdaHack.Client.ClientSem
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.MonadClientUI
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
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
  RespPingAI -> pongAI

handleResponseUI :: ( MonadAtomic m, MonadClientUI m
                    , MonadClientWriteRequest Request m )
                 => ResponseUI -> m ()
handleResponseUI cmd = case cmd of
  RespUpdAtomicUI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ (\c -> cmdAtomicSemCli c
                 >> execUpdAtomic c
                 >> drawRespUpdAtomicUI False c) cmds
    mapM_ (storeUndo . UpdAtomic) cmds  -- TODO: only store cmdA?
  RespSfxAtomicUI sfx -> do
    drawRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
  RespQueryUI aid -> do
    mleader <- getsClient _sleader
    assert (isJust mleader `blame` "query without leader" `twith` cmd) skip
    cmdH <- queryUI aid
    sendRequest cmdH
  RespPingUI -> pongUI
