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

handleResponse :: ( MonadClientSetup m
                  , MonadClientUI m
                  , MonadAtomic m
                  , MonadClientWriteRequest m )
               => Response -> m ()
handleResponse cmd = case cmd of
  RespUpdAtomic newState cmdA -> do
    hasUI <- clientHasUI
    cmds <- cmdAtomicFilterCli cmdA
    let handle !c = do
          s <- getState
          succeeded <- execUpdAtomicCatch c
          when succeeded $ do
            cli <- getClient
            cmdAtomicSemCli s c
            when hasUI $ displayRespUpdAtomicUI False cli c
    mapM_ handle cmds
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    sendRequestAI cmdC
  RespSfxAtomic sfx ->
    displayRespSfxAtomicUI False sfx
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequestUI cmdH
