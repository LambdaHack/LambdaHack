{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | The client-server communication monads.
module Game.LambdaHack.Client.ProtocolClient
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  ) where

import Game.LambdaHack.Client.MonadClient

class MonadClient m => MonadClientReadResponse resp m | m -> resp where
  receiveResponse  :: m resp

class MonadClient m => MonadClientWriteRequest req m | m -> req where
  sendRequest  :: req -> m ()
