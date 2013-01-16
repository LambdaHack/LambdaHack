{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Client.Action.ActionClass where

import Control.Monad.Writer.Strict (WriterT, lift)
import Data.Monoid

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action.Frontend
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs       :: !FrontendSession  -- ^ frontend session information
  , sbinding  :: !Binding          -- ^ binding of keys to commands
  , sconfigUI :: !ConfigUI         -- ^ the UI config for this session
  }

class MonadActionRO m => MonadClientRO m where
  getsSession  :: (Session -> a) -> m a
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a

instance (Monoid a, MonadClientRO m) => MonadClientRO (WriterT a m) where
  getsSession  = lift . getsSession
  getClient    = lift getClient
  getsClient   = lift . getsClient

class (MonadAction m, MonadClientRO m) => MonadClient m where
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a

instance (Monoid a, MonadClient m) => MonadClient (WriterT a m) where
  modifyClient = lift . modifyClient
  putClient    = lift . putClient
  liftIO       = lift . liftIO

class MonadClient m => MonadClientChan m where
  getChan      :: m ConnClient
  getsChan     :: (ConnClient -> a) -> m a
  modifyChan   :: (ConnClient -> ConnClient) -> m ()
  putChan      :: ConnClient -> m ()

instance (Monoid a, MonadClientChan m) => MonadClientChan (WriterT a m) where
  getChan      = lift getChan
  getsChan     = lift . getsChan
  modifyChan   = lift . modifyChan
  putChan      = lift . putChan
