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
import Game.LambdaHack.Client.State

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- Auxiliary AI and computer player clients have no @sfs@ nor @sbinding@.
data SessionUI = SessionUI
  { sfs       :: !FrontendSession  -- ^ frontend session information
  , sbinding  :: !Binding          -- ^ binding of keys to commands
  }

class MonadActionRO m => MonadClientRO m where
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a

instance (Monoid a, MonadClientRO m) => MonadClientRO (WriterT a m) where
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

class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a

instance (Monoid a, MonadClientUI m) => MonadClientUI (WriterT a m) where
  getsSession  = lift . getsSession

class MonadClient m => MonadClientChan m where
  getsChan     :: (ConnCli -> a) -> m a
