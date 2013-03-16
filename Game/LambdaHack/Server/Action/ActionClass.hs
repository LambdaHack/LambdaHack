{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Server.Action.ActionClass where

import Control.Monad.Writer.Strict (WriterT, lift)
import Data.Monoid

import Game.LambdaHack.Action
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Server.State

class MonadActionRO m => MonadServer m where
  getServer    :: m StateServer
  getsServer   :: (StateServer -> a) -> m a
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a

instance (Monoid a, MonadServer m) => MonadServer (WriterT a m) where
  getServer    = lift getServer
  getsServer   = lift . getsServer
  modifyServer = lift . modifyServer
  putServer    = lift . putServer
  liftIO       = lift . liftIO

class MonadServer m => MonadServerConn m where
  getDict      :: m ConnDict
  getsDict     :: (ConnDict -> a) -> m a
  modifyDict   :: (ConnDict -> ConnDict) -> m ()
  putDict      :: ConnDict -> m ()

instance (Monoid a, MonadServerConn m) => MonadServerConn (WriterT a m) where
  getDict      = lift getDict
  getsDict     = lift . getsDict
  modifyDict   = lift . modifyDict
  putDict      = lift . putDict
