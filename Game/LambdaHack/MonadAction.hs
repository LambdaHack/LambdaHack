{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions. Exposed to let library users
-- define their own variants of the main action type @Action@.
-- This module should not be imported anywhere except in Action
-- and TypeAction.
module Game.LambdaHack.MonadAction
  ( Session(..), MonadActionPure(..), MonadActionRO(..), MonadAction
  ) where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict (WriterT (WriterT, runWriterT), lift)

import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs       :: !FrontendSession  -- ^ frontend session information
  , sbinding  :: !Binding          -- ^ binding of keys to commands
  , sconfigUI :: !ConfigUI         -- ^ the UI config for this session
  }

class (Monad m, Functor m, MonadReader Pers m, Show (m ()))
      => MonadActionPure m where
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith     :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith   :: MonadActionPure m => Msg -> m a
  getGlobal   :: m State
  getsGlobal  :: (State -> a) -> m a
  getServer   :: m StateServer
  getsServer  :: (StateServer -> a) -> m a
  getClient   :: m StateClient
  getsClient  :: (StateClient -> a) -> m a
  getLocal    :: m State
  getsLocal   :: (State -> a) -> m a
  getsSession :: (Session -> a) -> m a

instance MonadActionPure m => MonadActionPure (WriterT Slideshow m) where
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)
  abortWith   = lift . abortWith
  getGlobal   = lift getGlobal
  getsGlobal  = lift . getsGlobal
  getServer   = lift getServer
  getsServer  = lift . getsServer
  getClient   = lift getClient
  getsClient  = lift . getsClient
  getLocal    = lift getLocal
  getsLocal   = lift . getsLocal
  getsSession = lift . getsSession

instance MonadActionPure m => Show (WriterT Slideshow m a) where
  show _ = "an action"

class MonadActionPure m => MonadActionRO m where
  putGlobal    :: State -> m ()
  modifyGlobal :: (State -> State) -> m ()
  putServer    :: StateServer -> m ()
  modifyServer :: (StateServer -> StateServer) -> m ()
  putClient    :: StateClient -> m ()
  modifyClient :: (StateClient -> StateClient) -> m ()
  putLocal     :: State -> m ()
  modifyLocal  :: (State -> State) -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a

instance MonadActionRO m => MonadActionRO (WriterT Slideshow m) where
  putGlobal    = lift . putGlobal
  modifyGlobal = lift . modifyGlobal
  putServer    = lift . putServer
  modifyServer = lift . modifyServer
  putClient    = lift . putClient
  modifyClient = lift . modifyClient
  putLocal     = lift . putLocal
  modifyLocal  = lift . modifyLocal
  liftIO       = lift . liftIO

class MonadActionRO m => MonadAction m where

instance MonadAction m => MonadAction (WriterT Slideshow m) where
