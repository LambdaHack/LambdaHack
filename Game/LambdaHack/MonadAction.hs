{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.MonadAction where

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
      => MonadActionBase m where
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith     :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith   :: Msg -> m a

instance MonadActionBase m => MonadActionBase (WriterT Slideshow m) where
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)
  abortWith   = lift . abortWith

instance MonadActionBase m => Show (WriterT Slideshow m a) where
  show _ = "an action"

class MonadActionBase m => MonadServerPure m where
  getGlobal   :: m State
  getsGlobal  :: (State -> a) -> m a
  getServer   :: m StateServer
  getsServer  :: (StateServer -> a) -> m a

instance MonadServerPure m => MonadServerPure (WriterT Slideshow m) where
  getGlobal   = lift getGlobal
  getsGlobal  = lift . getsGlobal
  getServer   = lift getServer
  getsServer  = lift . getsServer

class MonadActionBase m => MonadClientPure m where
  getsSession :: (Session -> a) -> m a
  getClient   :: m StateClient
  getsClient  :: (StateClient -> a) -> m a
  getLocal    :: m State
  getsLocal   :: (State -> a) -> m a

instance MonadClientPure m => MonadClientPure (WriterT Slideshow m) where
  getsSession = lift . getsSession
  getClient   = lift getClient
  getsClient  = lift . getsClient
  getLocal    = lift getLocal
  getsLocal   = lift . getsLocal

class (MonadServerPure m, MonadClientPure m) => MonadClientServerPure m where

instance MonadClientServerPure m
         => MonadClientServerPure (WriterT Slideshow m) where

class MonadClientServerPure m => MonadActionPure m where
  getDict     :: m StateDict
  getsDict    :: (StateDict -> a) -> m a

instance MonadActionPure m => MonadActionPure (WriterT Slideshow m) where
  getDict     = lift getDict
  getsDict    = lift . getsDict

class MonadActionBase m => MonadActionIO m where
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO :: IO a -> m a

instance MonadActionIO m => MonadActionIO (WriterT Slideshow m) where
  liftIO = lift . liftIO

class (MonadActionIO m, MonadServerPure m) => MonadServer m where
  modifyGlobal :: (State -> State) -> m ()
  putGlobal    :: State -> m ()
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()

instance MonadServer m => MonadServer (WriterT Slideshow m) where
  modifyGlobal = lift . modifyGlobal
  putGlobal    = lift . putGlobal
  modifyServer = lift . modifyServer
  putServer    = lift . putServer

class (MonadActionIO m, MonadClientPure m) => MonadClient m where
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  modifyLocal  :: (State -> State) -> m ()
  putLocal     :: State -> m ()

instance MonadClient m => MonadClient (WriterT Slideshow m) where
  modifyClient = lift . modifyClient
  putClient    = lift . putClient
  modifyLocal  = lift . modifyLocal
  putLocal     = lift . putLocal

class (MonadActionIO m, MonadClientServerPure m, MonadServer m, MonadClient m)
      => MonadClientServer m where

instance MonadClientServer m => MonadClientServer (WriterT Slideshow m) where

class (MonadActionIO m, MonadActionPure m, MonadClientServer m)
      => MonadAction m where
  modifyDict   :: (StateDict -> StateDict) -> m ()
  putDict      :: StateDict -> m ()

instance MonadAction m => MonadAction (WriterT Slideshow m) where
  modifyDict   = lift . modifyDict
  putDict      = lift . putDict
