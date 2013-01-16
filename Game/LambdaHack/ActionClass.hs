{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.ActionClass where

import Control.Concurrent.Chan
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict (WriterT (WriterT), runWriterT, lift)
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Dynamic

import Game.LambdaHack.Client.Action.Frontend
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Config
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- * Component types

-- | Connection information for each client and an options AI client
-- for the same faction, indexed by faction identifier.
type ConnDict = IM.IntMap ConnFaction

type ConnFaction = (ConnClient, Maybe ConnClient)

-- | Connection information for a client. Input and output channels, etc.
data ConnClient = ConnClient
  { toClient   :: Chan CmdCli
  , toServer   :: Chan Dynamic
  }

instance Show ConnClient where
  show _ = "client channels"

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs        :: !FrontendSession  -- ^ frontend session information
  , sbinding   :: !Binding          -- ^ binding of keys to commands
  , sconfigUI  :: !ConfigUI         -- ^ the UI config for this session
  }

-- * Basic monads

-- | The bottom of the action monads class semilattice.
class (Monad m, Functor m, Show (m ())) => MonadActionRoot m where
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith      :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith    :: Msg -> m a

instance (Monoid a, MonadActionRoot m) => MonadActionRoot (WriterT a m) where
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)
  abortWith   = lift . abortWith

instance MonadActionRoot m => Show (WriterT a m b) where
  show _ = "an action"

class MonadActionRoot m => MonadActionRO m where
  getState    :: m State
  getsState   :: (State -> a) -> m a

instance (Monoid a, MonadActionRO m) => MonadActionRO (WriterT a m) where
  getState    = lift getState
  getsState   = lift . getsState

class MonadActionRoot m => MonadActionIO m where
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO :: IO a -> m a

instance (Monoid a, MonadActionIO m) => MonadActionIO (WriterT a m) where
  liftIO = lift . liftIO

class (MonadActionIO m, MonadActionRO m) => MonadAction m where
  modifyState :: (State -> State) -> m ()
  putState    :: State -> m ()

instance (Monoid a, MonadAction m) => MonadAction (WriterT a m) where
  modifyState = lift . modifyState
  putState    = lift . putState

-- * Server monads

class (MonadReader Pers m, MonadActionRO m) => MonadServerRO m where
  getServer    :: m StateServer
  getsServer   :: (StateServer -> a) -> m a

instance (Monoid a, MonadServerRO m) => MonadServerRO (WriterT a m) where
  getServer    = lift getServer
  getsServer   = lift . getsServer

class (MonadAction m, MonadServerRO m) => MonadServer m where
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()

instance (Monoid a, MonadServer m) => MonadServer (WriterT a m) where
  modifyServer = lift . modifyServer
  putServer    = lift . putServer

class MonadServer m => MonadServerChan m where
  getDict      :: m ConnDict
  getsDict     :: (ConnDict -> a) -> m a
  modifyDict   :: (ConnDict -> ConnDict) -> m ()
  putDict      :: ConnDict -> m ()

instance (Monoid a, MonadServerChan m) => MonadServerChan (WriterT a m) where
  getDict      = lift getDict
  getsDict     = lift . getsDict
  modifyDict   = lift . modifyDict
  putDict      = lift . putDict

-- * Client monads

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

instance (Monoid a, MonadClient m) => MonadClient (WriterT a m) where
  modifyClient = lift . modifyClient
  putClient    = lift . putClient

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
