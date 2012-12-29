{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for game actions. Exposed to let library users
-- define their own variants of the main action type @Action@.
-- This module should not be imported anywhere except in Action
-- and TypeAction.
module Game.LambdaHack.MonadAction
  ( Session(..), MonadActionPure(..), MonadActionRO(..), MonadAction
  ) where

import Control.Arrow (first, second)
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict (WriterT (WriterT, runWriterT), lift)
import qualified Data.IntMap as IM

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
  getsGlobal  = (`fmap` getGlobal)
  getServer   :: m StateServer
  getsServer  :: (StateServer -> a) -> m a
  getsServer  = (`fmap` getServer)
  getDict     :: m StateDict
  getsDict    :: (StateDict -> a) -> m a
  getsDict    = (`fmap` getDict)
  getClient   :: m StateClient
  getClient   = do
    State{sside} <- getGlobal
    d <- getDict
    return $! fst $! d IM.! sside
  getsClient  :: (StateClient -> a) -> m a
  getsClient  = (`fmap` getClient)
  getLocal    :: m State
  getLocal    = do
    State{sside} <- getGlobal
    d <- getDict
    return $! snd $! d IM.! sside
  getsLocal   :: (State -> a) -> m a
  getsLocal   = (`fmap` getLocal)
  getsSession :: (Session -> a) -> m a

instance MonadActionPure m => MonadActionPure (WriterT Slideshow m) where
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)
  abortWith   = lift . abortWith
  getGlobal   = lift getGlobal
  getServer   = lift getServer
  getDict     = lift getDict
  getsSession = lift . getsSession

instance MonadActionPure m => Show (WriterT Slideshow m a) where
  show _ = "an action"

class MonadActionPure m => MonadActionRO m where
  modifyGlobal :: (State -> State) -> m ()
  putGlobal    :: State -> m ()
  putGlobal    = modifyGlobal . const
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()
  putServer    = modifyServer . const
  modifyDict   :: (StateDict -> StateDict) -> m ()
  putDict      :: StateDict -> m ()
  putDict      = modifyDict . const
  modifyClient :: (StateClient -> StateClient) -> m ()
  modifyClient f = do
    State{sside} <- getGlobal
    modifyDict (IM.adjust (first f) sside)
  putClient    :: StateClient -> m ()
  putClient    = modifyClient . const
  modifyLocal  :: (State -> State) -> m ()
  modifyLocal f = do
    State{sside} <- getGlobal
    modifyDict (IM.adjust (second f) sside)
  putLocal     :: State -> m ()
  putLocal     = modifyLocal . const
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a

instance MonadActionRO m => MonadActionRO (WriterT Slideshow m) where
  modifyGlobal = lift . modifyGlobal
  modifyServer = lift . modifyServer
  modifyDict   = lift . modifyDict
  liftIO       = lift . liftIO

class MonadActionRO m => MonadAction m where

instance MonadAction m => MonadAction (WriterT Slideshow m) where
