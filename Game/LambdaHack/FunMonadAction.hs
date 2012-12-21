{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses #-}
-- | Basic types and classes for game action. Exposed to let library users
-- define their own variants of the main action type @Action@.
-- This module should not be imported anywhere except in MonadAction
-- and TypeAction.
module Game.LambdaHack.FunMonadAction
  ( Session(..), FunAction
  , MonadActionPure(..), MonadActionRO(..), MonadAction(..)
  ) where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict

import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Animation (Frames)
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs       :: !FrontendSession  -- ^ frontend session information
  , scops     :: !Kind.COps        -- ^ game content
  , sbinding  :: !Binding          -- ^ binding of keys to commands
  , sconfigUI :: !ConfigUI         -- ^ the UI config for this session
  }

-- | The type of the function inside any pure action.
type FunActionPure a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (a -> IO ())                    -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

-- | The type of the function inside actions that don't modify server state.
type FunActionRO a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (Diary -> a -> IO ())           -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

-- | The type of the function inside any full-power action.
type FunAction a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (State -> Diary -> a -> IO ())  -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

class (Monad m, Functor m, MonadReader Pers m, Show (m ()))
      => MonadActionPure m where
  fun2actionPure :: FunActionPure a -> m a
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith :: MonadActionPure m => Msg -> m a
  abortWith msg = fun2actionPure (\_c _p _k a _s _d -> a msg)
  getServer :: m State
  getServer = fun2actionPure (\_c _p k _a s _d -> k s)
  getsServer :: (State -> a) -> m a
  getsServer = (`fmap` getServer)
  getClient :: m Diary
  getClient = fun2actionPure (\_c _p k _a _s d -> k d)
  getsClient :: (Diary -> a) -> m a
  getsClient = (`fmap` getClient)
  getsSession :: (Session -> a) -> m a
  getsSession f = fun2actionPure (\c _p k _a _s _d -> k (f c))

instance MonadActionPure m => MonadActionPure (WriterT Frames m) where
  fun2actionPure = lift . fun2actionPure
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)

instance MonadActionPure m => Show (WriterT Frames m a) where
  show _ = "an action"

class MonadActionPure m => MonadActionRO m where
  fun2actionRO :: FunActionRO a -> m a
  putClient :: Diary -> m ()
  putClient nd = fun2actionRO (\_c _p k _a _s _d -> k nd ())
  modifyClient :: (Diary -> Diary) -> m ()
  modifyClient f = fun2actionRO (\_c _p k _a _s d -> k (f d) ())
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO :: IO a -> m a
  liftIO x = fun2actionPure (\_c _p k _a _s _d -> x >>= k)

instance MonadActionRO m => MonadActionRO (WriterT Frames m) where
  fun2actionRO = lift . fun2actionRO

-- The following triggers a GHC limitation (Overlapping instances for Show):
-- instance MonadActionPure m => Show (m a) where
--   show _ = "an action"
-- TODO: try again, but not sooner than in a few years, so that users
-- with old compilers don't have compilation problems. The same with
--  instance MonadAction m => St.MonadState State m where
--  get = get
--  put ns = fun2action (\_c _p k _a _s d -> k ns d ())
-- and with MonadReader Pers m

class MonadActionRO m => MonadAction m where
  fun2action :: FunAction a -> m a
  putServer :: State -> m ()
  putServer ns = fun2action (\_c _p k _a _s d -> k ns d ())
  modifyServer :: (State -> State) -> m ()
  modifyServer f = fun2action (\_c _p k _a s d -> k (f s) d ())

instance MonadAction m => MonadAction (WriterT Frames m) where
  fun2action = lift . fun2action
