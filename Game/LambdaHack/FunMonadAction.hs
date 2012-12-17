{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic types and classes for game action. Exposed to let library users
-- define their own variants of the main action type @Action@.
-- This module should not be imported anywhere except in MonadAction
-- and TypeAction.
module Game.LambdaHack.FunMonadAction
  ( Session(..), FunActionRO, FunAction, MonadActionRO(..), MonadAction(..)
  ) where

import Control.Monad.Reader.Class
import qualified Control.Monad.State as St
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

-- | The type of the function inside any action.
type FunAction a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (State -> Diary -> a -> IO ())  -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

-- | The type of the function inside any read-only action.
type FunActionRO a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (a -> IO ())                    -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

class (Monad m, Functor m, MonadReader Pers m, Show (m ()))
      => MonadActionRO m where
  fun2actionRO :: FunActionRO a -> m a
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith :: (Msg -> m a) -> m a -> m a
  get :: m State
  get = fun2actionRO (\_c _p k _a s _d -> k s)
  gets :: (State -> a) -> m a
  gets = (`fmap` get)
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO :: IO a -> m a
  liftIO x = fun2actionRO (\_c _p k _a _s _d -> x >>= k)

-- The following triggers a GHC limitation (Overlapping instances for Show):
-- instance MonadActionRO m => Show (m a) where
--   show _ = "an action"
-- TODO: try again, but not sooner than in a few years, so that users
-- with old compilers don't have compilation problems. The same with
--  instance MonadAction m => St.MonadState State m where
--  get    = get
--  put ns = fun2action (\_c _p k _a _s d -> k ns d ())
-- and with MonadReader Pers m
instance MonadActionRO m => Show (WriterT Frames m a) where
  show _ = "an action"

instance MonadActionRO m => MonadActionRO (WriterT Frames m) where
  fun2actionRO = lift . fun2actionRO
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)

class (MonadActionRO m, St.MonadState State m) => MonadAction m where
  fun2action :: FunAction a -> m a

instance MonadAction m => MonadAction (WriterT Frames m) where
  fun2action = lift . fun2action
