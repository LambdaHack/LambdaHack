{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere in the library.
module Game.LambdaHack.TypeAction
  ( Action, executor
  ) where

import Control.Monad.Reader.Class
import qualified Control.Monad.State as St
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.FunAction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action {runAction :: FunAction a}

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\_c _p k _a s d -> k s d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and diary.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\c p k a s d ->
                          let next ns nd x =
                                runAction (f x) c p k a ns nd
                          in runAction m c p next a s d)

-- TODO: check if it's strict enough, if we don't keep old states for too long,
-- Perhaps make state type fields strict for that, too?
instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

instance Show (Action a) where
  show _ = "an action"

instance MonadActionRO Action where
  fun2actionRO f = Action (\c p k a s d -> f c p (k s d) a s d)
  tryWith exc m =
    Action (\c p k a s d ->
             let runA msg = runAction (exc msg) c p k a s d
             in runAction m c p k runA s d)

instance MonadAction Action where
  fun2action = Action

instance Functor Action where
  fmap f m = fun2actionRO (\c p k a s d ->
                            runAction m c p (\_ _ -> k . f) a s d)

--instance MonadAction m => MonadState State m where
instance St.MonadState State Action where
  get    = get
  put ns = fun2action (\_c _p k _a _s d -> k ns d ())

instance MonadReader Pers Action where
  ask = fun2actionRO (\_c p k _a _s _d -> k p)
  local f m = fun2action (\c p k a s d -> runAction m c (f p) k a s d)

-- | Run an action, with a given session, state and diary, in the @IO@ monad.
executor :: Action ()
         -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
         -> State -> Diary -> IO ()
executor m sfs scops sbinding sconfigUI state diary =
  runAction m
    Session{..}
    (dungeonPerception scops state)  -- create and cache perception
    (\_ _ _ -> return ())  -- final continuation returns result
    (\msg -> fail $ T.unpack $ "unhandled abort:" <+> msg)  -- e.g., in AI code
    state
    diary
