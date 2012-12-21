{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere in the library.
module Game.LambdaHack.TypeAction
  ( FunAction, Action, executor
  ) where

import Control.Monad.Reader.Class
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.MonadAction
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | The type of the function inside any full-power action.
type FunAction a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (State -> Diary -> a -> IO ())  -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

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

instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

instance Show (Action a) where
  show _ = "an action"

instance MonadActionPure Action where
  tryWith exc m =
    Action (\c p k a s d ->
             let runA msg = runAction (exc msg) c p k a s d
             in runAction m c p k runA s d)
  abortWith msg = Action (\_c _p _k a _s _d -> a msg)
  getServer     = Action (\_c _p k _a s d -> k s d s)
  getsServer    = (`fmap` getServer)
  getClient     = Action (\_c _p k _a s d -> k s d d)
  getsClient    = (`fmap` getClient)
  getsSession f = Action (\c _p k _a s d -> k s d (f c))

instance MonadActionRO Action where
  putClient nd   = Action (\_c _p k _a s _d -> k s nd ())
  modifyClient f = Action (\_c _p k _a s d -> k s (f d) ())
  liftIO x       = Action (\_c _p k _a s d -> x >>= k s d)

instance MonadAction Action where
  putServer ns   = Action (\_c _p k _a _s d -> k ns d ())
  modifyServer f = Action (\_c _p k _a s d -> k (f s) d ())

instance Functor Action where
  fmap f m = Action (\c p k a s d ->
                      runAction m c p (\s' d' -> k s' d' . f) a s d)

instance MonadReader Pers Action where
  ask       = Action (\_c p k _a s d -> k s d p)
  local f m = Action (\c p k a s d -> runAction m c (f p) k a s d)

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
