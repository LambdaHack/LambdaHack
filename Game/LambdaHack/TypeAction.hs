{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
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
   -> (State -> StateServer -> StateClient -> State -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current globalstate
   -> StateServer                     -- ^ current server state
   -> StateClient                     -- ^ current client state
   -> State                           -- ^ current local state
   -> IO ()

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action {runAction :: FunAction a}

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\_c _p k _a s ser cli loc -> k s ser cli loc x)

-- | Distributes the session and shutdown continuation,
-- threads the state and cli.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\c p k a s ser cli loc ->
                          let next ns nser ncli nloc x =
                                runAction (f x) c p k a ns nser ncli nloc
                          in runAction m c p next a s ser cli loc)

instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

instance Show (Action a) where
  show _ = "an action"

instance MonadActionPure Action where
  tryWith exc m =
    Action (\c p k a s ser cli loc ->
             let runA msg = runAction (exc msg) c p k a s ser cli loc
             in runAction m c p k runA s ser cli loc )
  abortWith msg = Action (\_c _p _k a _s _ser _cli _loc -> a msg)
  getGlobal     = Action (\_c _p k _a s ser cli loc -> k s ser cli loc s)
  getsGlobal    = (`fmap` getGlobal)
  getServer     = Action (\_c _p k _a s ser cli loc -> k s ser cli loc ser)
  getsServer    = (`fmap` getServer)
  getClient     = Action (\_c _p k _a s ser cli loc -> k s ser cli loc cli)
  getsClient    = (`fmap` getClient)
  getLocal      = Action (\_c _p k _a s ser cli loc -> k s ser cli loc loc)
  getsLocal     = (`fmap` getLocal)
  getsSession f = Action (\c _p k _a s ser cli loc -> k s ser cli loc (f c))

instance MonadActionRO Action where
  putGlobal ns   = Action (\_c _p k _a _s ser cli loc -> k ns ser cli loc ())
  modifyGlobal f = Action (\_c _p k _a s ser cli loc -> k (f s) ser cli loc ())
  putServer nser = Action (\_c _p k _a s _ser cli loc -> k s nser cli loc ())
  modifyServer f = Action (\_c _p k _a s ser cli loc -> k s (f ser) cli loc ())
  putClient ncli = Action (\_c _p k _a s ser _cli loc -> k s ser ncli loc ())
  modifyClient f = Action (\_c _p k _a s ser cli loc -> k s ser (f cli) loc ())
  putLocal nloc  = Action (\_c _p k _a s ser cli _loc -> k s ser cli nloc ())
  modifyLocal f  = Action (\_c _p k _a s ser cli loc -> k s ser cli (f loc) ())
  liftIO x       = Action (\_c _p k _a s ser cli loc -> x >>= k s ser cli loc)

instance MonadAction Action where

instance Functor Action where
  fmap f m =
    Action (\c p k a s ser cli loc ->
               runAction m c p (\s' ser' cli' loc' ->
                                   k s' ser' cli' loc'. f) a s ser cli loc)

instance MonadReader Pers Action where
  ask       = Action (\_c p k _a s ser cli loc -> k s ser cli loc p)
  local f m = Action (\c p k a s ser cli loc ->
                         runAction m c (f p) k a s ser cli loc)

-- | Run an action, with a given session, state and cli, in the @IO@ monad.
executor :: Action ()
         -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
         -> State -> StateServer -> StateClient -> State -> IO ()
executor m sfs scops sbinding sconfigUI state ser cli loc =
  runAction m
    Session{..}
    (dungeonPerception scops (sconfig ser) (sdebug cli) state)  -- create and cache perception
    (\_ _ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> fail $ T.unpack $ "unhandled abort:" <+> msg)  -- e.g., in AI code
    state
    ser
    cli
    loc
