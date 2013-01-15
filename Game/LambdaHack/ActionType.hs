{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere in the library.
module Game.LambdaHack.ActionType
  ( FunAction, Action, executor, FunActionCli, ActionCli, executorCli
  ) where

import Control.Monad.Reader.Class
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.ActionClass
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- | The type of the function inside any full-power action.
type FunAction a =
   Pers                               -- ^ cached perception
   -> (State -> StateServer -> ConnDict -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current global state
   -> StateServer                     -- ^ current server state
   -> ConnDict                        -- ^ client-server connection information
   -> IO ()

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action {runAction :: FunAction a}

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\_p k _a s ser d -> k s ser d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\p k a s ser d ->
                          let next ns nser nd x =
                                runAction (f x) p k a ns nser nd
                          in runAction m p next a s ser d)

instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor Action where
  fmap f m =
    Action (\p k a s ser d ->
               runAction m p (\s' ser' d' ->
                                   k s' ser' d'. f) a s ser d)

instance Show (Action a) where
  show _ = "an action"

instance MonadActionRoot Action where
  tryWith exc m =
    Action (\p k a s ser d ->
             let runA msg = runAction (exc msg) p k a s ser d
             in runAction m p k runA s ser d)
  abortWith msg = Action (\_p _k a _s _ser _d -> a msg)

instance MonadReader Pers Action where
  ask       = Action (\p k _a s ser d -> k s ser d p)
  local f m = Action (\p k a s ser d ->
                         runAction m (f p) k a s ser d)

instance MonadActionRO Action where
  getState  = Action (\_p k _a s ser d -> k s ser d s)
  getsState = (`fmap` getState)

instance MonadActionIO Action where
  liftIO x = Action (\_p k _a s ser d -> x >>= k s ser d)

instance MonadAction Action where
  modifyState f = Action (\_p k _a s ser d -> k (f s) ser d ())
  putState      = modifyState . const

instance MonadServerRO Action where
  getServer  = Action (\_p k _a s ser d -> k s ser d ser)
  getsServer = (`fmap` getServer)

instance MonadServer Action where
  modifyServer f = Action (\_p k _a s ser d -> k s (f ser) d ())
  putServer      = modifyServer . const

instance MonadServerChan Action where
  getDict        = Action (\_p k _a s ser d -> k s ser d d)
  getsDict       = (`fmap` getDict)
  modifyDict f   = Action (\_p k _a s ser d -> k s ser (f d) ())
  putDict        = modifyDict . const

-- | Run an action, with a given session, state and history, in the @IO@ monad.
executor :: Action () -> Pers -> State -> StateServer -> ConnDict -> IO ()
executor m pers s ser d =
  runAction m
    pers
    (\_ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> let err = "unhandled server abort for side" <+> showT (getSide s)
                       <+> ":" <+> msg
             in fail $ T.unpack err)
    s
    ser
    d

-- TODO: check if we can move factionPerception from state to Reader
-- TODO: check if we can unify State put/get. etc. between Client and Server
-- | The type of the function inside any client action.
type FunActionCli a =
   Session                            -- ^ client session setup data
   -> (State -> StateClient -> ConnClient -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current local state
   -> StateClient                     -- ^ current client state
   -> ConnClient                      -- ^ this client connection information
   -> IO ()

-- | Client actions.
newtype ActionCli a = ActionCli {runActionCli :: FunActionCli a}

-- | Invokes the action continuation on the provided argument.
returnActionCli :: a -> ActionCli a
returnActionCli x = ActionCli (\_c k _a s cli d -> k s cli d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindActionCli :: ActionCli a -> (a -> ActionCli b) -> ActionCli b
bindActionCli m f = ActionCli (\c k a s cli d ->
                          let next ns ncli nd x =
                                runActionCli (f x) c k a ns ncli nd
                          in runActionCli m c next a s cli d)

instance Monad ActionCli where
  return = returnActionCli
  (>>=)  = bindActionCli

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor ActionCli where
  fmap f m =
    ActionCli (\c k a s cli d ->
               runActionCli m c (\s' cli' d' ->
                                 k s' cli' d' . f) a s cli d)

instance Show (ActionCli a) where
  show _ = "an action"

instance MonadActionRoot ActionCli where
  tryWith exc m =
    ActionCli (\c k a s cli d ->
             let runA msg = runActionCli (exc msg) c k a s cli d
             in runActionCli m c k runA s cli d)
  abortWith msg = ActionCli (\_c _k a _s _cli _d -> a msg)

instance MonadActionRO ActionCli where
  getState      = ActionCli (\_c k _a s cli d -> k s cli d s)
  getsState     = (`fmap` getState)

instance MonadActionIO ActionCli where
  liftIO x = ActionCli (\_c k _a s cli d -> x >>= k s cli d)

instance MonadAction ActionCli where
  modifyState f  = ActionCli (\_c k _a s cli d -> k (f s) cli d ())
  putState       = modifyState . const

instance MonadClientRO ActionCli where
  getsSession f = ActionCli (\c k _a s cli d -> k s cli d (f c))
  getClient     = ActionCli (\_c k _a s cli d -> k s cli d cli)
  getsClient    = (`fmap` getClient)

instance MonadClient ActionCli where
  modifyClient f = ActionCli (\_c k _a s cli d -> k s (f cli) d ())
  putClient      = modifyClient . const

instance MonadClientChan ActionCli where
  getChan        = ActionCli (\_c k _a s cli d -> k s cli d d)
  getsChan       = (`fmap` getChan)
  modifyChan f   = ActionCli (\_c k _a s cli d -> k s cli (f d) ())
  putChan        = modifyChan . const

-- | Run an action, with a given session, state and history, in the @IO@ monad.
executorCli :: ActionCli ()
            -> FrontendSession -> Binding -> ConfigUI
            -> State -> StateClient -> ConnClient -> IO ()
executorCli m sfs sbinding sconfigUI s cli d =
  runActionCli m
    Session{..}
    (\_ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> let err = "unhandled abort for client" <+> showT (getSide s)
                       <+> ":" <+> msg
             in fail $ T.unpack err)
    s
    cli
    d
