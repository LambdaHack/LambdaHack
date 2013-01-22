{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Client.Action.ActionType
  ( FunActionCli, ActionCli, executorCli
  ) where

import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.State
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- TODO: check if we can move factionPerception from state to Reader
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

-- | Client parts of actions of human and computer player characters.
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

instance MonadActionAbort ActionCli where
  tryWith exc m =
    ActionCli (\c k a s cli d ->
             let runA msg = runActionCli (exc msg) c k a s cli d
             in runActionCli m c k runA s cli d)
  abortWith msg = ActionCli (\_c _k a _s _cli _d -> a msg)

instance MonadActionRO ActionCli where
  getState      = ActionCli (\_c k _a s cli d -> k s cli d s)
  getsState     = (`fmap` getState)

instance MonadAction ActionCli where
  modifyState f  = ActionCli (\_c k _a s cli d -> k (f s) cli d ())
  putState       = modifyState . const

instance MonadClientRO ActionCli where
  getClient     = ActionCli (\_c k _a s cli d -> k s cli d cli)
  getsClient    = (`fmap` getClient)

instance MonadClient ActionCli where
  modifyClient f = ActionCli (\_c k _a s cli d -> k s (f cli) d ())
  putClient      = modifyClient . const
  liftIO x       = ActionCli (\_c k _a s cli d -> x >>= k s cli d)

instance MonadClientUI ActionCli where
  getsSession f = ActionCli (\c k _a s cli d -> k s cli d (f c))

instance MonadClientChan ActionCli where
  getChan        = ActionCli (\_c k _a s cli d -> k s cli d d)
  getsChan       = (`fmap` getChan)
  modifyChan f   = ActionCli (\_c k _a s cli d -> k s cli (f d) ())
  putChan        = modifyChan . const

-- | Run an action, with a given session, state and history, in the @IO@ monad.
executorCli :: ActionCli () -> Session -> State -> StateClient -> ConnClient
            -> IO ()
executorCli m sess s cli d =
  runActionCli m
    sess
    (\_ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> let err = "unhandled abort for client" <+> showT (getSide s)
                       <+> ":" <+> msg
             in fail $ T.unpack err)
    s
    cli
    d
