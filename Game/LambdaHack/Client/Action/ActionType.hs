{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Client.Action.ActionType
  ( FunActionCli, ActionCli, executorCli
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- | The type of the function inside any client action.
type FunActionCli c a =
   SessionUI                          -- ^ client UI setup data
   -> Conn c                       -- ^ this client connection information
   -> (State -> StateClient -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current local state
   -> StateClient                     -- ^ current client state
   -> IO ()

-- | Client parts of actions of human and computer player characters.
newtype ActionCli c a = ActionCli {runActionCli :: FunActionCli c a}

-- | Invokes the action continuation on the provided argument.
returnActionCli :: a -> ActionCli c a
returnActionCli x = ActionCli (\_c _d k _a s cli  -> k s cli x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindActionCli :: ActionCli c a -> (a -> ActionCli c b) -> ActionCli c b
bindActionCli m f = ActionCli (\c d k a s cli ->
                          let next ns ncli x =
                                runActionCli (f x) c d k a ns ncli
                          in runActionCli m c d next a s cli)

instance Monad (ActionCli c) where
  return = returnActionCli
  (>>=)  = bindActionCli

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor (ActionCli c) where
  fmap f m =
    ActionCli (\c d k a s cli ->
               runActionCli m c d (\s' cli' ->
                                 k s' cli' . f) a s cli)

instance Show (ActionCli c a) where
  show _ = "an action"

instance MonadActionAbort (ActionCli c) where
  tryWith exc m  =
    ActionCli (\c d k a s cli ->
             let runA msg = runActionCli (exc msg) c d k a s cli
             in runActionCli m c d k runA s cli)
  abortWith msg  = ActionCli (\_c _d _k a _s _cli -> a msg)

instance MonadActionRO (ActionCli c) where
  getState       = ActionCli (\_c _d k _a s cli -> k s cli s)
  getsState      = (`fmap` getState)

instance MonadAction (ActionCli c) where
  modifyState f  = ActionCli (\_c _d k _a s cli -> k (f s) cli ())
  putState       = modifyState . const

instance MonadClient (ActionCli c) where
  getClient      = ActionCli (\_c _d k _a s cli -> k s cli cli)
  getsClient     = (`fmap` getClient)
  modifyClient f = ActionCli (\_c _d k _a s cli -> k s (f cli) ())
  putClient      = modifyClient . const
  liftIO x       = ActionCli (\_c _d k _a s cli -> x >>= k s cli)

instance MonadClientUI (ActionCli c) where
  getsSession f  = ActionCli (\c _d k _a s cli -> k s cli (f c))

instance MonadClientConn c (ActionCli c) where
  getsConn f  = ActionCli (\_c d k _a s cli -> k s cli (f d))

-- | Run an action, with a given session, state and history, in the @IO@ monad.
executorCli :: ActionCli c () -> SessionUI -> State -> StateClient -> Conn c
            -> IO ()
executorCli m sess s cli d =
  runActionCli m
    sess
    d
    (\_ _ _ -> return ())
    (\msg -> let err = "unhandled abort for client"
                       <+> showT (sfaction s EM.! sside cli)
                       <+> ":" <+> msg
             in fail $ T.unpack err)
    s
    cli
