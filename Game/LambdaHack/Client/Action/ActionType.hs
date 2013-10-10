{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Client.Action.ActionType
  ( FunActionCli, ActionCli, executorCli
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified System.Random as R

import Game.LambdaHack.Client.Action.ActionClass
import qualified Game.LambdaHack.Client.Action.Save as Save
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
import Game.LambdaHack.Utils.Thread

-- | The type of the function inside any client action.
type FunActionCli c a =
   SessionUI                          -- ^ client UI setup data
   -> (ChanServer c, Save.ChanSave)   -- ^ this client connection information
   -> (State -> StateClient -> a -> IO ())
                                      -- ^ continuation
   -> (R.StdGen -> Msg -> IO ())      -- ^ failure/reset continuation
   -> State                           -- ^ current local state
   -> StateClient                     -- ^ current client state
   -> IO ()

-- | Client parts of actions of human and computer player characters.
newtype ActionCli c a = ActionCli {runActionCli :: FunActionCli c a}

-- | Invokes the action continuation on the provided argument.
returnActionCli :: a -> ActionCli c a
returnActionCli x = ActionCli (\_c _d k _a s cli -> k s cli x)

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

-- TODO: make sure fmap is inlined and all else is inlined here and elsewhere
instance Functor (ActionCli c) where
  fmap f m =
    ActionCli (\c d k a s cli ->
               runActionCli m c d (\s' cli' ->
                                 k s' cli' . f) a s cli)

instance MonadClientAbort (ActionCli c) where
  tryWith exc m  =
    ActionCli (\c d k a s cli ->
             let runA srandom msg =
                   runActionCli (exc msg) c d k a s cli {srandom}
             in runActionCli m c d k runA s cli)
  abortWith msg  = ActionCli (\_c _d _k a _s cli -> a (srandom cli) msg)

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
  saveClient     = ActionCli (\_c (_, toSave) k _a s cli -> do
                                 -- Wipe out previous candidates for saving.
                                 void $ tryTakeMVar toSave
                                 putMVar toSave $ Just (s, cli)
                                 k s cli ())

instance MonadClientUI (ActionCli c) where
  getsSession f  = ActionCli (\c _d k _a s cli -> k s cli (f c))

instance MonadClientReadServer c (ActionCli c) where
  readServer     =
    ActionCli (\_c (ChanServer{..}, _) k _a s cli -> do
                  ccmd <- atomically . readTQueue $ fromServer
                  k s cli ccmd)

instance MonadClientWriteServer (ActionCli c) where
  writeServer scmd =
    ActionCli (\_c (ChanServer{..}, _) k _a s cli -> do
                  atomically . writeTQueue toServer $ scmd
                  k s cli ())

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: ActionCli c ()
            -> SessionUI -> State -> StateClient -> ChanServer c
            -> IO ()
executorCli m sess s cli d = do
  childrenClient <- newMVar []
  toSave <- newEmptyMVar
  void $ forkChild childrenClient $ Save.loopSave toSave
  runActionCli m
    sess
    (d, toSave)
    (\_ _ _ -> return ())
    (\_ msg -> let err = "unhandled abort for client"
                         <+> showT (sfactionD s EM.! sside cli)
                         <+> ":" <+> msg
               in fail $ T.unpack err)
    s
    cli
  -- Wait until the last save starts and tell the save thread to end.
  putMVar toSave Nothing
  -- Wait until the save thread ends.
  waitForChildren childrenClient
