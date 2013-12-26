{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Client.Action.ActionType
  ( FunActionCli, ActionCli, executorCli
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import qualified Data.Text as T
import System.FilePath
import qualified System.Random as R

import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Msg
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State

-- | The type of the function inside any client action.
type FunActionCli c d a =
   SessionUI                          -- ^ client UI setup data
   -> (ChanServer c d, Save.ChanSave (State, StateClient))
                                      -- ^ this client connection information
   -> (State -> StateClient -> a -> IO ())
                                      -- ^ continuation
   -> (R.StdGen -> Msg -> IO ())      -- ^ failure/reset continuation
   -> State                           -- ^ current local state
   -> StateClient                     -- ^ current client state
   -> IO ()

-- | Client parts of actions of human and computer player characters.
newtype ActionCli c d a = ActionCli {runActionCli :: FunActionCli c d a}

-- | Invokes the action continuation on the provided argument.
returnActionCli :: a -> ActionCli c d a
returnActionCli x = ActionCli (\_c _d k _a s cli -> k s cli x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindActionCli :: ActionCli c d a -> (a -> ActionCli c d b) -> ActionCli c d b
bindActionCli m f = ActionCli (\c d k a s cli ->
                          let next ns ncli x =
                                runActionCli (f x) c d k a ns ncli
                          in runActionCli m c d next a s cli)

instance Monad (ActionCli c d) where
  return = returnActionCli
  (>>=)  = bindActionCli

instance Applicative (ActionCli c d) where
    pure  = return
    (<*>) = ap

-- TODO: make sure fmap is inlined and all else is inlined here and elsewhere
instance Functor (ActionCli c d) where
  fmap f m =
    ActionCli (\c d k a s cli ->
               runActionCli m c d (\s' cli' ->
                                 k s' cli' . f) a s cli)

instance MonadActionRO (ActionCli c d) where
  getState       = ActionCli (\_c _d k _a s cli -> k s cli s)
  getsState      = (`fmap` getState)

instance MonadAction (ActionCli c d) where
  modifyState f  = ActionCli (\_c _d k _a s cli -> k (f s) cli ())
  putState       = modifyState . const

instance MonadClient (ActionCli c d) where
  getClient      = ActionCli (\_c _d k _a s cli -> k s cli cli)
  getsClient     = (`fmap` getClient)
  modifyClient f = ActionCli (\_c _d k _a s cli -> k s (f cli) ())
  putClient      = modifyClient . const
  liftIO x       = ActionCli (\_c _d k _a s cli -> x >>= k s cli)
  saveClient     = ActionCli (\_c (_, toSave) k _a s cli -> do
                                 Save.saveToChan toSave (s, cli)
                                 k s cli ())

instance MonadClientUI (ActionCli c d) where
  getsSession f  = ActionCli (\c _d k _a s cli -> k s cli (f c))

instance MonadClientReadServer c (ActionCli c d) where
  readServer     =
    ActionCli (\_c (ChanServer{..}, _) k _a s cli -> do
                  ccmd <- atomically . readTQueue $ fromServer
                  k s cli ccmd)

instance MonadClientWriteServer d (ActionCli c d) where
  writeServer scmd =
    ActionCli (\_c (ChanServer{..}, _) k _a s cli -> do
                  atomically . writeTQueue toServer $ scmd
                  k s cli ())

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: ActionCli c d ()
            -> SessionUI -> State -> StateClient -> ChanServer c d
            -> IO ()
executorCli m sess s cli d =
  let saveFile (_, cli2) =
        configAppDataDir (sconfigUI cli2)
        </> fromMaybe "save" (ssavePrefixCli (sdebugCli cli2))
        <.> saveName (sside cli2) (sisAI cli2)
      exe toSave =
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
  in Save.wrapInSaves saveFile exe
