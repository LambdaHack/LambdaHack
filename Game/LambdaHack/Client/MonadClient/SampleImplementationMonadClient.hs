{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Client.MonadClient.SampleImplementationMonadClient
  ( ActionCli, executorCli
  ) where

import Control.Applicative
import Control.Concurrent.STM
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Maybe
import System.FilePath

import Game.LambdaHack.Client.MonadClient.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Response
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State

data CliState c d = CliState
  { cliState   :: !State             -- ^ current global state
  , cliClient  :: !StateClient       -- ^ current client state
  , cliDict    :: !(ChanServer c d)  -- ^ this client connection information
  , cliToSave  :: !(Save.ChanSave (State, StateClient))
                                     -- ^ connection to the save thread
  , cliSession :: SessionUI          -- ^ UI setup data, empty for AI clients
  }

-- | Server state transformation monad.
newtype ActionCli c d a =
    ActionCli {runActionCli :: StateT (CliState c d) IO a}
  deriving (Monad, Functor, Applicative)

instance MonadActionRO (ActionCli c d) where
  getState    = ActionCli $ gets cliState
  getsState f = ActionCli $ gets $ f . cliState

instance MonadAction (ActionCli c d) where
  modifyState f = ActionCli $ state $ \cliS ->
    let newCliS = cliS {cliState = f $ cliState cliS}
    in newCliS `seq` ((), newCliS)
  putState    s = ActionCli $ state $ \cliS ->
    let newCliS = cliS {cliState = s}
    in newCliS `seq` ((), newCliS)

instance MonadClient (ActionCli c d) where
  getClient      = ActionCli $ gets cliClient
  getsClient   f = ActionCli $ gets $ f . cliClient
  modifyClient f = ActionCli $ state $ \cliS ->
    let newCliS = cliS {cliClient = f $ cliClient cliS}
    in newCliS `seq` ((), newCliS)
  putClient    s = ActionCli $ state $ \cliS ->
    let newCliS = cliS {cliClient = s}
    in newCliS `seq` ((), newCliS)
  liftIO         = ActionCli . IO.liftIO
  saveClient     = ActionCli $ do
    s <- gets cliState
    cli <- gets cliClient
    toSave <- gets cliToSave
    IO.liftIO $ Save.saveToChan toSave (s, cli)

instance MonadClientUI (ActionCli c d) where
  getsSession f  = ActionCli $ gets $ f . cliSession

instance MonadClientReadServer c (ActionCli c d) where
  readServer     = ActionCli $ do
    ChanServer{fromServer} <- gets cliDict
    IO.liftIO $ atomically . readTQueue $ fromServer

instance MonadClientWriteServer d (ActionCli c d) where
  writeServer scmd = ActionCli $ do
    ChanServer{toServer} <- gets cliDict
    IO.liftIO $ atomically . writeTQueue toServer $ scmd

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: ActionCli c d ()
            -> SessionUI -> State -> StateClient -> ChanServer c d
            -> IO ()
executorCli m cliSession cliState cliClient cliDict =
  let saveFile (_, cli2) =
        fromMaybe "save" (ssavePrefixCli (sdebugCli cli2))
        <.> saveName (sside cli2) (sisAI cli2)
      exe cliToSave =
        evalStateT (runActionCli m) CliState{..}
  in Save.wrapInSaves saveFile exe
