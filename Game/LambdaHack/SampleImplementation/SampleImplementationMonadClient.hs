{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleImplementationMonadClient
  ( executorCli
  ) where

import Control.Applicative
import Control.Concurrent.STM
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Maybe
import System.FilePath

import Game.LambdaHack.Atomic.HandleCmdAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.MonadClientUI
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.ProtocolServer

data CliState resp req = CliState
  { cliState   :: !State        -- ^ current global state
  , cliClient  :: !StateClient  -- ^ current client state
  , cliDict    :: !(ChanServer resp req)
                                -- ^ this client connection information
  , cliToSave  :: !(Save.ChanSave (State, StateClient))
                                -- ^ connection to the save thread
  , cliSession :: SessionUI     -- ^ UI setup data, empty for AI clients
  }

-- | Server state transformation monad.
newtype CliImplementation resp req a =
    CliImplementation {runCliImplementation :: StateT (CliState resp req) IO a}
  deriving (Monad, Functor, Applicative)

instance MonadStateRead (CliImplementation resp req) where
  getState    = CliImplementation $ gets cliState
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite (CliImplementation resp req) where
  modifyState f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = f $ cliState cliS}
    in newCliS `seq` ((), newCliS)
  putState    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = s}
    in newCliS `seq` ((), newCliS)

instance MonadClient (CliImplementation resp req) where
  getClient      = CliImplementation $ gets cliClient
  getsClient   f = CliImplementation $ gets $ f . cliClient
  modifyClient f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = f $ cliClient cliS}
    in newCliS `seq` ((), newCliS)
  putClient    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = s}
    in newCliS `seq` ((), newCliS)
  liftIO         = CliImplementation . IO.liftIO
  saveClient     = CliImplementation $ do
    s <- gets cliState
    cli <- gets cliClient
    toSave <- gets cliToSave
    IO.liftIO $ Save.saveToChan toSave (s, cli)

instance MonadClientUI (CliImplementation resp req) where
  getsSession f  = CliImplementation $ gets $ f . cliSession
  liftIO         = CliImplementation . IO.liftIO

instance MonadClientReadResponse resp (CliImplementation resp req) where
  receiveResponse     = CliImplementation $ do
    ChanServer{responseQ} <- gets cliDict
    IO.liftIO $ atomically . readTQueue $ responseQ

instance MonadClientWriteRequest req (CliImplementation resp req) where
  sendRequest scmd = CliImplementation $ do
    ChanServer{requestQ} <- gets cliDict
    IO.liftIO $ atomically . writeTQueue requestQ $ scmd

instance MonadAtomic (CliImplementation resp req) where
  execAtomic = handleCmdAtomic

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: CliImplementation resp req ()
            -> SessionUI -> State -> StateClient -> ChanServer resp req
            -> IO ()
executorCli m cliSession cliState cliClient cliDict =
  let saveFile (_, cli2) =
        fromMaybe "save" (ssavePrefixCli (sdebugCli cli2))
        <.> saveName (sside cli2) (sisAI cli2)
      exe cliToSave =
        evalStateT (runCliImplementation m) CliState{..}
  in Save.wrapInSaves saveFile exe
