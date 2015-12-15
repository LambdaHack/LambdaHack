{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleMonadClient
  ( executorCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , CliImplementation
#endif
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.STM
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import System.FilePath

import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.FileClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
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

-- | Client state transformation monad.
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
  saveChanClient = CliImplementation $ gets cliToSave

instance MonadClientUI (CliImplementation resp req) where
  putSession   s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliSession = s}
    in newCliS `seq` ((), newCliS)
  getsSession f  = CliImplementation $ gets $ f . cliSession

instance MonadClientReadResponse resp (CliImplementation resp req) where
  receiveResponse     = CliImplementation $ do
    ChanServer{responseS} <- gets cliDict
    IO.liftIO $ atomically . readTQueue $ responseS

instance MonadClientWriteRequest req (CliImplementation resp req) where
  sendRequest scmd = CliImplementation $ do
    ChanServer{requestS} <- gets cliDict
    IO.liftIO $ atomically . writeTQueue requestS $ scmd

-- | The game-state semantics of atomic commands
-- as computed on the client.
instance MonadAtomic (CliImplementation resp req) where
  execAtomic = handleCmdAtomic

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: Kind.COps
            -> CliImplementation resp req ()
            -> FactionId
            -> ChanServer resp req
            -> IO ()
executorCli cops m fid cliDict =
  let saveFile (_, cli) =
        ssavePrefixCli (sdebugCli cli)
        <.> saveName (sside cli) (sisAI cli)
      totalState cliToSave = CliState
        { cliState = emptyState cops
        , cliClient = emptyStateClient fid
        , cliDict
        , cliToSave
        , cliSession = undefined
        }
      exe = evalStateT (runCliImplementation m) . totalState
  in Save.wrapInSaves tryCreateDir encodeEOF saveFile exe
