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

import Game.LambdaHack.Common.Prelude

import Control.Concurrent.STM
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Binary
import System.FilePath

import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.FileClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.ProtocolServer

data CliState sess resp req = CliState
  { cliState   :: !State        -- ^ current global state
  , cliClient  :: !StateClient  -- ^ current client state
  , cliDict    :: !(ChanServer resp req)
                                -- ^ this client connection information
  , cliToSave  :: !(Save.ChanSave (State, StateClient, sess))
                                -- ^ connection to the save thread
  , cliSession :: !sess         -- ^ UI state, empty for AI clients
  }

-- | Client state transformation monad.
newtype CliImplementation sess resp req a = CliImplementation
  { runCliImplementation :: StateT (CliState sess resp req) IO a }
  deriving (Monad, Functor, Applicative)

instance MonadStateRead (CliImplementation sess resp req) where
  getState    = CliImplementation $ gets cliState
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite (CliImplementation sess resp req) where
  modifyState f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = f $ cliState cliS}
    in newCliS `seq` ((), newCliS)
  putState    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = s}
    in newCliS `seq` ((), newCliS)

instance MonadClient (CliImplementation sess resp req) where
  getClient      = CliImplementation $ gets cliClient
  getsClient   f = CliImplementation $ gets $ f . cliClient
  modifyClient f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = f $ cliClient cliS}
    in newCliS `seq` ((), newCliS)
  putClient    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = s}
    in newCliS `seq` ((), newCliS)
  liftIO         = CliImplementation . IO.liftIO

instance MonadClientSetup (CliImplementation () resp req) where
  saveClient     = CliImplementation $ do
    toSave <- gets cliToSave
    s <- gets cliState
    cli <- gets cliClient
    IO.liftIO $ Save.saveToChan toSave (s, cli, ())
  restartClient  = return ()

instance MonadClientSetup (CliImplementation SessionUI resp req) where
  saveClient     = CliImplementation $ do
    toSave <- gets cliToSave
    s <- gets cliState
    cli <- gets cliClient
    sess <- gets cliSession
    IO.liftIO $ Save.saveToChan toSave (s, cli, sess)
  restartClient  = CliImplementation $ state $ \cliS ->
    let sess = cliSession cliS
        newSess = (emptySessionUI (sconfig sess))
                    { schanF = schanF sess
                    , sbinding = sbinding sess
                    , shistory = shistory sess
                    , _sreport = _sreport sess }
        newCliS = cliS {cliSession = newSess}
    in newCliS `seq` ((), newCliS)

instance MonadClientUI (CliImplementation SessionUI resp req) where
  getSession    = CliImplementation $ gets cliSession
  getsSession f  = CliImplementation $ gets $ f . cliSession
  modifySession f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliSession = f $ cliSession cliS}
    in newCliS `seq` ((), newCliS)
  putSession   s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliSession = s}
    in newCliS `seq` ((), newCliS)

instance MonadClientReadResponse resp (CliImplementation sess resp req) where
  receiveResponse     = CliImplementation $ do
    ChanServer{responseS} <- gets cliDict
    IO.liftIO $ atomically . readTQueue $ responseS

instance MonadClientWriteRequest req (CliImplementation sess resp req) where
  sendRequest scmd = CliImplementation $ do
    ChanServer{requestS} <- gets cliDict
    IO.liftIO $ atomically . writeTQueue requestS $ scmd

-- | The game-state semantics of atomic commands
-- as computed on the client.
instance MonadAtomic (CliImplementation sess resp req) where
  execAtomic = handleCmdAtomic

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: Binary sess
            => Kind.COps
            -> sess
            -> CliImplementation sess resp req ()
            -> FactionId
            -> ChanServer resp req
            -> IO ()
executorCli cops cliSession m fid cliDict =
  let saveFile (_, cli, _) =
        ssavePrefixCli (sdebugCli cli)
        <.> saveName (sside cli) (sisAI cli)
      totalState cliToSave = CliState
        { cliState = emptyState cops
        , cliClient = emptyStateClient fid
        , cliDict
        , cliToSave
        , cliSession
        }
      exe = evalStateT (runCliImplementation m) . totalState
  in Save.wrapInSaves tryCreateDir encodeEOF saveFile exe
