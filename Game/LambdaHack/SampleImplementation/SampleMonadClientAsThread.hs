{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleMonadClientAsThread
  ( executorCliAsThread
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , CliImplementation
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Binary
import System.FilePath

import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.FileM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.ProtocolM hiding (saveName)

data CliState sess = CliState
  { cliState   :: !State        -- ^ current global state
  , cliClient  :: !StateClient  -- ^ current client state
  , cliDict    :: !ChanServer   -- ^ this client connection information
  , cliToSave  :: !(Save.ChanSave (State, StateClient, sess))
                                -- ^ connection to the save thread
  , cliSession :: !sess         -- ^ UI state, empty for AI clients
  }

-- | Client state transformation monad.
newtype CliImplementation sess a = CliImplementation
  { runCliImplementation :: StateT (CliState sess) IO a }
  deriving (Monad, Functor, Applicative)

instance MonadStateRead (CliImplementation sess) where
  {-# INLINE getsState #-}
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite (CliImplementation sess) where
  {-# INLINE modifyState #-}
  modifyState f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliState cliS
    in ((), cliS {cliState = newCliState})

instance MonadClient (CliImplementation sess) where
  {-# INLINE getsClient #-}
  getsClient   f = CliImplementation $ gets $ f . cliClient
  {-# INLINE modifyClient #-}
  modifyClient f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliClient cliS
    in ((), cliS {cliClient = newCliState})
  {-# INLINE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

instance MonadClientSetup (CliImplementation ()) where
  {-# INLINE saveClient #-}
  saveClient = CliImplementation $ do
    toSave <- gets cliToSave
    s <- gets cliState
    cli <- gets cliClient
    IO.liftIO $ Save.saveToChan toSave (s, cli, ())
  {-# INLINE restartClient #-}
  restartClient = return ()

instance MonadClientSetup (CliImplementation SessionUI) where
  {-# INLINE saveClient #-}
  saveClient = CliImplementation $ do
    toSave <- gets cliToSave
    s <- gets cliState
    cli <- gets cliClient
    sess <- gets cliSession
    IO.liftIO $ Save.saveToChan toSave (s, cli, sess)
  {-# INLINE restartClient #-}
  restartClient  = CliImplementation $ state $ \cliS ->
    let sess = cliSession cliS
        !newSess = (emptySessionUI (sconfig sess))
                     { schanF = schanF sess
                     , sbinding = sbinding sess
                     , shistory = shistory sess
                     , _sreport = _sreport sess
                     , sstart = sstart sess
                     , sgstart = sgstart sess
                     , sallTime = sallTime sess
                     , snframes = snframes sess
                     , sallNframes = sallNframes sess
                     }
    in ((), cliS {cliSession = newSess})

instance MonadClientUI (CliImplementation SessionUI) where
  {-# INLINE getsSession #-}
  getsSession   f = CliImplementation $ gets $ f . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliImplementation $ state $ \cliS ->
    let !newCliSession = f $ cliSession cliS
    in ((), cliS {cliSession = newCliSession})
  {-# INLINE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

instance MonadClientReadResponse (CliImplementation sess) where
  {-# INLINE receiveResponse #-}
  receiveResponse = CliImplementation $ do
    ChanServer{responseS} <- gets cliDict
    IO.liftIO $ takeMVar responseS

instance MonadClientWriteRequest (CliImplementation sess) where
  {-# INLINE sendRequest #-}
  sendRequest scmd = CliImplementation $ do
    ChanServer{requestS} <- gets cliDict
    IO.liftIO $ putMVar requestS scmd

-- | The game-state semantics of atomic commands
-- as computed on the client.
instance MonadAtomic (CliImplementation sess) where
  execUpdAtomic cmd = handleUpdAtomic cmd
  execSfxAtomic _sfx = return ()

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCliAsThread :: Binary sess
                    => Bool
                    -> CliImplementation sess ()
                    -> sess
                    -> Kind.COps
                    -> FactionId
                    -> ChanServer
                    -> IO ()
{-# INLINE executorCliAsThread #-}
executorCliAsThread isAI m cliSession cops fid cliDict =
  let saveFile (_, cli, _) =
        ssavePrefixCli (sdebugCli cli)
        <.> saveName (sside cli) isAI
      totalState cliToSave = CliState
        { cliState = emptyState cops
        , cliClient = emptyStateClient fid
        , cliDict
        , cliToSave
        , cliSession
        }
      exe = evalStateT (runCliImplementation m) . totalState
  in Save.wrapInSaves tryCreateDir encodeEOF saveFile exe
