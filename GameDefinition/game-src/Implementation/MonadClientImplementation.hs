{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game client monads. Just as any other
-- component of the library, this implementation can be substituted.
module Implementation.MonadClientImplementation
  ( executorCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , CliState(..), CliImplementation(..)
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import           Control.Monad.Trans.State.Strict hiding (State)

import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleAtomicM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.LoopM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.MonadStateRead
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Server (ChanServer (..))

data CliState = CliState
  { cliState   :: State            -- ^ current global state
  , cliClient  :: StateClient      -- ^ current client state
  , cliSession :: Maybe SessionUI  -- ^ UI state, empty for AI clients
  , cliDict    :: ChanServer       -- ^ this client connection information
  , cliToSave  :: Save.ChanSave (StateClient, Maybe SessionUI)
                                   -- ^ connection to the save thread
  }

-- | Client state transformation monad.
newtype CliImplementation a = CliImplementation
  { runCliImplementation :: StateT CliState IO a }
  deriving (Monad, Functor, Applicative)

instance MonadStateRead CliImplementation where
  {-# INLINE getsState #-}
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite CliImplementation where
  {-# INLINE modifyState #-}
  modifyState f = CliImplementation $ state $ \cliS ->
    let !newCliS = cliS {cliState = f $ cliState cliS}
    in ((), newCliS)
  {-# INLINE putState #-}
  putState newCliState = CliImplementation $ state $ \cliS ->
    let !newCliS = cliS {cliState = newCliState}
    in ((), newCliS)

instance MonadClientRead CliImplementation where
  {-# INLINE getsClient #-}
  getsClient f = CliImplementation $ gets $ f . cliClient
  liftIO = CliImplementation . IO.liftIO

instance MonadClient CliImplementation where
  {-# INLINE modifyClient #-}
  modifyClient f = CliImplementation $ state $ \cliS ->
    let !newCliS = cliS {cliClient = f $ cliClient cliS}
    in ((), newCliS)

instance MonadClientSetup CliImplementation where
  saveClient = CliImplementation $ do
    toSave <- gets cliToSave
    cli <- gets cliClient
    msess <- gets cliSession
    IO.liftIO $ Save.saveToChan toSave (cli, msess)

instance MonadClientUI CliImplementation where
  {-# INLINE getsSession #-}
  getsSession f = CliImplementation $ gets $ f . fromJust . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliImplementation $ state $ \cliS ->
    let !newCliSession = f $ fromJust $ cliSession cliS
        !newCliS = cliS {cliSession = Just newCliSession}
    in ((), newCliS)
  updateClientLeader aid = do
    s <- getState
    modifyClient $ updateLeader aid s
  getCacheBfs = BfsM.getCacheBfs
  getCachePath = BfsM.getCachePath

instance MonadClientReadResponse CliImplementation where
  receiveResponse = CliImplementation $ do
    ChanServer{responseS} <- gets cliDict
    IO.liftIO $ takeMVar responseS

instance MonadClientWriteRequest CliImplementation where
  sendRequestAI scmd = CliImplementation $ do
    ChanServer{requestAIS} <- gets cliDict
    IO.liftIO $ putMVar requestAIS scmd
  sendRequestUI scmd = CliImplementation $ do
    ChanServer{requestUIS} <- gets cliDict
    IO.liftIO $ putMVar (fromJust requestUIS) scmd
  clientHasUI = CliImplementation $ do
    mSession <- gets cliSession
    return $! isJust mSession

instance MonadClientAtomic CliImplementation where
  {-# INLINE execUpdAtomic #-}
  execUpdAtomic _ = return ()  -- handleUpdAtomic, until needed, save resources
    -- Don't catch anything; assume exceptions impossible.
  {-# INLINE execPutState #-}
  execPutState = putState

-- | Run the main client loop, with the given arguments and empty
-- initial states, in the @IO@ monad.
executorCli :: CCUI -> UIOptions -> ClientOptions -> Bool
            -> COps
            -> FactionId
            -> ChanServer
            -> IO ()
executorCli ccui sUIOptions clientOptions startsNewGame
            cops@COps{corule} fid cliDict =
  let cliSession | isJust (requestUIS cliDict) =
                     Just $ emptySessionUI sUIOptions
                 | otherwise = Nothing
      stateToFileName (cli, _) =
        ssavePrefixCli (soptions cli) <> Save.saveNameCli corule (sside cli)
      totalState cliToSave = CliState
        { cliState = updateCOpsAndCachedData (const cops) emptyState
            -- state is empty, so the cached data is left empty and untouched
        , cliClient = emptyStateClient fid
        , cliDict
        , cliToSave
        , cliSession
        }
      m = loopCli ccui sUIOptions clientOptions startsNewGame
      exe = evalStateT (runCliImplementation m) . totalState
  in Save.wrapInSaves cops stateToFileName exe
