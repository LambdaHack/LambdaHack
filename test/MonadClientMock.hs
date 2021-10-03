{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game client monads. Just as any other
-- component of the library, this implementation can be substituted.
module MonadClientMock
  ( emptyCliState
  , executorCli
-- #ifdef EXPOSE_INTERNAL
--     -- * Internal operations
  , MonadClientMock(..)
-- #endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
--import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict
    ( StateT(StateT), gets, state, evalStateT )

import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleAtomicM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.LoopM
import           Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State

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
  }

emptyCliState :: CliState
emptyCliState = CliState
  { cliState = emptyState 
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Nothing 
  , cliDict = undefined 
  , cliToSave = undefined 
  }  

-- | Client state transformation monad.
newtype MonadClientMock a = MonadClientMock
  { runMonadClientMock :: StateT CliState IO a }  -- we build off io so we can compile but we don't want to use it
  deriving (Monad, Functor, Applicative)

instance MonadStateRead MonadClientMock where
  {-# INLINE getsState #-}
  getsState f = MonadClientMock $ gets $ f . cliState

instance MonadStateWrite MonadClientMock where
  {-# INLINE modifyState #-}
  modifyState f = MonadClientMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = f $ cliState cliS}
    in ((), newCliS)
  {-# INLINE putState #-}
  putState newCliState = MonadClientMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = newCliState}
    in ((), newCliS)

instance MonadClientRead MonadClientMock where
  {-# INLINE getsClient #-}
  getsClient f = MonadClientMock $ gets $ f . cliClient
  liftIO = return undefined

instance MonadClient MonadClientMock where
  {-# INLINE modifyClient #-}
  modifyClient f = MonadClientMock $ state $ \cliS ->
    let !newCliS = cliS {cliClient = f $ cliClient cliS}
    in ((), newCliS)

-- instance MonadClientSetup MonadClientMock where
--   saveClient = MonadClientMock $ do
--     --toSave <- gets cliToSave
--     cli <- gets cliClient
--     msess <- gets cliSession
--     IO.liftIO $ Save.saveToChan toSave (cli, msess)

instance MonadClientUI MonadClientMock where
  {-# INLINE getsSession #-}
  getsSession f = MonadClientMock $ gets $ f . fromJust . cliSession
  {-# INLINE modifySession #-}
  modifySession f = MonadClientMock $ state $ \cliS ->
    let !newCliSession = f $ fromJust $ cliSession cliS
        !newCliS = cliS {cliSession = Just newCliSession}
    in ((), newCliS)
  updateClientLeader aid = do
    s <- getState
    modifyClient $ updateLeader aid s
  getCacheBfs = BfsM.getCacheBfs
  getCachePath = BfsM.getCachePath

-- instance MonadClientReadResponse MonadClientMock where
--   receiveResponse = MonadClientMock $ do
--     ChanServer{responseS} <- gets cliDict
--     IO.liftIO $ takeMVar responseS

-- instance MonadClientWriteRequest MonadClientMock where
--   sendRequestAI scmd = MonadClientMock $ do
--     ChanServer{requestAIS} <- gets cliDict
--     IO.liftIO $ putMVar requestAIS scmd
--   sendRequestUI scmd = MonadClientMock $ do
--     ChanServer{requestUIS} <- gets cliDict
--     IO.liftIO $ putMVar (fromJust requestUIS) scmd
--   clientHasUI = MonadClientMock $ do
--     mSession <- gets cliSession
--     return $! isJust mSession

instance MonadClientAtomic MonadClientMock where
  {-# INLINE execUpdAtomic #-}
  execUpdAtomic _ = return ()  -- handleUpdAtomic, until needed, save resources
    -- Don't catch anything; assume exceptions impossible.
  {-# INLINE execPutState #-}
  execPutState = putState

-- cliStubFn :: ( MonadClientSetup m
--            , MonadClientUI m
--            , MonadClientAtomic m
--            , MonadClientReadResponse m
--            , MonadClientWriteRequest m )
--         => CCUI -> UIOptions -> ClientOptions -> m ()
-- cliStubFn ccui sUIOptions clientOptions = return undefined 

-- | Run the main client loop, with the given arguments and empty
-- initial states
executorCli :: CCUI -> UIOptions -> ClientOptions
            -> COps
            -> Bool
            -> FactionId
            -> ChanServer
            -> IO ()
executorCli ccui sUIOptions clientOptions cops@COps{corule} isUI fid cliDict =
--  return Just True
  let cliSession | isUI = Just $ emptySessionUI sUIOptions
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
      m = loopCli ccui sUIOptions clientOptions
      exe = evalStateT (runMonadClientMock m) . totalState
  in Save.wrapInSaves cops stateToFileName exe
