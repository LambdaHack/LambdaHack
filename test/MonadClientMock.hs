{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game client monads. Just as any other
-- component of the library, this implementation can be substituted.
module MonadClientMock
  ( emptyCliState
  , executorCli
-- #ifdef EXPOSE_INTERNAL
--     -- * Internal operations
  , CliMock(..)
-- #endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
--import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT), gets, state, evalStateT )

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

-- just for test code
import           Game.LambdaHack.Client.UI.HandleHelperM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import Game.LambdaHack.Definition.DefsInternal ( toContentSymbol )



data CliState = CliState
  { cliState   :: State            -- ^ current global state
  , cliClient  :: StateClient      -- ^ current client state
  , cliSession :: Maybe SessionUI  -- ^ UI state, empty for AI clients
  -- , cliDict    :: ChanServer       -- ^ this client connection information
  -- , cliToSave  :: Save.ChanSave (StateClient, Maybe SessionUI)
  }

emptyCliState :: CliState
emptyCliState = CliState
  { cliState = emptyState 
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Nothing 
  -- , cliDict = undefined 
  -- , cliToSave = undefined 
  }  

-- | Client state transformation monad.
newtype CliMock a = CliMock
  { runCliMock :: StateT CliState IO a }  -- we build off io so we can compile but we don't want to use it
  deriving (Monad, Functor, Applicative)

instance MonadStateRead CliMock where
  {-# INLINE getsState #-}
  getsState f = CliMock $ gets $ f . cliState

instance MonadStateWrite CliMock where
  {-# INLINE modifyState #-}
  modifyState f = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = f $ cliState cliS}
    in ((), newCliS)
  {-# INLINE putState #-}
  putState newCliState = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = newCliState}
    in ((), newCliS)

instance MonadClientRead CliMock where
  {-# INLINE getsClient #-}
  getsClient f = CliMock $ gets $ f . cliClient
  liftIO = CliMock . IO.liftIO

instance MonadClient CliMock where
  {-# INLINE modifyClient #-}
  modifyClient f = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliClient = f $ cliClient cliS}
    in ((), newCliS)

-- instance MonadClientSetup CliMock where
--   saveClient = CliMock $ do
--     --toSave <- gets cliToSave
--     cli <- gets cliClient
--     msess <- gets cliSession
--     IO.liftIO $ Save.saveToChan toSave (cli, msess)

instance MonadClientUI CliMock where
  {-# INLINE getsSession #-}
  getsSession f = CliMock $ gets $ f . fromJust . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliMock $ state $ \cliS ->
    let !newCliSession = f $ fromJust $ cliSession cliS
        !newCliS = cliS {cliSession = Just newCliSession}
    in ((), newCliS)
  updateClientLeader aid = do
    s <- getState
    modifyClient $ updateLeader aid s
  getCacheBfs = BfsM.getCacheBfs
  getCachePath = BfsM.getCachePath

-- instance MonadClientReadResponse CliMock where
--   receiveResponse = CliMock $ do
--     ChanServer{responseS} <- gets cliDict
--     IO.liftIO $ takeMVar responseS

-- instance MonadClientWriteRequest CliMock where
--   sendRequestAI scmd = CliMock $ do
--     ChanServer{requestAIS} <- gets cliDict
--     IO.liftIO $ putMVar requestAIS scmd
--   sendRequestUI scmd = CliMock $ do
--     ChanServer{requestUIS} <- gets cliDict
--     IO.liftIO $ putMVar (fromJust requestUIS) scmd
--   clientHasUI = CliMock $ do
--     mSession <- gets cliSession
--     return $! isJust mSession

instance MonadClientAtomic CliMock where
  {-# INLINE execUpdAtomic #-}
  execUpdAtomic _ = return ()  -- handleUpdAtomic, until needed, save resources
    -- Don't catch anything; assume exceptions impossible.
  {-# INLINE execPutState #-}
  execPutState = putState


executorCli :: CliMock a -> IO a 
executorCli testFn = 
  evalStateT (runCliMock testFn) emptyCliState 
  
