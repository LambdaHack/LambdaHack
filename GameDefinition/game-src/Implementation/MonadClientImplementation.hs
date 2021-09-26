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
import           System.Timeout

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
  receiveResponseWithTimeout t = CliImplementation $ do
    ChanServer{responseS} <- gets cliDict
    IO.liftIO $ timeout t $ takeMVar responseS
      -- TODO: this loses values sometimes, see https://stackoverflow.com/questions/22171895/using-tchan-with-timeout
      -- it will probably require STM to be fixed, either internally or even the variable can't be a simple MVar, which is a pity;
      -- here is a program that loses values and locks up occasionally, just as LH with this feature:
{-

import Control.Concurrent
import Control.Exception
import System.Timeout

main :: IO ()
main = do
  mv <- newMVar True
  let loopTake c = do
        mb <- timeout 1 (mask_ $ takeMVar mv)
        case mb of
          Just True -> print c
          Just False -> loopTake (c + 1)
          Nothing -> loopTake c
      loopPut n =
        if n == 0 then return () else do
          putMVar mv False
          loopPut (n - 1)
      oneMillion = 10000
  loopTake 0
  forkIO $ loopPut oneMillion >> putMVar mv True
  loopTake 0


remaining things to do if going the timeout way:

time out after real time spend without a query, not after a given number
of game state change commands

wrong: I gave server command, it ignored it and I didn't get control back
automatically, but messages scrolled too fast
this should be:
if command given then any game state change command causes interrupt
>
test this by disabling underAI keys eating

benchmark and check that results identical and if much slower

use this stuff to make UI available in AI mode
but only in game with no leader-having faction
this will also simplify code for games with leader
>
but possibly a bigger problem and more code is to permit
doing UI stuff without a leader
>
so perhaps a better start is to permit spectacors
though that's also no leader
but I can make a fake leader with full Per, etc.
and let the leader change levels
so the spectacor would probably be an extra faction

save sreqPending, etc.?
not, but make sure

mark in UI that server ready and allude to that in the msg?

problem: when command dropped, some things in our ClientState and SessionUI
may already be set as if it was performed
(though not in State and ServerState)

-}

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
executorCli :: CCUI -> UIOptions -> ClientOptions
            -> COps
            -> Bool
            -> FactionId
            -> ChanServer
            -> IO ()
executorCli ccui sUIOptions clientOptions cops@COps{corule} isUI fid cliDict =
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
      exe = evalStateT (runCliImplementation m) . totalState
  in Save.wrapInSaves cops stateToFileName exe
