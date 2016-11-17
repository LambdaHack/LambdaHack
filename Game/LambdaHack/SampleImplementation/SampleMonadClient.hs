{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleMonadClient
  ( runCli, CliState(..), initialCliState
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , CliImplementation
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Binary
import GHC.Generics (Generic)

import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

data CliState = CliState
  { cliState   :: !State              -- ^ current global state
  , cliClient  :: !StateClient        -- ^ current client state
  , cliSession :: !(Maybe SessionUI)  -- ^ UI state, empty for AI clients
  }
  deriving Generic

instance Binary CliState

-- | Client state transformation monad.
newtype CliImplementation a = CliImplementation
  { runCliImplementation :: StateT CliState IO a }
  deriving (Monad, Functor, Applicative)

instance MonadStateRead CliImplementation where
  {-# INLINABLE getState #-}
  getState    = CliImplementation $ gets cliState
  {-# INLINE getsState #-}
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite CliImplementation where
  {-# INLINE modifyState #-}
  modifyState f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliState cliS
    in ((), cliS {cliState = newCliState})
  {-# INLINABLE putState #-}
  putState s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliState = s})

instance MonadClient CliImplementation where
  {-# INLINABLE getClient #-}
  getClient      = CliImplementation $ gets cliClient
  {-# INLINE getsClient #-}
  getsClient   f = CliImplementation $ gets $ f . cliClient
  {-# INLINE modifyClient #-}
  modifyClient f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliClient cliS
    in ((), cliS {cliClient = newCliState})
  {-# INLINABLE putClient #-}
  putClient s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliClient = s})
  {-# INLINABLE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

instance MonadClientSetup CliImplementation where
  saveClient = return ()
  restartClient  = CliImplementation $ state $ \cliS ->
    case cliSession cliS of
      Just sess ->
        let !newSess = (emptySessionUI (sconfig sess))
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
        in ((), cliS {cliSession = Just newSess})
      Nothing -> ((), cliS)

instance MonadClientUI CliImplementation where
  {-# INLINABLE getSession #-}
  getSession      = CliImplementation $ gets $ fromJust . cliSession
  {-# INLINE getsSession #-}
  getsSession   f = CliImplementation $ gets $ f . fromJust . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliImplementation $ state $ \cliS ->
    let !newCliSession = f $ fromJust $ cliSession cliS
    in ((), cliS {cliSession = Just newCliSession})
  {-# INLINABLE putSession #-}
  putSession s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliSession = Just s})
  {-# INLINABLE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

-- | The game-state semantics of atomic commands
-- as computed on the client.
instance MonadAtomic CliImplementation where
  execUpdAtomic cmd = handleUpdAtomic cmd
  execSfxAtomic _sfx = return ()

initialCliState :: Kind.COps
                -> Maybe SessionUI
                -> FactionId
                -> CliState
initialCliState cops cliSession fid =
  CliState
    { cliState = emptyState cops
    , cliClient = emptyStateClient fid
    , cliSession
    }

runCli :: CliImplementation a -> CliState -> IO (a, CliState)
{-# INLINE runCli #-}
runCli m = runStateT (runCliImplementation m)
