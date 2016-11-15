{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
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

data CliState sess = CliState
  { cliState   :: !State        -- ^ current global state
  , cliClient  :: !StateClient  -- ^ current client state
  , cliSession :: !sess         -- ^ UI state, empty for AI clients
  }

-- | Client state transformation monad.
newtype CliImplementation sess a = CliImplementation
  { runCliImplementation :: StateT (CliState sess) IO a }
  deriving (Monad, Functor, Applicative)

instance MonadStateRead (CliImplementation sess) where
  {-# INLINE getState #-}
  getState    = CliImplementation $ gets cliState
  {-# INLINE getsState #-}
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadStateWrite (CliImplementation sess) where
  {-# INLINE modifyState #-}
  modifyState f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliState cliS
    in ((), cliS {cliState = newCliState})
  {-# INLINE putState #-}
  putState s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliState = s})

instance MonadClient (CliImplementation sess) where
  {-# INLINE getClient #-}
  getClient      = CliImplementation $ gets cliClient
  {-# INLINE getsClient #-}
  getsClient   f = CliImplementation $ gets $ f . cliClient
  {-# INLINE modifyClient #-}
  modifyClient f = CliImplementation $ state $ \cliS ->
    let !newCliState = f $ cliClient cliS
    in ((), cliS {cliClient = newCliState})
  {-# INLINE putClient #-}
  putClient s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliClient = s})
  {-# INLINE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

instance MonadClientSetup (CliImplementation ()) where
  saveClient = return ()
  restartClient = return ()

instance MonadClientSetup (CliImplementation SessionUI) where
  saveClient = return ()
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
  {-# INLINE getSession #-}
  getSession      = CliImplementation $ gets cliSession
  {-# INLINE getsSession #-}
  getsSession   f = CliImplementation $ gets $ f . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliImplementation $ state $ \cliS ->
    let !newCliSession = f $ cliSession cliS
    in ((), cliS {cliSession = newCliSession})
  {-# INLINE putSession #-}
  putSession s = CliImplementation $ state $ \cliS ->
    s `seq` ((), cliS {cliSession = s})
  {-# INLINE liftIO #-}
  liftIO = CliImplementation . IO.liftIO

-- | The game-state semantics of atomic commands
-- as computed on the client.
instance MonadAtomic (CliImplementation sess) where
  execUpdAtomic cmd = handleUpdAtomic cmd
  execSfxAtomic _sfx = return ()

initialCliState :: Kind.COps
                -> sess
                -> FactionId
                -> CliState sess
initialCliState cops cliSession fid =
  CliState
    { cliState = emptyState cops
    , cliClient = emptyStateClient fid
    , cliSession
    }

runCli :: CliImplementation sess a -> CliState sess -> IO (a, CliState sess)
{-# INLINE runCli #-}
runCli m = runStateT (runCliImplementation m)
