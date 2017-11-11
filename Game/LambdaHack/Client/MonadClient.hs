-- | Basic client monads and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monads
    MonadClient( getsClient
               , modifyClient
               , liftIO  -- exposed only to be implemented, not used
               )
  , MonadClientSetup(..)
  , MonadClientAtomic(..)
    -- * Assorted primitives
  , getClient, putClient
  , debugPossiblyPrint, rndToAction, rndToActionForget
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Text.IO as T
import           System.IO (hFlush, stdout)
import qualified System.Random as R

import Game.LambdaHack.Atomic (UpdAtomic)
import Game.LambdaHack.Client.ClientOptions
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State

-- | Monad for writing to client state.
class MonadStateRead m => MonadClient m where
  getsClient    :: (StateClient -> a) -> m a
  modifyClient  :: (StateClient -> StateClient) -> m ()
  -- We do not provide a MonadIO instance, so that outside
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO        :: IO a -> m a

-- | Client monad for saving and restarting games.
class MonadClient m => MonadClientSetup m where
  saveClient    :: m ()
  restartClient :: m ()

-- | Monad for executing atomic game state transformations on a client.
class MonadClient m => MonadClientAtomic m where
  -- | Execute an atomic update that changes the client's 'State'.
  execUpdAtomic :: UpdAtomic -> m ()
  -- | Put state that is intended to be the result of performing
  -- an atomic update by the server on its copy of the client's 'State'.
  execPutState :: State -> m ()

getClient :: MonadClient m => m StateClient
getClient = getsClient id

putClient :: MonadClient m => StateClient -> m ()
putClient s = modifyClient (const s)

debugPossiblyPrint :: MonadClient m => Text -> m ()
debugPossiblyPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . soptions
  when sdbgMsgCli $ liftIO $  do
    T.hPutStrLn stdout t
    hFlush stdout

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  gen <- getsClient srandom
  let (gen1, gen2) = R.split gen
  modifyClient $ \ser -> ser {srandom = gen1}
  return $! St.evalState r gen2

-- | Invoke pseudo-random computation, don't change generator kept in state.
rndToActionForget :: MonadClient m => Rnd a -> m a
rndToActionForget r = do
  gen <- getsClient srandom
  return $! St.evalState r gen
