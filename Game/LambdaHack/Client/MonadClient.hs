-- | Basic client monad and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monads
    MonadClientRead ( getsClient
                    , liftIO  -- exposed only to be implemented, not used
                    )
  , MonadClient(modifyClient)
    -- * Assorted primitives
  , getClient, putClient
  , debugPossiblyPrint, rndToAction, rndToActionForget
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Text.IO as T
import           System.IO (hFlush, stdout)

import Game.LambdaHack.Client.ClientOptions
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random

-- | Monad for reading client state.
class MonadStateRead m => MonadClientRead m where
  getsClient :: (StateClient -> a) -> m a
  -- We do not provide a MonadIO instance, so that outside
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO :: IO a -> m a

-- | Monad for writing to client state.
class MonadClientRead m => MonadClient m where
  modifyClient :: (StateClient -> StateClient) -> m ()

getClient :: MonadClientRead m => m StateClient
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
  gen1 <- getsClient srandom
  let (a, gen2) = St.runState r gen1
  modifyClient $ \cli -> cli {srandom = gen2}
  return a

-- | Invoke pseudo-random computation, don't change generator kept in state.
rndToActionForget :: MonadClientRead m => Rnd a -> m a
rndToActionForget r = do
  gen <- getsClient srandom
  return $! St.evalState r gen
