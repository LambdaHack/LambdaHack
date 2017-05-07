-- | Basic client monad and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monad
    MonadClient( getsClient, modifyClient
               , liftIO  -- exposed only to be implemented, not used
               )
  , MonadClientSetup( saveClient
                    , restartClient
                    )
    -- * Assorted primitives
  , getClient, putClient
  , debugPossiblyPrint, removeServerSave
  , rndToAction, rndToActionForget
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Text.IO as T
import System.FilePath
import System.IO (hFlush, stdout)
import qualified System.Random as R

import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.File
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save

class MonadStateRead m => MonadClient m where
  getsClient    :: (StateClient -> a) -> m a
  modifyClient  :: (StateClient -> StateClient) -> m ()
  -- We do not provide a MonadIO instance, so that outside
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO        :: IO a -> m a

class MonadClient m => MonadClientSetup m where
  saveClient    :: m ()
  restartClient :: m ()

getClient :: MonadClient m => m StateClient
getClient = getsClient id

putClient :: MonadClient m => StateClient -> m ()
putClient s = modifyClient (const s)

debugPossiblyPrint :: MonadClient m => Text -> m ()
debugPossiblyPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . sdebugCli
  when sdbgMsgCli $ liftIO $  do
    T.hPutStrLn stdout t
    hFlush stdout

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  -- Hack: assume the same prefix for client as for the server.
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  dataDir <- liftIO appDataDir
  let serverSaveFile bkp = dataDir
                           </> "saves"
                           </> bkp
                           <> prefix
                           <.> Save.saveNameSer
  bSer <- liftIO $ doesFileExist (serverSaveFile "")
  when bSer $ liftIO $ renameFile (serverSaveFile "") (serverSaveFile "bkp.")

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
