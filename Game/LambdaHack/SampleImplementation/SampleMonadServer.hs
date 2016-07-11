{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleMonadServer
  ( executorSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , SerImplementation
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Exception as Ex
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import qualified Data.EnumMap.Strict as EM
import System.FilePath

import Game.LambdaHack.Atomic.BroadcastAtomicWrite
import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Thread
import Game.LambdaHack.Server.FileM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.State

data SerState = SerState
  { serState  :: !State           -- ^ current global state
  , serServer :: !StateServer     -- ^ current server state
  , serDict   :: !ConnServerDict  -- ^ client-server connection information
  , serToSave :: !(Save.ChanSave (State, StateServer))
                                  -- ^ connection to the save thread
  }

-- | Server state transformation monad.
newtype SerImplementation a =
    SerImplementation {runSerImplementation :: StateT SerState IO a}
  deriving (Monad, Functor, Applicative)

instance MonadStateRead SerImplementation where
  getState    = SerImplementation $ gets serState
  getsState f = SerImplementation $ gets $ f . serState

instance MonadStateWrite SerImplementation where
  modifyState f = SerImplementation $ state $ \serS ->
    let newSerS = serS {serState = f $ serState serS}
    in newSerS `seq` ((), newSerS)
  putState    s = SerImplementation $ state $ \serS ->
    let newSerS = serS {serState = s}
    in newSerS `seq` ((), newSerS)

instance MonadServer SerImplementation where
  getServer      = SerImplementation $ gets serServer
  getsServer   f = SerImplementation $ gets $ f . serServer
  modifyServer f = SerImplementation $ state $ \serS ->
    let newSerS = serS {serServer = f $ serServer serS}
    in newSerS `seq` ((), newSerS)
  putServer    s = SerImplementation $ state $ \serS ->
    let newSerS = serS {serServer = s}
    in newSerS `seq` ((), newSerS)
  liftIO         = SerImplementation . IO.liftIO
  saveChanServer = SerImplementation $ gets serToSave

instance MonadServerReadRequest SerImplementation where
  getDict      = SerImplementation $ gets serDict
  getsDict   f = SerImplementation $ gets $ f . serDict
  modifyDict f =
    SerImplementation $ modify $ \serS -> serS {serDict = f $ serDict serS}
  putDict    s =
    SerImplementation $ modify $ \serS -> serS {serDict = s}
  liftIO       = SerImplementation . IO.liftIO

-- | The game-state semantics of atomic commands
-- as computed on the server.
instance MonadAtomic SerImplementation where
  execAtomic = handleAndBroadcastServer

-- | Send an atomic action to all clients that can see it.
handleAndBroadcastServer :: (MonadStateWrite m, MonadServerReadRequest m)
                         => CmdAtomic -> m ()
handleAndBroadcastServer atomic = do
  sperFidOld <- getsServer sperFid
  sperCacheFidOld <- getsServer sperCacheFid
  persLitOld <- getsServer slit
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  let updatePer fid lid per msrvPer =
        let upd = EM.adjust (EM.adjust (const per) lid) fid
            srvUpd = case msrvPer of
              Nothing -> id
              Just srvPer -> EM.adjust (EM.adjust (const srvPer) lid) fid
        in modifyServer $ \ser2 ->
             ser2 { sperFid = upd (sperFid ser2)
                  , sperCacheFid = srvUpd (sperCacheFid ser2) }
      updateLit slit = modifyServer $ \ser -> ser {slit}
      getItemFovCache = getsServer sItemFovCache
  handleAndBroadcast knowEvents sperFidOld sperCacheFidOld
                     getItemFovCache persLitOld
                     updatePer updateLit sendUpdateAI sendUpdateUI atomic

-- | Run an action in the @IO@ monad, with undefined state.
executorSer :: Kind.COps -> SerImplementation () -> IO ()
executorSer cops m = do
  let saveFile (_, ser) = ssavePrefixSer (sdebugSer ser) <.> saveName
      totalState serToSave = SerState
        { serState = emptyState cops
        , serServer = emptyStateServer
        , serDict = EM.empty
        , serToSave
        }
      exe = evalStateT (runSerImplementation m) . totalState
      exeWithSaves = Save.wrapInSaves tryCreateDir encodeEOF saveFile exe
  -- Wait for clients to exit even in case of server crash
  -- (or server and client crash), which gives them time to save
  -- and report their own inconsistencies, if any.
  -- TODO: send them a message to tell users "server crashed"
  -- and then wait for them to exit normally.
  Ex.handle (\(ex :: Ex.SomeException) -> do
               threadDelay 1000000  -- let clients report their errors
               Ex.throw ex)  -- crash eventually, which kills clients
            exeWithSaves
  waitForChildren childrenServer  -- no crash, wait for clients indefinitely
