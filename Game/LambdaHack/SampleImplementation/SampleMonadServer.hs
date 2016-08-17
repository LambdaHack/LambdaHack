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
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.FileM
import Game.LambdaHack.Server.HandleAtomicM
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
    let !newSerState = f $ serState serS
    in ((), serS {serState = newSerState})
  putState s = SerImplementation $ state $ \serS ->
    s `seq` ((), serS {serState = s})

instance MonadServer SerImplementation where
  getServer      = SerImplementation $ gets serServer
  getsServer   f = SerImplementation $ gets $ f . serServer
  modifyServer f = SerImplementation $ state $ \serS ->
    let !newSerServer = f $ serServer serS
    in ((), serS {serServer = newSerServer})
  putServer    s = SerImplementation $ state $ \serS ->
    s `seq` ((), serS {serServer = s})
  liftIO         = SerImplementation . IO.liftIO
  saveChanServer = SerImplementation $ gets serToSave

instance MonadServerReadRequest SerImplementation where
  getDict      = SerImplementation $ gets serDict
  getsDict   f = SerImplementation $ gets $ f . serDict
  modifyDict f = SerImplementation $ state $ \serS ->
    let !newSerDict = f $ serDict serS
    in ((), serS {serDict = newSerDict})
  putDict s = SerImplementation $ state $ \serS ->
    s `seq` ((), serS {serDict = s})
  liftIO = SerImplementation . IO.liftIO

-- | The game-state semantics of atomic commands
-- as computed on the server.
instance MonadAtomic SerImplementation where
  execAtomic = handleAndBroadcastServer

-- | Send an atomic action to all clients that can see it.
handleAndBroadcastServer :: (MonadStateWrite m, MonadServerReadRequest m)
                         => CmdAtomic -> m ()
handleAndBroadcastServer atomic = do
  case atomic of
    UpdAtomic cmd -> cmdAtomicSemSer cmd
    SfxAtomic _sfx -> return ()
  sperFidOld <- getsServer sperFid
  sperCacheFidOld <- getsServer sperCacheFid
  discoAspect <- getsServer sdiscoAspect
  sactorAspect <- getsServer sactorAspect
  sfovClearLid <- getsServer sfovClearLid
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  let updatePerFid f = modifyServer $ \ser -> ser {sperFid = f $ sperFid ser}
      updatePerCacheFid f =
        modifyServer $ \ser -> ser {sperCacheFid = f $ sperCacheFid ser}
  handleAndBroadcast knowEvents sperFidOld sperCacheFidOld
                     discoAspect sactorAspect sfovClearLid
                     updatePerFid updatePerCacheFid getCacheLucid
                     sendUpdateAI sendUpdateUI atomic

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
