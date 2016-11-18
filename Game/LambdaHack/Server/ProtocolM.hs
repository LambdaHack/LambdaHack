-- | The server definitions for the server-client communication protocol.
module Game.LambdaHack.Server.ProtocolM
  ( -- * The communication channels
    CliSerQueue, ChanServer(..), updateCopsDict
  , ConnServerDict  -- exposed only to be implemented, not used
    -- * The server-client communication monad
  , MonadServerReadRequest
      ( getDict  -- exposed only to be implemented, not used
      , getsDict  -- exposed only to be implemented, not used
      , modifyDict  -- exposed only to be implemented, not used
      , putDict  -- exposed only to be implemented, not used
      , saveChanServer  -- exposed only to be implemented, not used
      , liftIO  -- exposed only to be implemented, not used
      )
    -- * Protocol
  , sendUpdate, sendSfx, sendQueryAI, sendQueryUI
    -- * Assorted
  , killAllClients, childrenServer, updateConn
  , saveServer, saveName, tryRestore
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM, mapWithKeyM_)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.DebugM
import Game.LambdaHack.Server.FileM
import Game.LambdaHack.Server.MonadServer hiding (liftIO)
import Game.LambdaHack.Server.State

#ifdef CLIENTS_AS_THREADS
import Game.LambdaHack.Common.Thread
#else
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.ProtocolM
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.SampleImplementation.SampleMonadClient
#endif

type CliSerQueue = MVar

#ifdef CLIENTS_AS_THREADS
writeQueueAI :: MonadServerReadRequest m
             => ResponseAI -> CliSerQueue ResponseAI -> m ()
{-# INLINABLE writeQueueAI #-}
writeQueueAI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseAI cmd
  liftIO $ putMVar responseS cmd

writeQueueUI :: MonadServerReadRequest m
             => ResponseUI -> CliSerQueue ResponseUI -> m ()
{-# INLINABLE writeQueueUI #-}
writeQueueUI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseUI cmd
  liftIO $ putMVar responseS cmd

readQueueAI :: MonadServerReadRequest m
            => CliSerQueue RequestAI -> m RequestAI
{-# INLINABLE readQueueAI #-}
readQueueAI requestS = liftIO $ takeMVar requestS

readQueueUI :: MonadServerReadRequest m
            => CliSerQueue RequestUI -> m RequestUI
{-# INLINABLE readQueueUI #-}
readQueueUI requestS = liftIO $ takeMVar requestS

newQueue :: IO (CliSerQueue a)
newQueue = newEmptyMVar
#endif

saveServer :: MonadServerReadRequest m => m ()
{-# INLINABLE saveServer #-}
saveServer = do
  s <- getState
  ser <- getServer
  dictAll <- getDict
  toSave <- saveChanServer
  liftIO $ Save.saveToChan toSave (s, ser, dictAll)

saveName :: String
saveName = serverSaveName

tryRestore :: MonadServerReadRequest m
           => Kind.COps -> DebugModeSer
           -> m (Maybe (State, StateServer, ConnServerDict))
{-# INLINABLE tryRestore #-}
tryRestore Kind.COps{corule} sdebugSer = do
  let bench = sbenchmark $ sdebugCli sdebugSer
  if bench then return Nothing
  else do
    let prefix = ssavePrefixSer sdebugSer
        name = prefix <.> saveName
    res <-
      liftIO $ Save.restoreGame tryCreateDir doesFileExist strictDecodeEOF name
    let stdRuleset = Kind.stdRuleset corule
        cfgUIName = rcfgUIName stdRuleset
        content = rcfgUIDefault stdRuleset
    dataDir <- liftIO $ appDataDir
    liftIO $ tryWriteFile (dataDir </> cfgUIName) content
#ifdef CLIENTS_AS_THREADS
    return $! case res of
                Just (s, ser) -> Just (s, ser, EM.empty)
                Nothing -> Nothing
#else
    return res
#endif

-- | Connection channel between the server and a single client.
data ChanServer resp req = ChanServer
  { responseS :: !(CliSerQueue resp)
  , requestS  :: !(CliSerQueue req)
  }

-- | Either states or connections to the human-controlled client
-- of a faction and to the AI client for the same faction.
#ifdef CLIENTS_AS_THREADS
data FrozenClient = FThread !(Maybe (ChanServer ResponseUI RequestUI))
                            !(ChanServer ResponseAI RequestAI)
#else
type FrozenClient = CliState
#endif

-- | Connection information for all factions, indexed by faction identifier.
type ConnServerDict = EM.EnumMap FactionId FrozenClient

-- TODO: refactor so that the monad is split in 2 and looks analogously
-- to the Client monads. Restrict the Dict to implementation modules.
-- Then on top of that implement sendQueryAI, etc.
-- For now we call it MonadServerReadRequest
-- though it also has the functionality of MonadServerWriteResponse.

-- | The server monad with the ability to communicate with clients.
class MonadServer m => MonadServerReadRequest m where
  getDict      :: m ConnServerDict
  getsDict     :: (ConnServerDict -> a) -> m a
  modifyDict   :: (ConnServerDict -> ConnServerDict) -> m ()
  putDict      :: ConnServerDict -> m ()
  saveChanServer :: m (Save.ChanSave (State, StateServer, ConnServerDict))
  liftIO       :: IO a -> m a

updateCopsDict :: MonadServerReadRequest m
               => KeyKind -> Config -> DebugModeCli -> m ()
{-# INLINABLE updateCopsDict #-}
updateCopsDict copsClient sconfig sdebugCli = do
#ifndef CLIENTS_AS_THREADS
  cops <- getsState scops
  schanF <- liftIO $ Frontend.chanFrontendIO sdebugCli
#endif
  let updFrozenClient :: FrozenClient -> FrozenClient
#ifdef CLIENTS_AS_THREADS
      updFrozenClient (FThread mconnUI connAI) = FThread mconnUI connAI
#else
      updFrozenClient cliS = cliS { cliState = updState (cliState cliS)
                                 , cliSession = updSession <$> cliSession cliS }
      sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      updState = updateCOps (const cops)
      updSession sess = sess {schanF, sbinding}
#endif
  modifyDict $ EM.map updFrozenClient

sendUpdate :: MonadServerReadRequest m => FactionId -> UpdAtomic -> m ()
{-# INLINABLE sendUpdate #-}
sendUpdate !fid !cmd = do
  frozenClient <- getsDict $ (EM.! fid)
  case frozenClient of
#ifdef CLIENTS_AS_THREADS
    FThread mconn conn -> do
      writeQueueAI (RespUpdAtomicAI cmd) $ responseS conn
      maybe (return ())
            (\c -> writeQueueUI (RespUpdAtomicUI cmd) $ responseS c) mconn
#else
    cliState@CliState{cliSession=Nothing} -> do
      let m = handleSelfAI cmd
      ((), cliStateNew) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid cliStateNew
    cliState -> do
      let m = handleSelfUI cmd
      ((), cliStateNew) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid cliStateNew
#endif

sendSfx :: MonadServerReadRequest m => FactionId -> SfxAtomic -> m ()
{-# INLINABLE sendSfx #-}
sendSfx !fid !sfx = do
  frozenClient <- getsDict $ (EM.! fid)
  case frozenClient of
#ifdef CLIENTS_AS_THREADS
    FThread (Just conn) _ ->
      writeQueueUI (RespSfxAtomicUI sfx) $ responseS conn
#else
    cliState@CliState{cliSession=Just{}} -> do
      let m = displayRespSfxAtomicUI False sfx
      ((), cliStateNew) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid cliStateNew
#endif
    _ -> return ()

sendQueryAI :: MonadServerReadRequest m => FactionId -> ActorId -> m RequestAI
{-# INLINABLE sendQueryAI #-}
sendQueryAI fid aid = do
  frozenClient <- getsDict $ (EM.! fid)
  req <- case frozenClient of
#ifdef CLIENTS_AS_THREADS
    FThread _ conn -> do
      writeQueueAI (RespQueryAI aid) $ responseS conn
      readQueueAI $ requestS conn
#else
    cliState -> do
      let m = queryAI aid
      (req, cliStateNew) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid cliStateNew
      return req
#endif
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestAI aid req
  return req

sendQueryUI :: (MonadAtomic m, MonadServerReadRequest m)
            => FactionId -> ActorId -> m RequestUI
{-# INLINABLE sendQueryUI #-}
sendQueryUI fid aid = do
  frozenClient <- getsDict $ (EM.! fid)
  req <- case frozenClient of
#ifdef CLIENTS_AS_THREADS
    FThread (Just conn) _ -> do
      writeQueueUI RespQueryUI $ responseS conn
      readQueueUI $ requestS conn
#else
    cliState@CliState{cliSession=Just{}} -> do
      let m = queryUI
      (req, cliStateNew) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid cliStateNew
      return req
#endif
    _ -> assert `failure` "no channel for faction" `twith` fid
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestUI aid req
  return req

killAllClients :: (MonadAtomic m, MonadServerReadRequest m) => m ()
{-# INLINABLE killAllClients #-}
killAllClients = do
  d <- getDict
  let sendKill fid _ =
        -- We can't check in sfactionD, because client can be from an old game.
        sendUpdate fid $ UpdKillExit fid
  mapWithKeyM_ sendKill d

-- Global variable for all children threads of the server.
childrenServer :: MVar [Async ()]
{-# NOINLINE childrenServer #-}
childrenServer = unsafePerformIO (newMVar [])

-- | Update connections to the new definition of factions.
-- Connect to clients in old or newly spawned threads
-- that read and write directly to the channels.
updateConn :: (MonadAtomic m, MonadServerReadRequest m)
           => Kind.COps
           -> KeyKind -> Config -> DebugModeCli
           -> (SessionUI -> Kind.COps -> FactionId
               -> ChanServer ResponseUI RequestUI
               -> IO ())
           -> (Kind.COps -> FactionId
               -> ChanServer ResponseAI RequestAI
               -> IO ())
           -> m ()
{-# INLINABLE updateConn #-}
updateConn cops copsClient sconfig sdebugCli
           _executorUI _executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let sess = emptySessionUI sconfig
#ifdef CLIENTS_AS_THREADS
      mkChanServer :: IO (ChanServer resp req)
      mkChanServer = do
        responseS <- newQueue
        requestS <- newQueue
        return $! ChanServer{..}
#else
      initStateUI fid = do
        let initCli = initialCliState cops (Just sess) fid
        snd <$> runCli (initUI copsClient sconfig sdebugCli) initCli
      initStateAI fid = do
        let initCli = initialCliState cops Nothing fid
        snd <$> runCli (initAI sdebugCli) initCli
#endif
      addConn :: FactionId -> Faction -> IO FrozenClient
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing | fhasUI $ gplayer fact -> do
#ifdef CLIENTS_AS_THREADS
          connS <- mkChanServer
          connAI <- mkChanServer
          return $! FThread (Just connS) connAI
#else
          initStateUI fid
#endif
        Nothing -> do
#ifdef CLIENTS_AS_THREADS
          connAI <- mkChanServer
          return $! FThread Nothing connAI
#else
          initStateAI fid
#endif
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  let newD = d `EM.union` oldD  -- never kill old clients
  putDict newD
#ifdef CLIENTS_AS_THREADS
  -- Spawn client threads.
  let toSpawn = newD EM.\\ oldD
      forkUI fid connS =
        forkChild childrenServer $ _executorUI sess cops fid connS
      forkAI fid connS =
        forkChild childrenServer $ _executorAI cops fid connS
      forkClient fid (FThread mconnUI connAI) = do
        -- When a connection is reused, clients are not respawned,
        -- even if UI usage changes, but it works OK thanks to UI faction
        -- clients distinguished by positive FactionId numbers.
        forkAI fid connAI  -- AI clients always needed, e.g., for auto-explore
        maybe (return ()) (forkUI fid) mconnUI
  liftIO $ mapWithKeyM_ forkClient toSpawn
#endif
