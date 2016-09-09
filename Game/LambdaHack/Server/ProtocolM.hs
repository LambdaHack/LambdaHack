{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The server definitions for the server-client communication protocol.
module Game.LambdaHack.Server.ProtocolM
  ( -- * The communication channels
    CliSerQueue, ChanServer(..)
  , ConnServerDict  -- exposed only to be implemented, not used
    -- * The server-client communication monad
  , MonadServerReadRequest
      ( getDict  -- exposed only to be implemented, not used
      , getsDict  -- exposed only to be implemented, not used
      , modifyDict  -- exposed only to be implemented, not used
      , putDict  -- exposed only to be implemented, not used
      , liftIO  -- exposed only to be implemented, not used
      )
    -- * Protocol
  , sendUpdateAI, sendQueryAI, sendNonLeaderQueryAI
  , sendUpdateUI, sendQueryUI
    -- * Assorted
  , killAllClients, childrenServer, updateConn
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ConnServerFaction
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM, mapWithKeyM_)
import Game.LambdaHack.Common.Thread
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.LoopM
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.SampleImplementation.SampleMonadClient
import Game.LambdaHack.Server.DebugM
import Game.LambdaHack.Server.MonadServer hiding (liftIO)
import Game.LambdaHack.Server.State

type CliSerQueue = MVar

writeQueueAI :: MonadServerReadRequest m
              => ResponseAI -> CliSerQueue ResponseAI -> m ()
writeQueueAI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseAI cmd
  liftIO $ putMVar responseS cmd

writeQueueUI :: MonadServerReadRequest m
              => ResponseUI -> CliSerQueue ResponseUI -> m ()
writeQueueUI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseUI cmd
  liftIO $ putMVar responseS cmd

readQueueAI :: MonadServerReadRequest m
             => CliSerQueue RequestAI -> m RequestAI
readQueueAI requestS = liftIO $ takeMVar requestS

readQueueUI :: MonadServerReadRequest m
             => CliSerQueue RequestUI -> m RequestUI
readQueueUI requestS = liftIO $ takeMVar requestS

newQueue :: IO (CliSerQueue a)
newQueue = newEmptyMVar

-- | Connection channel between the server and a single client.
data ChanServer resp req = ChanServer
  { responseS :: !(CliSerQueue resp)
  , requestS  :: !(CliSerQueue req)
  }

data FrozenClient sess resp req =
    FState !(CliState sess)
  | FThread !(ChanServer resp req)

-- | Connections to the human-controlled client of a faction and
-- to the AI client for the same faction.
type ConnServerFaction = ( Maybe (FrozenClient SessionUI ResponseUI RequestUI)
                         , FrozenClient () ResponseAI RequestAI )

-- | Connection information for all factions, indexed by faction identifier.
type ConnServerDict = EM.EnumMap FactionId ConnServerFaction

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
  liftIO       :: IO a -> m a

sendUpdateAI :: MonadServerReadRequest m
             => FactionId -> ResponseAI -> m ()
sendUpdateAI fid cmd = do
  frozenClient <- getsDict $ (EM.! fid)
  case frozenClient of
    (fUI, FState cliState) -> do
      let m = case cmd of
            RespUpdAtomicAI c -> handleSelfAI c
            _ -> assert `failure` cmd
      ((), newCliState) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid (fUI, FState newCliState)
    (_, FThread conn) ->
      writeQueueAI cmd $ responseS conn

sendQueryAI :: MonadServerReadRequest m
            => FactionId -> ActorId -> m RequestAI
sendQueryAI fid aid = do
  frozenClient <- getsDict $ (EM.! fid)
  req <- case frozenClient of
    (fUI, FState cliState) -> do
      let m = queryAI
      (req, newCliState) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid (fUI, FState newCliState)
      return req
    (_, FThread conn) -> do
      writeQueueAI RespQueryAI $ responseS conn
      readQueueAI $ requestS conn
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestAI aid req
  return req

sendNonLeaderQueryAI :: MonadServerReadRequest m
                     => FactionId -> ActorId -> m ReqAI
sendNonLeaderQueryAI fid aid = do
  frozenClient <- getsDict $ (EM.! fid)
  req <- case frozenClient of
    (fUI, FState cliState) -> do
      let m = nonLeaderQueryAI aid
      (req, newCliState) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid (fUI, FState newCliState)
      return req
    (_, FThread conn) -> do
      writeQueueAI (RespNonLeaderQueryAI aid) $ responseS conn
      readQueueAI $ requestS conn
  case req of
    (_, Just{}) -> assert `failure` req
    (cmd, Nothing) -> do
      debug <- getsServer $ sniffIn . sdebugSer
      when debug $ debugRequestAI aid req
      return cmd

sendUpdateUI :: MonadServerReadRequest m
             => FactionId -> ResponseUI -> m ()
sendUpdateUI fid cmd = do
  frozenClient <- getsDict $ (EM.! fid)
  case frozenClient of
    (Just (FState cliState), fAI) -> do
      let m = case cmd of
            RespUpdAtomicUI c -> handleSelfUI c
            RespSfxAtomicUI sfx -> displayRespSfxAtomicUI False sfx
            _ -> assert `failure` cmd
      ((), newCliState) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid (Just (FState newCliState), fAI)
    (Just (FThread conn), _) ->
      writeQueueUI cmd $ responseS conn
    _ -> assert `failure` "no channel for faction" `twith` fid

sendQueryUI :: (MonadAtomic m, MonadServerReadRequest m)
            => FactionId -> ActorId -> m RequestUI
sendQueryUI fid aid = do
  frozenClient <- getsDict $ (EM.! fid)
  req <- case frozenClient of
    (Just (FState cliState), fAI) -> do
      let m = queryUI
      (req, newCliState) <- liftIO $ runCli m cliState
      modifyDict $ EM.insert fid (Just (FState newCliState), fAI)
      return req
    (Just (FThread conn), _) -> do
      writeQueueUI RespQueryUI $ responseS conn
      readQueueUI $ requestS conn
    _ -> assert `failure` "no channel for faction" `twith` fid
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestUI aid req
  return req

killAllClients :: (MonadAtomic m, MonadServerReadRequest m) => m ()
killAllClients = do
  d <- getDict
  let sendKill fid cs = do
        -- We can't check in sfactionD, because client can be from an old game.
        when (isJust $ fst cs) $
          sendUpdateUI fid $ RespUpdAtomicUI $ UpdKillExit fid
        sendUpdateAI fid $ RespUpdAtomicAI $ UpdKillExit fid
  mapWithKeyM_ sendKill d

-- Global variable for all children threads of the server.
childrenServer :: MVar [Async ()]
{-# NOINLINE childrenServer #-}
childrenServer = unsafePerformIO (newMVar [])

-- | Update connections to the new definition of factions.
-- Connect to clients in old or newly spawned threads
-- that read and write directly to the channels.
updateConn :: (MonadAtomic m, MonadServerReadRequest m)
           => Bool
           -> Kind.COps
           -> KeyKind -> Config -> DebugModeCli
           -> (SessionUI -> Kind.COps -> FactionId
               -> ChanServer ResponseUI RequestUI
               -> IO ())
           -> (Kind.COps -> FactionId
               -> ChanServer ResponseAI RequestAI
               -> IO ())
           -> m ()
updateConn useTreadsForNewClients cops copsClient sconfig sdebugCli
           executorUI executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let mkChanServer :: IO (ChanServer resp req)
      mkChanServer = do
        responseS <- newQueue
        requestS <- newQueue
        return $! ChanServer{..}
      cliSession = emptySessionUI sconfig
      initStateUI fid = do
        let initCli = initialCliState cops cliSession fid
        snd <$> runCli (initUI copsClient sconfig sdebugCli) initCli
      initStateAI fid = do
        let initCli = initialCliState cops () fid
        snd <$> runCli (initAI sdebugCli) initCli
      addConn :: FactionId -> Faction -> IO ConnServerFaction
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing | fhasUI $ gplayer fact ->
          if useTreadsForNewClients then do
            connS <- mkChanServer
            connAI <- mkChanServer
            return (Just $ FThread connS, FThread connAI)
          else do
            iUI <- initStateUI fid
            iAI <- initStateAI fid
            return (Just $ FState iUI, FState iAI)
        Nothing ->
          if useTreadsForNewClients then do
            connAI <- mkChanServer
            return (Nothing, FThread connAI)
          else do
            iAI <- initStateAI fid
            return (Nothing, FState iAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  let newD = d `EM.union` oldD  -- never kill old clients
  putDict newD
  -- Spawn client threads.
  let toSpawn = newD EM.\\ oldD
  let forkUI fid (FThread connS) =
        forkChild childrenServer $ executorUI cliSession cops fid connS
      forkUI _ FState{} = return ()
      forkAI fid (FThread connS) =
        forkChild childrenServer $ executorAI cops fid connS
      forkAI _ FState{} = return ()
      forkClient fid (connUI, connAI) = do
        -- When a connection is reused, clients are not respawned,
        -- even if UI usage changes, but it works OK thanks to UI faction
        -- clients distinguished by positive FactionId numbers.
        forkAI fid connAI  -- AI clients always needed, e.g., for auto-explore
        maybe (return ()) (forkUI fid) connUI
  liftIO $ mapWithKeyM_ forkClient toSpawn
