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
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
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

-- | Connections to the human-controlled client of a faction and
-- to the AI client for the same faction.
type ConnServerFaction = ( Maybe (ChanServer ResponseUI RequestUI)
                         , ChanServer ResponseAI RequestAI )

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
  conn <- getsDict $ snd . (EM.! fid)
  writeQueueAI cmd $ responseS conn
  cmdS <- readQueueAI $ requestS conn
  case fst cmdS of
    ReqAINop -> return ()
    _ -> assert `failure` cmdS

sendQueryAI :: MonadServerReadRequest m
            => FactionId -> ActorId -> m RequestAI
sendQueryAI fid aid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeQueueAI RespQueryAI $ responseS conn
  req <- readQueueAI $ requestS conn
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestAI aid req
  return $! req

sendNonLeaderQueryAI :: MonadServerReadRequest m
                     => FactionId -> ActorId -> m ReqAI
sendNonLeaderQueryAI fid aid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeQueueAI (RespNonLeaderQueryAI aid) $ responseS conn
  req <- readQueueAI $ requestS conn
  case req of
    (_, Just{}) -> assert `failure` req
    (cmd, Nothing) -> do
      debug <- getsServer $ sniffIn . sdebugSer
      when debug $ debugRequestAI aid req
      return cmd

sendUpdateUI :: MonadServerReadRequest m
             => FactionId -> ResponseUI -> m ()
sendUpdateUI fid cmd = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just conn -> do
      writeQueueUI cmd $ responseS conn
      cmdS <- readQueueUI $ requestS conn
      case fst cmdS of
        ReqUINop -> return ()
        _ -> assert `failure` cmdS

sendQueryUI :: (MonadAtomic m, MonadServerReadRequest m)
            => FactionId -> ActorId -> m RequestUI
sendQueryUI fid aid = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just conn -> do
      writeQueueUI RespQueryUI $ responseS conn
      req <- readQueueUI $ requestS conn
      debug <- getsServer $ sniffIn . sdebugSer
      when debug $ debugRequestUI aid req
      return $! req

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
           => (FactionId
               -> ChanServer ResponseUI RequestUI
               -> IO ())
           -> (FactionId
               -> ChanServer ResponseAI RequestAI
               -> IO ())
           -> m ()
updateConn executorUI executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let mkChanServer :: IO (ChanServer resp req)
      mkChanServer = do
        responseS <- newQueue
        requestS <- newQueue
        return $! ChanServer{..}
      addConn :: FactionId -> Faction -> IO ConnServerFaction
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing | fhasUI $ gplayer fact -> do
          connS <- mkChanServer
          connAI <- mkChanServer
          return (Just connS, connAI)
        Nothing -> do
          connAI <- mkChanServer
          return (Nothing, connAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  let newD = d `EM.union` oldD  -- never kill old clients
  putDict newD
  -- Spawn client threads.
  let toSpawn = newD EM.\\ oldD
  let forkUI fid connS = forkChild childrenServer $ executorUI fid connS
      forkAI fid connS = forkChild childrenServer $ executorAI fid connS
      forkClient fid (connUI, connAI) = do
        -- When a connection is reused, clients are not respawned,
        -- even if UI usage changes, but it works OK thanks to UI faction
        -- clients distinguished by positive FactionId numbers.
        forkAI fid connAI  -- AI clients always needed, e.g., for auto-explore
        maybe (return ()) (forkUI fid) connUI
  liftIO $ mapWithKeyM_ forkClient toSpawn
