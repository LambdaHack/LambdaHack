-- | The server definitions for the server-client communication protocol.
module Game.LambdaHack.Server.ProtocolServer
  ( -- * The communication channels
    ChanServer(..)
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
  , sendUpdateAI, sendQueryAI, sendPingAI
  , sendUpdateUI, sendQueryUI, sendPingUI
    -- * Assorted
  , killAllClients, childrenServer, updateConn
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (TQueue, atomically)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar
import Control.Monad
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
import Game.LambdaHack.Server.DebugServer
import Game.LambdaHack.Server.MonadServer hiding (liftIO)
import Game.LambdaHack.Server.State

-- | Connection channel between the server and a single client.
data ChanServer resp req = ChanServer
  { responseS :: !(TQueue resp)
  , requestS  :: !(TQueue req)
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

writeTQueueAI :: MonadServerReadRequest m
              => ResponseAI -> TQueue ResponseAI -> m ()
writeTQueueAI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseAI cmd
  liftIO $ atomically $ STM.writeTQueue responseS cmd

writeTQueueUI :: MonadServerReadRequest m
              => ResponseUI -> TQueue ResponseUI -> m ()
writeTQueueUI cmd responseS = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ debugResponseUI cmd
  liftIO $ atomically $ STM.writeTQueue responseS cmd

readTQueueAI :: MonadServerReadRequest m
             => TQueue RequestAI -> m RequestAI
readTQueueAI requestS = liftIO $ atomically $ STM.readTQueue requestS

readTQueueUI :: MonadServerReadRequest m
             => TQueue RequestUI -> m RequestUI
readTQueueUI requestS = liftIO $ atomically $ STM.readTQueue requestS

sendUpdateAI :: MonadServerReadRequest m
             => FactionId -> ResponseAI -> m ()
sendUpdateAI fid cmd = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI cmd $ responseS conn

sendQueryAI :: MonadServerReadRequest m
            => FactionId -> ActorId -> m RequestAI
sendQueryAI fid aid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI (RespQueryAI aid) $ responseS conn
  req <- readTQueueAI $ requestS conn
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ debugRequestAI aid req
  return $! req

sendPingAI :: (MonadAtomic m, MonadServerReadRequest m)
           => FactionId -> m ()
sendPingAI fid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI RespPingAI $ responseS conn
  -- debugPrint $ "AI client" <+> tshow fid <+> "pinged..."
  cmdPong <- readTQueueAI $ requestS conn
  -- debugPrint $ "AI client" <+> tshow fid <+> "responded."
  case cmdPong of
    ReqAIPong -> return ()
    _ -> assert `failure` (fid, cmdPong)

sendUpdateUI :: MonadServerReadRequest m
             => FactionId -> ResponseUI -> m ()
sendUpdateUI fid cmd = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just conn ->
      writeTQueueUI cmd $ responseS conn

sendQueryUI :: (MonadAtomic m, MonadServerReadRequest m)
            => FactionId -> ActorId -> m RequestUI
sendQueryUI fid aid = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just conn -> do
      writeTQueueUI RespQueryUI $ responseS conn
      req <- readTQueueUI $ requestS conn
      debug <- getsServer $ sniffIn . sdebugSer
      when debug $ debugRequestUI aid req
      return $! req

sendPingUI :: (MonadAtomic m, MonadServerReadRequest m)
           => FactionId -> m ()
sendPingUI fid = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just conn -> do
      writeTQueueUI RespPingUI $ responseS conn
      -- debugPrint $ "UI client" <+> tshow fid <+> "pinged..."
      cmdPong <- readTQueueUI $ requestS conn
      -- debugPrint $ "UI client" <+> tshow fid <+> "responded."
      case cmdPong of
        ReqUIPong ats -> mapM_ execAtomic ats
        _ -> assert `failure` (fid, cmdPong)

killAllClients :: (MonadAtomic m, MonadServerReadRequest m) => m ()
killAllClients = do
  d <- getDict
  let sendKill fid _ = do
        -- We can't check in sfactionD, because client can be from an old game.
        when (fromEnum fid > 0) $
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
        responseS <- STM.newTQueueIO
        requestS <- STM.newTQueueIO
        return $! ChanServer{..}
      addConn :: FactionId -> Faction -> IO ConnServerFaction
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing | playerUI $ gplayer fact -> do
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
  let forkUI fid connS =
        forkChild childrenServer $ executorUI fid connS
      forkAI fid connS =
        forkChild childrenServer $ executorAI fid connS
      forkClient fid (connUI, connAI) = do
        -- When a connection is reused, clients are not respawned,
        -- even if UI usage changes, but it works OK thanks to UI faction
        -- clients distinguished by positive FactionId numbers.
        forkAI fid connAI  -- AI clients always needed, e.g., for auto-explore
        maybe skip (forkUI fid) connUI
  liftIO $ mapWithKeyM_ forkClient toSpawn
