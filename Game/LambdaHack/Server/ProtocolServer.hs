-- | The server definitions for the server-client communication protocol.
module Game.LambdaHack.Server.ProtocolServer
  ( -- * The communication channels
    ChanServer(..)
  , ConnServerDict  -- exposed only to be implemented, not used
    -- * The server-client communication monad
  , MonadConnServer( getDict  -- exposed only to be implemented, not used
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
import Control.Concurrent.STM (TQueue, atomically)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM, mapWithKeyM_)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game.LambdaHack.Utils.Thread
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Frontend
import qualified Game.LambdaHack.Frontend as Frontend
import Game.LambdaHack.Server.MonadServer hiding (liftIO)
import Game.LambdaHack.Server.State

-- | Connection channels between the server and a single client.
data ChanServer c d = ChanServer
  { fromServer :: !(TQueue c)
  , toServer   :: !(TQueue d)
  }

-- | Connections to the human-controlled client of a faction and
-- to the AI client for the same faction.
type ConnServerFaction = ( Maybe (ChanFrontend, ChanServer ResponseUI Request)
                         , ChanServer ResponseAI RequestTimed )

-- | Connection information for all factions, indexed by faction identifier.
type ConnServerDict = EM.EnumMap FactionId ConnServerFaction

-- | The server monad with the ability to communicate with clients.
class MonadServer m => MonadConnServer m where
  getDict      :: m ConnServerDict
  getsDict     :: (ConnServerDict -> a) -> m a
  modifyDict   :: (ConnServerDict -> ConnServerDict) -> m ()
  putDict      :: ConnServerDict -> m ()
  liftIO       :: IO a -> m a

writeTQueueAI :: MonadConnServer m => ResponseAI -> TQueue ResponseAI -> m ()
writeTQueueAI cmd fromServer = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugResponseAI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue fromServer cmd

writeTQueueUI :: MonadConnServer m => ResponseUI -> TQueue ResponseUI -> m ()
writeTQueueUI cmd fromServer = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugResponseUI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue fromServer cmd

readTQueueAI :: MonadConnServer m => TQueue RequestTimed -> m RequestTimed
readTQueueAI toServer = do
  cmd <- liftIO $ atomically $ STM.readTQueue toServer
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ do
    let aid = aidOfRequestTimed cmd
    d <- debugAid aid "RequestTimed" cmd
    liftIO $ T.hPutStrLn stderr d
  return $! cmd

readTQueueUI :: MonadConnServer m => TQueue Request -> m Request
readTQueueUI toServer = do
  cmd <- liftIO $ atomically $ STM.readTQueue toServer
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ do
    let aid = aidOfRequest cmd
    d <- debugAid aid "Request" cmd
    liftIO $ T.hPutStrLn stderr d
  return $! cmd

sendUpdateAI :: MonadConnServer m => FactionId -> ResponseAI -> m ()
sendUpdateAI fid cmd = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI cmd $ fromServer conn

sendQueryAI :: MonadConnServer m => FactionId -> ActorId -> m RequestTimed
sendQueryAI fid aid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI (RespQueryAI aid) $ fromServer conn
  readTQueueAI $ toServer conn

sendPingAI :: (MonadAtomic m, MonadConnServer m)
           => FactionId -> m ()
sendPingAI fid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI RespPingAI $ fromServer conn
  -- debugPrint $ "AI client" <+> tshow fid <+> "pinged..."
  cmdHack <- readTQueueAI $ toServer conn
  -- debugPrint $ "AI client" <+> tshow fid <+> "responded."
  case cmdHack of
    ReqPongHack ats -> mapM_ execAtomic ats
    _ -> assert `failure` (fid, cmdHack)

sendUpdateUI :: MonadConnServer m => FactionId -> ResponseUI -> m ()
sendUpdateUI fid cmd = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just (_, conn) ->
      writeTQueueUI cmd $ fromServer conn

sendQueryUI :: (MonadAtomic m, MonadConnServer m)
            => FactionId -> ActorId -> m Request
sendQueryUI fid aid = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just (_, conn) -> do
      writeTQueueUI (RespQueryUI aid) $ fromServer conn
      readTQueueUI $ toServer conn

sendPingUI :: (MonadAtomic m, MonadConnServer m) => FactionId -> m ()
sendPingUI fid = do
  cs <- getsDict $ fst . (EM.! fid)
  case cs of
    Nothing -> assert `failure` "no channel for faction" `twith` fid
    Just (_, conn) -> do
      writeTQueueUI RespPingUI $ fromServer conn
      -- debugPrint $ "UI client" <+> tshow fid <+> "pinged..."
      cmdHack <- readTQueueUI $ toServer conn
      -- debugPrint $ "UI client" <+> tshow fid <+> "responded."
      case cmdHack of
        ReqTimed (ReqPongHack ats) -> mapM_ execAtomic ats
        _ -> assert `failure` (fid, cmdHack)

killAllClients :: (MonadAtomic m, MonadConnServer m) => m ()
killAllClients = do
  d <- getDict
  let sendKill fid _ = do
        -- We can't check in sfactionD, because client can be from an old game.
        when (fromEnum fid > 0) $
          sendUpdateUI fid $ RespUpdAtomicUI $ UpdKillExit fid
        sendUpdateAI fid $ RespUpdAtomicAI $ UpdKillExit fid
  mapWithKeyM_ sendKill d

-- Global variable for all children threads of the server.
childrenServer :: MVar [MVar ()]
{-# NOINLINE childrenServer #-}
childrenServer = unsafePerformIO (newMVar [])

-- | Update connections to the new definition of factions.
-- Connect to clients in old or newly spawned threads
-- that read and write directly to the channels.
updateConn :: (MonadAtomic m, MonadConnServer m)
           => (FactionId
               -> Frontend.ChanFrontend
               -> ChanServer ResponseUI Request
               -> IO ())
           -> (FactionId
               -> ChanServer ResponseAI RequestTimed
               -> IO ())
           -> m ()
updateConn executorUI executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let mkChanServer :: IO (ChanServer c d)
      mkChanServer = do
        fromServer <- STM.newTQueueIO
        toServer <- STM.newTQueueIO
        return $! ChanServer{..}
      mkChanFrontend :: IO Frontend.ChanFrontend
      mkChanFrontend = STM.newTQueueIO
      addConn :: FactionId -> Faction -> IO ConnServerFaction
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing | playerUI $ gplayer fact -> do
          connF <- mkChanFrontend
          connS <- mkChanServer
          connAI <- mkChanServer
          return (Just (connF, connS), connAI)
        Nothing -> do
          connAI <- mkChanServer
          return (Nothing, connAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  let newD = d `EM.union` oldD  -- never kill old clients
  putDict newD
  -- Spawn client threads.
  let toSpawn = newD EM.\\ oldD
      fdict fid = ( fst
                    $ fromMaybe (assert `failure` "no channel" `twith` fid)
                    $ fst
                    $ fromMaybe (assert `failure` "no faction" `twith` fid)
                    $ EM.lookup fid newD
                  , maybe T.empty gname  -- a faction can go inactive
                    $ EM.lookup fid factionD
                  )
      fromM = Frontend.fromMulti Frontend.connMulti
  liftIO $ void $ takeMVar fromM  -- stop Frontend
  let forkUI fid (connF, connS) =
        void $ forkChild childrenServer $ executorUI fid connF connS
      forkAI fid connS =
        void $ forkChild childrenServer $ executorAI fid connS
      forkClient fid (connUI, connAI) = do
        -- When a connection is reused, clients are not respawned,
        -- even if UI usage changes, but it works OK thanks to UI faction
        -- clients distinguished by positive FactionId numbers.
        forkAI fid connAI  -- AI clients always needed, e.g., for auto-explore
        maybe skip (forkUI fid) connUI
  liftIO $ mapWithKeyM_ forkClient toSpawn
  nU <- nUI
  liftIO $ putMVar fromM (nU, fdict)  -- restart Frontend
