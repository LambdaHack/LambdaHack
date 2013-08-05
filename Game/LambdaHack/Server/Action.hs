-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServer( getServer, getsServer, putServer, modifyServer )
  , MonadServerConn
  , tryRestore, updateConn, waitForChildren, speedupCOps
    -- * Communication
  , sendUpdateUI, sendQueryUI, sendUpdateAI, sendQueryAI
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg
  , mkConfigRules, restoreScore, registerScore
  , rndToAction, fovMode, resetFidPerception, getPerFid
  ) where

import Data.Key (mapWithKeyM, mapWithKeyM_)
import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import qualified Data.Text.IO as T
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as R
import System.Directory
import System.Time

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Server.Action.ActionClass
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import qualified Game.LambdaHack.Common.HighScore as HighScore
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.File
import Game.LambdaHack.Frontend

fovMode :: MonadServer m => m FovMode
fovMode = do
  configFovMode <- getsServer (configFovMode . sconfig)
  sdebugSer <- getsServer sdebugSer
  return $ fromMaybe configFovMode $ stryFov sdebugSer

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m ()
resetFidPerception fid lid = do
  cops <- getsState scops
  lvl <- getsLevel lid id
  configFov <- fovMode
  s <- getState
  let per = levelPerception cops s configFov fid lid lvl
      upd = EM.adjust (EM.adjust (const per) lid) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}

getPerFid :: MonadServer m => FactionId -> LevelId -> m Perception
getPerFid fid lid = do
  pers <- getsServer sper
  let fper = fromMaybe (assert `failure` (lid, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` (lid, fid)) $ EM.lookup lid fper
  return $! per

saveGameSer :: MonadServer m => m ()
saveGameSer = do
  s <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameSer config s ser

-- | Save a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadServer m => m ()
saveGameBkp = do
  s <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameBkpSer config s ser

-- | Dumps the current game rules configuration to a file.
dumpCfg :: MonadServer m => FilePath -> m ()
dumpCfg fn = do
  config <- getsServer sconfig
  liftIO $ ConfigIO.dump config fn

writeTQueueAI :: MonadServerConn m => CmdClientAI -> TQueue CmdClientAI -> m ()
writeTQueueAI cmd toClient = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugCmdClientAI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue toClient cmd

writeTQueueUI :: MonadServerConn m => CmdClientUI -> TQueue CmdClientUI -> m ()
writeTQueueUI cmd toClient = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugCmdClientUI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue toClient cmd

readTQueue :: MonadServerConn m => TQueue CmdSer -> m CmdSer
readTQueue toServer = do
  cmd <- liftIO $ atomically $ STM.readTQueue toServer
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ do
    let aid = aidCmdSer cmd
    d <- debugAid aid (showT ("CmdSer", cmd))
    liftIO $ T.hPutStrLn stderr d
  return cmd

sendUpdateAI :: MonadServerConn m => FactionId -> CmdClientAI -> m ()
sendUpdateAI fid cmd = do
  conn <- getsDict $ snd . (EM.! fid)
  maybe skip (writeTQueueAI cmd . toClient) conn

sendQueryAI :: MonadServerConn m => FactionId -> ActorId -> m CmdSer
sendQueryAI fid aid = do
  mconn <- getsDict (snd . (EM.! fid))
  let connSend conn = do
        writeTQueueAI (CmdQueryAI aid) $ toClient conn
        readTQueue $ toServer conn
  maybe (assert `failure` (fid, aid)) connSend mconn

sendUpdateUI :: MonadServerConn m => FactionId -> CmdClientUI -> m ()
sendUpdateUI fid cmd = do
  mconn <- getsDict (fst . (EM.! fid))
  maybe skip (writeTQueueUI cmd . toClient . snd) mconn

sendQueryUI :: MonadServerConn m => FactionId -> ActorId -> m CmdSer
sendQueryUI fid aid = do
  mconn <- getsDict (fst . (EM.! fid))
  let connSend conn = do
        writeTQueueUI (CmdQueryUI aid) $ toClient conn
        readTQueue $ toServer conn
  maybe (assert `failure` (fid, aid)) (connSend . snd) mconn

-- | Create a server config file. Warning: when it's used, the game state
-- may still be undefined, hence the content ops are given as an argument.
mkConfigRules :: MonadServer m
              => Kind.Ops RuleKind -> m (Config, R.StdGen, R.StdGen)
mkConfigRules = liftIO . ConfigIO.mkConfigRules

-- | Read the high scores table. Return the empty table if no file.
-- Warning: when it's used, the game state
-- may still be undefined, hence the config is given as an argument.
restoreScore :: MonadServer m => Config -> m HighScore.ScoreTable
restoreScore Config{configScoresFile} = do
  b <- liftIO $ doesFileExist configScoresFile
  if not b
    then return HighScore.empty
    else liftIO $ strictDecodeEOF configScoresFile

-- | Generate a new score, register it and save.
registerScore :: MonadServer m => Status -> Int -> m ()
registerScore status total = do
  when (total /= 0) $ do
    config <- getsServer sconfig
    -- Re-read the table in case it's changed by a concurrent game.
    table <- restoreScore config
    time <- getsState stime
    date <- liftIO $ getClockTime
    let (ntable, _) = HighScore.register table total time status date
    liftIO $
      encodeEOF (configScoresFile config) (ntable :: HighScore.ScoreTable)

tryRestore :: MonadServer m
           => Kind.COps -> m (Maybe (State, StateServer))
tryRestore Kind.COps{corule} = do
  let pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  -- A throw-away copy of rules config, to be used until the old
  -- version of the config can be read from the savefile.
  (sconfig, _, _) <- mkConfigRules corule
  liftIO $ Save.restoreGameSer sconfig pathsDataFile

-- | Update connections to the new definition of factions.
-- Connect to clients in old or newly spawned threads
-- that read and write directly to the channels.
updateConn :: (MonadAtomic m, MonadServerConn m)
           => (FactionId -> FrontendConn -> Conn CmdClientUI -> IO ())
           -> (FactionId -> Conn CmdClientAI -> IO ())
           -> m ()
updateConn executorUI executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let mkConn :: IO (Conn c)
      mkConn = do
        toClient <- newTQueueIO
        toServer <- newTQueueIO
        return Conn{..}
      mkfConn :: IO FrontendConn
      mkfConn = do
        ftoClient <- newTQueueIO
        ftoFrontend <- newTQueueIO
        return FrontendConn{..}
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing -> do
          connUI <- if isHumanFact fact
                    then do
                      cUI <- mkConn
                      cf <- mkfConn
                      return $ Just (cf, cUI)
                    else return Nothing
          connAI <- fmap Just mkConn
          -- TODO, when net clients, etc., are included:
          -- connAI <- if usesAIFact fact
          --           then mkConn
          --           else return Nothing
          return (connUI, connAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  putDict d
  -- Spawn or kill client threads.
  let toSpawn = d EM.\\ oldD
      toKill  = oldD EM.\\ d  -- don't kill all, share old threads
  mapWithKeyM_ (\fid _ -> execCmdAtomic $ KillExitA fid) toKill
  -- TODO: kill multiplex threads for toKill, perhaps the client should
  -- send its multiplex thread a message inviting it to exit
  let forkUI fid (fconn, conn) = do
        void $ forkIO $ multiplex (ftoFrontend fconn) fid multiFrontendTQueue
        void $ forkChild $ executorUI fid fconn conn
      forkClient fid (connUI, connAI) = do
        maybe skip (forkUI fid) connUI
        maybe skip (void . forkChild . executorAI fid) connAI
  liftIO $ mapWithKeyM_ forkClient toSpawn

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.6.0.0/Control-Concurrent.html
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkIO (io `finally` putMVar mvar ())
-- 7.6  forkFinally io (\_ -> putMVar mvar ())

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code.
speedupCOps :: Bool -> Kind.COps -> Kind.COps
speedupCOps allClear copsSlow@Kind.COps{cotile=tile} =
  let ospeedup = Tile.speedup allClear tile
      cotile = tile {Kind.ospeedup}
  in copsSlow {Kind.cotile}

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer $ \ser -> ser {srandom = ng}
  return a
