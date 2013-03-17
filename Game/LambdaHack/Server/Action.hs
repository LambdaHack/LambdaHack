{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServer( getServer, getsServer, putServer, modifyServer )
  , MonadServerConn
  , executorSer, tryRestore, connServer, launchClients
  , waitForChildren, speedupCOps
    -- * Communication
  , sendUpdateUI, sendQueryUI, sendUpdateAI, sendQueryAI
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg
  , mkConfigRules, restoreScore, registerScore
  , rndToAction, resetFidPerception, getPerFid
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as R
import System.Directory
import System.Time

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.ServerCmd
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action.ActionClass
import Game.LambdaHack.Server.Action.ActionType (executorSer)
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import qualified Game.LambdaHack.HighScore as HighScore
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.File

default (Text)

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m ()
resetFidPerception fid arena = do
  cops <- getsState scops
  configFovMode <- getsServer (configFovMode . sconfig)
  sdebugSer <- getsServer sdebugSer
  lvl <- getsLevel arena id
  s <- getState
  let tryFov = stryFov sdebugSer
      fovMode = fromMaybe configFovMode tryFov
      per = levelPerception cops s fovMode fid arena lvl
      upd = EM.adjust (EM.adjust (const per) arena) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}

getPerFid :: MonadServer m => FactionId -> LevelId -> m Perception
getPerFid fid arena = do
  pers <- getsServer sper
  let fper = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup arena fper
  return $! per

saveGameSer :: MonadServer m => m ()
saveGameSer = do
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameSer config glo ser

-- | Save a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadServer m => m ()
saveGameBkp = do
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameBkpSer config glo ser

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
  conn <- getsDict (fst . (EM.! fid))
  maybe skip (writeTQueueUI cmd . toClient) conn

sendQueryUI :: MonadServerConn m => FactionId -> ActorId -> m CmdSer
sendQueryUI fid aid = do
  mconn <- getsDict (fst . (EM.! fid))
  let connSend conn = do
        writeTQueueUI (CmdQueryUI aid) $ toClient conn
        readTQueue $ toServer conn
  maybe (assert `failure` (fid, aid)) connSend mconn

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
    let ntable = HighScore.register table total time status date
    liftIO $ encodeEOF (configScoresFile config) ntable

tryRestore :: MonadServer m
           => Kind.COps -> m (Either (State, StateServer, Msg) Msg)
tryRestore Kind.COps{corule} = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  -- A throw-away copy of rules config, to be used until the old
  -- version of the config can be read from the savefile.
  (sconfig, _, _) <- mkConfigRules corule
  liftIO $ Save.restoreGameSer sconfig pathsDataFile title

-- | Prepare connections based on factions.
connServer :: MonadServerConn m => m ()
connServer = do
  faction <- getsState sfaction
  -- Prepare connections based on factions.
  let mkConn :: IO (Maybe (Conn c))
      mkConn = do
        toClient <- newTQueueIO
        toServer <- newTQueueIO
        return $ Just $ Conn{..}
      addConn (fid, fact) = do
        connUI <- if isHumanFact fact
                  then mkConn
                  else return Nothing
        connAI <- if usesAIFact fact
                  then mkConn
                  else return Nothing
        return (fid, (connUI, connAI))
  connAssocs <- liftIO $ mapM addConn $ EM.assocs faction
  putDict $ EM.fromDistinctAscList connAssocs

-- | Connect to clients by starting them in spawned threads that read
-- and write directly to the channels.
launchClients :: MonadServerConn m
              => (FactionId -> Conn CmdClientUI -> IO ())
              -> (FactionId -> Conn CmdClientAI -> IO ())
              -> m ()
launchClients executorUI executorAI = do
  let forkClient (fid, (connUI, connAI)) = do
        maybe skip (void . forkChild . executorUI fid) connUI
        maybe skip (void . forkChild . executorAI fid) connAI
  d <- getDict
  liftIO $ mapM_ forkClient $ EM.assocs d

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
