-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServer( getServer, getsServer, putServer, modifyServer )
  , MonadConnServer
  , tryRestore, updateConn, waitForChildren, speedupCOps
    -- * Communication
  , sendUpdateUI, sendQueryUI, sendUpdateAI, sendQueryAI
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg
  , mkConfigRules, restoreScore, registerScore, deduceQuits
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
import Data.List
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
import qualified Game.LambdaHack.Frontend as Frontend
import Game.LambdaHack.Common.ActorState

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

writeTQueueAI :: MonadConnServer m => CmdClientAI -> TQueue CmdClientAI -> m ()
writeTQueueAI cmd fromServer = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugCmdClientAI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue fromServer cmd

writeTQueueUI :: MonadConnServer m => CmdClientUI -> TQueue CmdClientUI -> m ()
writeTQueueUI cmd fromServer = do
  debug <- getsServer $ sniffOut . sdebugSer
  when debug $ do
    d <- debugCmdClientUI cmd
    liftIO $ T.hPutStrLn stderr d
  liftIO $ atomically $ STM.writeTQueue fromServer cmd

readTQueue :: MonadConnServer m => TQueue CmdSer -> m CmdSer
readTQueue toServer = do
  cmd <- liftIO $ atomically $ STM.readTQueue toServer
  debug <- getsServer $ sniffIn . sdebugSer
  when debug $ do
    let aid = aidCmdSer cmd
    d <- debugAid aid (showT ("CmdSer", cmd))
    liftIO $ T.hPutStrLn stderr d
  return cmd

sendUpdateAI :: MonadConnServer m => FactionId -> CmdClientAI -> m ()
sendUpdateAI fid cmd = do
  conn <- getsDict $ snd . (EM.! fid)
  maybe skip (writeTQueueAI cmd . fromServer) conn

sendQueryAI :: MonadConnServer m => FactionId -> ActorId -> m CmdSer
sendQueryAI fid aid = do
  mconn <- getsDict (snd . (EM.! fid))
  let connSend conn = do
        writeTQueueAI (CmdQueryAI aid) $ fromServer conn
        readTQueue $ toServer conn
  maybe (assert `failure` (fid, aid)) connSend mconn

sendUpdateUI :: MonadConnServer m => FactionId -> CmdClientUI -> m ()
sendUpdateUI fid cmd = do
  mconn <- getsDict (fst . (EM.! fid))
  maybe skip (writeTQueueUI cmd . fromServer . snd) mconn

sendQueryUI :: MonadConnServer m => FactionId -> ActorId -> m CmdSer
sendQueryUI fid aid = do
  mconn <- getsDict (fst . (EM.! fid))
  let connSend conn = do
        writeTQueueUI (CmdQueryUI aid) $ fromServer conn
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
registerScore :: MonadServer m => FactionId -> m ()
registerScore fid = do
  factionD <- getsState sfactionD
  let faction = factionD EM.! fid
      mstatus = fmap snd $ gquit faction
  when (isHumanFact faction) $ do
    total <- case gleader faction of
      Nothing -> return 0
      Just leader -> do
        b <- getsState $ getActorBody leader
        getsState $ snd . calculateTotal fid (blid b)
    case mstatus of
      Just status | total /= 0 -> do
        config <- getsServer sconfig
        -- Re-read the table in case it's changed by a concurrent game.
        table <- restoreScore config
        time <- getsState stime
        date <- liftIO $ getClockTime
        let (ntable, _) = HighScore.register table total time status date
        liftIO $
          encodeEOF (configScoresFile config) (ntable :: HighScore.ScoreTable)
      _ -> return ()

-- Send any QuitFactionA actions that can be deduced from their current state.
deduceQuits :: (MonadAtomic m, MonadServer m) => FactionId -> Status -> m ()
deduceQuits fid Defeated = assert `failure` fid
deduceQuits fid Camping = assert `failure` fid
deduceQuits fid Restart{} = assert `failure` fid
deduceQuits fid Conquer = assert `failure` fid
deduceQuits fid status = do
  cops <- getsState scops
  let quitF statusQ fidQ = do
        oldSt <- getsState $ gquit . (EM.! fidQ) . sfactionD
        case oldSt of
          Just (_, Killed{}) -> return ()  -- Do not overwrite in case
          Just (_, Defeated) -> return ()  -- many things happen in 1 turn.
          Just (_, Conquer) -> return ()
          Just (_, Escape) -> return ()
          _ -> do
            -- TODO: only send to fidQ: revealItems
            execCmdAtomic $ QuitFactionA fidQ oldSt $ Just (True, statusQ)
            registerScore fidQ
            modifyServer $ \ser -> ser {squit = True}  -- end turn ASAP
      mapQuitF statusQ fids = mapM_ (quitF statusQ) $ delete fid fids
  quitF status fid
  factionD <- getsState sfactionD
  let inGame fact = case gquit fact of
        Just (_, Killed{}) -> False
        Just (_, Defeated{}) -> False
        Just (_, Restart{}) -> False  -- effectively, commits suicide
        _ -> True
  let assocsInGame = filter (inGame . snd) $ EM.assocs factionD
      keysInGame = map fst assocsInGame
      assocsSpawn = filter (isSpawningFact cops . snd) assocsInGame
      assocsNotSpawn = filter (not . isSpawningFact cops . snd) assocsInGame
      assocsHuman = filter (isHumanFact . snd) assocsInGame
  case assocsNotSpawn of
    _ | null assocsHuman ->
      -- No screensaver mode for now --- all non-human players win.
      mapQuitF Conquer keysInGame
    [] ->
      -- Only spawners remain so all win, human or not, allied or not.
      mapQuitF Conquer keysInGame
    (_, fact1) : rest | null assocsSpawn && all (isAllied fact1 . fst) rest ->
      -- Only one allied team remains in a no-spawners game.
      mapQuitF Conquer keysInGame
    _ | status == Escape -> do
      -- Otherwise, in a spawners game or a game with many teams alive,
      -- only complete Victory matters.
      let (victors, losers) = partition (flip isAllied fid . snd) assocsInGame
      mapQuitF Escape $ map fst victors
      mapQuitF Defeated $ map fst losers
    _ -> return ()

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
updateConn :: (MonadAtomic m, MonadConnServer m)
           => (FactionId -> Frontend.ChanFrontend -> ChanServer CmdClientUI
               -> IO ())
           -> (FactionId -> ChanServer CmdClientAI -> IO ())
           -> m ()
updateConn executorUI executorAI = do
  -- Prepare connections based on factions.
  oldD <- getDict
  let mkChanServer :: IO (ChanServer c)
      mkChanServer = do
        fromServer <- newTQueueIO
        toServer <- newTQueueIO
        return ChanServer{..}
      mkChanFrontend :: IO Frontend.ChanFrontend
      mkChanFrontend = newTQueueIO
      addConn fid fact = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing -> do
          connUI <- if isHumanFact fact
                    then do
                      connF <- mkChanFrontend
                      connS <- mkChanServer
                      return $ Just (connF, connS)
                    else return Nothing
          connAI <- fmap Just mkChanServer
          -- TODO, when net clients, etc., are included:
          -- connAI <- if usesAIFact fact
          --           then mkChanServer
          --           else return Nothing
          return (connUI, connAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  putDict d
  -- Spawn and kill client threads.
  let toSpawn = d EM.\\ oldD
      toKill  = oldD EM.\\ d  -- don't kill all, share old threads
      fdict fid = fst $ (fromMaybe (assert `failure` fid))
                $ fst $ (fromMaybe (assert `failure` fid))
                $ EM.lookup fid d
      fromM = Frontend.fromMulti Frontend.connMulti
  nH <- nHumans
  liftIO $ void $ takeMVar fromM  -- stop Frontend
  mapWithKeyM_ (\fid _ -> execCmdAtomic $ KillExitA fid) toKill
  let forkUI fid (connF, connS) = void $ forkChild $ executorUI fid connF connS
      forkAI fid connS = void $ forkChild $ executorAI fid connS
      forkClient fid (connUI, connAI) = do
        maybe skip (forkUI fid) connUI
        maybe skip (forkAI fid) connAI
  liftIO $ mapWithKeyM_ forkClient toSpawn
  liftIO $ putMVar fromM (nH, fdict)  -- restart Frontend

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
