-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServer( getServer, getsServer, putServer, modifyServer )
  , MonadConnServer
  , tryRestore, updateConn, killAllClients, waitForChildren, speedupCOps
    -- * Communication
  , sendUpdateUI, sendQueryUI, sendUpdateAI, sendQueryAI
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg
  , mkConfigRules, restoreScore, revealItems, deduceQuits
  , rndToAction, fovMode, resetFidPerception, getPerFid
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM, mapWithKeyM_)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Frontend as Frontend
import Game.LambdaHack.Server.Action.ActionClass
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.File

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
  writeTQueueAI cmd $ fromServer conn

sendQueryAI :: MonadConnServer m => FactionId -> ActorId -> m CmdSer
sendQueryAI fid aid = do
  conn <- getsDict $ snd . (EM.! fid)
  writeTQueueAI (CmdQueryAI aid) $ fromServer conn
  readTQueue $ toServer conn

sendUpdateUI :: MonadConnServer m => FactionId -> CmdClientUI -> m ()
sendUpdateUI fid cmd = do
  conn <- getsDict $ snd . fst . (EM.! fid)
  writeTQueueUI cmd $ fromServer conn

sendQueryUI :: MonadConnServer m => FactionId -> ActorId -> m CmdSer
sendQueryUI fid aid = do
  conn <- getsDict $ snd . fst . (EM.! fid)
  writeTQueueUI (CmdQueryUI aid) $ fromServer conn
  readTQueue $ toServer conn

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
registerScore :: MonadServer m => Status -> Maybe Actor -> FactionId -> m ()
registerScore status mbody fid = do
  assert (maybe True ((fid ==) . bfid) mbody) skip
  factionD <- getsState sfactionD
  let fact = factionD EM.! fid
  assert (isHumanFact fact) skip
  total <- case mbody of
    Just body -> getsState $ snd . calculateTotal body
    Nothing -> case gleader fact of
      Nothing -> return 0
      Just aid -> do
        b <- getsState $ getActorBody aid
        getsState $ snd . calculateTotal b
  config <- getsServer sconfig
  -- Re-read the table in case it's changed by a concurrent game.
  table <- restoreScore config
  time <- getsState stime
  date <- liftIO getClockTime
  let saveScore (ntable, _) =
        liftIO $ encodeEOF (configScoresFile config)
                           (ntable :: HighScore.ScoreTable)
  maybe skip saveScore $ HighScore.register table total time status date

revealItems :: (MonadAtomic m, MonadServer m)
            => Maybe FactionId -> Maybe Actor -> m ()
revealItems mfid mbody = do
  dungeon <- getsState sdungeon
  discoS <- getsServer sdisco
  let discover b iid _numPieces = do
        item <- getsState $ getItemBody iid
        let ik = fromJust $ jkind discoS item
        execCmdAtomic $ DiscoverA (blid b) (bpos b) iid ik
      f aid = do
        b <- getsState $ getActorBody aid
        let ourSide = maybe True (== bfid b) mfid
        when (ourSide && Just b /= mbody) $ mapActorItems_ (discover b) b
  mapDungeonActors_ f dungeon
  maybe skip (\b -> mapActorItems_ (discover b) b) mbody

quitF :: (MonadAtomic m, MonadServer m)
      => Maybe Actor -> Status -> FactionId -> m ()
quitF mbody status fid = do
  assert (maybe True ((fid ==) . bfid) mbody) skip
  fact <- getsState $ (EM.! fid) . sfactionD
  let oldSt = gquit fact
  case fmap stOutcome $ oldSt of
    Just Killed -> return ()    -- Do not overwrite in case
    Just Defeated -> return ()  -- many things happen in 1 turn.
    Just Conquer -> return ()
    Just Escape -> return ()
    _ -> do
      when (isHumanFact fact) $ do
        revealItems (Just fid) mbody
        registerScore status mbody fid
      execCmdAtomic $ QuitFactionA fid mbody oldSt $ Just status
      modifyServer $ \ser -> ser {squit = True}  -- end turn ASAP

-- Send any QuitFactionA actions that can be deduced from their current state.
deduceQuits :: (MonadAtomic m, MonadServer m) => Actor -> Status -> m ()
deduceQuits body Status{stOutcome=Defeated} = assert `failure` body
deduceQuits body Status{stOutcome=Camping} = assert `failure` body
deduceQuits body Status{stOutcome=Restart} = assert `failure` body
deduceQuits body Status{stOutcome=Conquer} = assert `failure` body
deduceQuits body status = do
  cops <- getsState scops
  let fid = bfid body
      mapQuitF statusF fids = mapM_ (quitF Nothing statusF) $ delete fid fids
  quitF (Just body) status fid
  let inGame fact = case fmap stOutcome $ gquit fact of
        Just Killed -> False
        Just Defeated -> False
        Just Restart -> False  -- effectively, commits suicide
        _ -> True
  factionD <- getsState sfactionD
  let assocsInGame = filter (inGame . snd) $ EM.assocs factionD
      keysInGame = map fst assocsInGame
      assocsSpawn = filter (isSpawningFact cops . snd) assocsInGame
      assocsNotSpawn = filter (not . isSpawningFact cops . snd) assocsInGame
      assocsHuman = filter (isHumanFact . snd) assocsInGame
  case assocsNotSpawn of
    _ | null assocsHuman ->
      -- No screensaver mode for now --- all non-human players win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    [] ->
      -- Only spawners remain so all win, human or not, allied or not.
      mapQuitF status{stOutcome=Conquer} keysInGame
    (_, fact1) : rest | null assocsSpawn && all (isAllied fact1 . fst) rest ->
      -- Only one allied team remains in a no-spawners game.
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | stOutcome status == Escape -> do
      -- Otherwise, in a spawners game or a game with many teams alive,
      -- only complete Victory matters.
      let (victors, losers) = partition (flip isAllied fid . snd) assocsInGame
      mapQuitF status{stOutcome=Escape} $ map fst victors
      mapQuitF status{stOutcome=Defeated} $ map fst losers
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
      addConn fid _ = case EM.lookup fid oldD of
        Just conns -> return conns  -- share old conns and threads
        Nothing -> do
          connF <- mkChanFrontend
          connS <- mkChanServer
          connAI <- mkChanServer
          return ((connF, connS), connAI)
  factionD <- getsState sfactionD
  d <- liftIO $ mapWithKeyM addConn factionD
  let newD = d `EM.union` oldD  -- never kill old clients
  putDict newD
  -- Spawn and kill client threads.
  let toSpawn = newD EM.\\ oldD
      fdict fid = ( fst $ fst
                    $ fromMaybe (assert `failure` fid)
                    $ EM.lookup fid newD
                  , maybe T.empty gname  -- a faction can go inactive
                    $ EM.lookup fid factionD
                  )
      fromM = Frontend.fromMulti Frontend.connMulti
  liftIO $ void $ takeMVar fromM  -- stop Frontend
  let forkUI fid (connF, connS) = void $ forkChild $ executorUI fid connF connS
      forkAI fid connS = void $ forkChild $ executorAI fid connS
      forkClient fid (connUI, connAI) = do
        -- This is fragile: when a connection is reused, clients are not
        -- respawned, even if AI or UI usage changes (it does not, currently,
        -- thanks to faction numbering, etc.).
        -- TODO: perhaps spawn both AI and UI clients always, but then
        -- the UI client should not display a welcome message, until
        -- the server tells it to.
        when (isHumanFact $ factionD EM.! fid) $ forkUI fid connUI
        when (usesAIFact $ factionD EM.! fid) $ forkAI fid connAI
  liftIO $ mapWithKeyM_ forkClient toSpawn
  nH <- nHumans
  liftIO $ putMVar fromM (nH, fdict)  -- restart Frontend

killAllClients :: (MonadAtomic m, MonadConnServer m) => m ()
killAllClients = do
  d <- getDict
  let sendKill fid _ = do
        sendUpdateUI fid $ CmdAtomicUI $ KillExitA fid
        sendUpdateAI fid $ CmdAtomicAI $ KillExitA fid
  mapWithKeyM_ sendKill d

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
      cotile = tile {Kind.ospeedup = ospeedup}
  in copsSlow {Kind.cotile = cotile}

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer $ \ser -> ser {srandom = ng}
  return a
