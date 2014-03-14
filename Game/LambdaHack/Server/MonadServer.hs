-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.MonadServer
  ( -- * Action monads
    MonadServer( getServer, getsServer, modifyServer, putServer, saveServer
               , liftIO  -- ^ exposed only to be implemented, not used
               )
  , MonadConnServer( getDict -- ^ exposed only to be implemented, not used
                   , getsDict  -- ^ exposed only to be implemented, not used
                   , modifyDict  -- ^ exposed only to be implemented, not used
                   , putDict  -- ^ exposed only to be implemented, not used
                   )
  , tryRestore, updateConn, killAllClients, speedupCOps
    -- * Communication
  , sendUpdateAI, sendQueryAI, sendPingAI
  , sendUpdateUI, sendQueryUI, sendPingUI
    -- * Assorted primitives
  , debugPrint, dumpRngs
  , getSetGen, restoreScore, revealItems, deduceQuits
  , rndToAction, resetSessionStart, resetGameStart, elapsedSessionTimeGT
  , tellAllClipPS, tellGameClipPS
  , resetFidPerception, getPerFid
  , childrenServer, saveName
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex hiding (handle)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM, mapWithKeyM_)
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game.LambdaHack.Utils.Thread
import System.Directory
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.Save
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Frontend as Frontend
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.File

class MonadReadState m => MonadServer m where
  getServer    :: m StateServer
  getsServer   :: (StateServer -> a) -> m a
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveServer   :: m ()

class MonadServer m => MonadConnServer m where
  getDict      :: m ConnServerDict
  getsDict     :: (ConnServerDict -> a) -> m a
  modifyDict   :: (ConnServerDict -> ConnServerDict) -> m ()
  putDict      :: ConnServerDict -> m ()

saveName :: String
saveName = serverSaveName

debugPrint :: MonadServer m => Text -> m ()
debugPrint t = do
  debug <- getsServer $ sdbgMsgSer . sdebugSer
  when debug $ liftIO $ do
    T.hPutStrLn stderr t
    hFlush stderr

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m ()
resetFidPerception fid lid = do
  cops <- getsState scops
  lvl <- getLevel lid
  fovMode <- getsServer $ sfovMode . sdebugSer
  per <- getsState
         $ levelPerception cops (fromMaybe (Digital 12) fovMode) fid lid lvl
  let upd = EM.adjust (EM.adjust (const per) lid) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}

getPerFid :: MonadServer m => FactionId -> LevelId -> m Perception
getPerFid fid lid = do
  pers <- getsServer sper
  let fper = fromMaybe (assert `failure` "no perception for faction"
                               `twith` (lid, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` "no perception for level"
                              `twith` (lid, fid)) $ EM.lookup lid fper
  return $! per

-- | Dumps RNG states from the start of the game to stderr.
dumpRngs :: MonadServer m => m ()
dumpRngs = do
  rngs <- getsServer srngs
  liftIO $ do
    T.hPutStrLn stderr $ tshow rngs
    hFlush stderr

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

-- TODO: refactor wrt Game.LambdaHack.Common.Save
-- | Read the high scores table. Return the empty table if no file.
restoreScore :: MonadServer m => Kind.COps -> m HighScore.ScoreTable
restoreScore Kind.COps{corule} = do
  let stdRuleset = Kind.stdRuleset corule
      scoresFile = rscoresFile stdRuleset
  dataDir <- liftIO appDataDir
  let path = dataDir </> scoresFile
  configExists <- liftIO $ doesFileExist path
  mscore <- liftIO $ do
    res <- Ex.try $
      if configExists then do
        s <- strictDecodeEOF path
        return $ Just s
      else return Nothing
    let handler :: Ex.SomeException -> IO (Maybe a)
        handler e = do
          let msg = "High score restore failed. The error message is:"
                    <+> (T.unwords . T.lines) (tshow e)
          delayPrint $ msg
          return Nothing
    either handler return res
  maybe (return HighScore.empty) return mscore

-- | Generate a new score, register it and save.
registerScore :: MonadServer m => Status -> Maybe Actor -> FactionId -> m ()
registerScore status mbody fid = do
  cops@Kind.COps{corule} <- getsState scops
  assert (maybe True ((fid ==) . bfid) mbody) skip
  fact <- getsState $ (EM.! fid) . sfactionD
  total <- case mbody of
    Just body -> getsState $ snd . calculateTotal body
    Nothing -> case gleader fact of
      Nothing -> return 0
      Just aid -> do
        b <- getsState $ getActorBody aid
        getsState $ snd . calculateTotal b
  let stdRuleset = Kind.stdRuleset corule
      scoresFile = rscoresFile stdRuleset
  dataDir <- liftIO appDataDir
  -- Re-read the table in case it's changed by a concurrent game.
  table <- restoreScore cops
  time <- getsState stime
  date <- liftIO getClockTime
  DebugModeSer{sdifficultySer} <- getsServer sdebugSer
  factionD <- getsState sfactionD
  dungeon <- getsState sdungeon
  let path = dataDir </> scoresFile
      outputScore (worthMentioning, (ntable, pos)) =
        -- If not human, probably debugging, so dump instead of registering.
        if not $ playerAiLeader $ gplayer fact then
          if worthMentioning then
            liftIO $ encodeEOF path (ntable :: HighScore.ScoreTable)
          else return ()
        else
          debugPrint $ T.intercalate "\n"
          $ HighScore.showScore (pos, HighScore.getRecord pos ntable)
      diff | not $ playerUI $ gplayer fact = difficultyDefault
           | otherwise = sdifficultySer
      theirVic (fi, fa) | isAtWar fact fi
                          && not (isHorrorFact cops fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isAllied fact fi || fi == fid = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      fightsAgainstSpawners =
        let escape = any lescape $ EM.elems dungeon
            isSpawner = isSpawnFact fact
        in escape && not isSpawner
      registeredScore =
        HighScore.register table total time status date diff
                           (playerName $ gplayer fact)
                           ourVictims theirVictims fightsAgainstSpawners
  outputScore registeredScore

resetSessionStart :: MonadServer m => m ()
resetSessionStart = do
  sstart <- liftIO getClockTime
  modifyServer $ \ser -> ser {sstart}

-- TODO: all this breaks when games are loaded; we'd need to save
-- elapsed game clock time to fix this.
resetGameStart :: MonadServer m => m ()
resetGameStart = do
  sgstart <- liftIO getClockTime
  time <- getsState stime
  modifyServer $ \ser ->
    ser {sgstart, sallTime = timeAdd (sallTime ser) time}

elapsedSessionTimeGT :: MonadServer m => Int -> m Bool
elapsedSessionTimeGT stopAfter = do
  current <- liftIO getClockTime
  TOD s p <- getsServer sstart
  return $! TOD (s + fromIntegral stopAfter) p <= current

tellAllClipPS :: MonadServer m => m ()
tellAllClipPS = do
  bench <- getsServer $ sbenchmark . sdebugSer
  when bench $ do
    TOD s p <- getsServer sstart
    TOD sCur pCur <- liftIO getClockTime
    allTime <- getsServer sallTime
    gtime <- getsState stime
    let time = timeAdd allTime gtime
    let diff = fromIntegral sCur + fromIntegral pCur / 10e12
               - fromIntegral s - fromIntegral p / 10e12
        cps = fromIntegral (timeFit time timeClip) / diff :: Double
    debugPrint $ "Session time:" <+> tshow diff <> "s."
                 <+> "Average clips per second:" <+> tshow cps <> "."

tellGameClipPS :: MonadServer m => m ()
tellGameClipPS = do
  bench <- getsServer $ sbenchmark . sdebugSer
  when bench $ do
    TOD s p <- getsServer sgstart
    unless (s == 0) $ do  -- loaded game, don't report anything
      TOD sCur pCur <- liftIO getClockTime
      time <- getsState stime
      let diff = fromIntegral sCur + fromIntegral pCur / 10e12
                 - fromIntegral s - fromIntegral p / 10e12
          cps = fromIntegral (timeFit time timeClip) / diff :: Double
      debugPrint $ "Game time:" <+> tshow diff <> "s."
                   <+> "Average clips per second:" <+> tshow cps <> "."

revealItems :: (MonadAtomic m, MonadServer m)
            => Maybe FactionId -> Maybe Actor -> m ()
revealItems mfid mbody = do
  dungeon <- getsState sdungeon
  discoS <- getsServer sdisco
  let discover b iid _numPieces = do
        item <- getsState $ getItemBody iid
        let ik = fromJust $ jkind discoS item
        execUpdAtomic $ UpdDiscover (blid b) (bpos b) iid ik
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
      when (playerUI $ gplayer fact) $ do
        revealItems (Just fid) mbody
        registerScore status mbody fid
      execUpdAtomic $ UpdQuitFaction fid mbody oldSt $ Just status
      modifyServer $ \ser -> ser {squit = True}  -- end turn ASAP

-- Send any QuitFactionA actions that can be deduced from their current state.
deduceQuits :: (MonadAtomic m, MonadServer m) => Actor -> Status -> m ()
deduceQuits body status@Status{stOutcome}
  | stOutcome `elem` [Defeated, Camping, Restart, Conquer] =
    assert `failure` "no quitting to deduce" `twith` (status, body)
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
      assocsNotHorror = filter (not . isHorrorFact cops . snd) assocsInGame
      assocsUI = filter (playerUI . gplayer . snd) assocsInGame
  case assocsNotHorror of
    _ | null assocsUI ->
      -- Only non-UI players left in the game and they all win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    [] ->
      -- Only horrors remain, so they win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    (_, fact1) : rest | all (not . isAtWar fact1 . fst) rest ->
      -- Nobody is at war any more, so all win.
      -- TODO: check not at war with each other.
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | stOutcome status == Escape -> do
      -- Otherwise, in a game with many warring teams alive,
      -- only complete Victory matters, until enough of them die.
      let (victors, losers) = partition (flip isAllied fid . snd) assocsInGame
      mapQuitF status{stOutcome=Escape} $ map fst victors
      mapQuitF status{stOutcome=Defeated} $ map fst losers
    _ -> return ()

tryRestore :: MonadServer m
           => Kind.COps -> DebugModeSer -> m (Maybe (State, StateServer))
tryRestore Kind.COps{corule} sdebugSer = do
  let stdRuleset = Kind.stdRuleset corule
      scoresFile = rscoresFile stdRuleset
      pathsDataFile = rpathsDataFile stdRuleset
      prefix = ssavePrefixSer sdebugSer
  let copies = [( "GameDefinition" </> scoresFile
                , scoresFile )]
      name = fromMaybe "save" prefix <.> saveName
  liftIO $ Save.restoreGame name copies pathsDataFile

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

killAllClients :: (MonadAtomic m, MonadConnServer m) => m ()
killAllClients = do
  d <- getDict
  let sendKill fid _ = do
        -- We can't check in sfactionD, because client can be from an old game.
        when (fromEnum fid > 0) $
          sendUpdateUI fid $ RespUpdAtomicUI $ UpdKillExit fid
        sendUpdateAI fid $ RespUpdAtomicAI $ UpdKillExit fid
  mapWithKeyM_ sendKill d

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code.
speedupCOps :: Bool -> Kind.COps -> Kind.COps
speedupCOps allClear copsSlow@Kind.COps{cotile=tile} =
  let ospeedup = Tile.speedup allClear tile
      cotile = tile {Kind.ospeedup = Just ospeedup}
  in copsSlow {Kind.cotile = cotile}

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer $ \ser -> ser {srandom = ng}
  return $! a

-- | Gets a random generator from the arguments or, if not present,
-- generates one.
getSetGen :: MonadServer m
          => Maybe R.StdGen
          -> m R.StdGen
getSetGen mrng = case mrng of
  Just rnd -> return rnd
  Nothing -> liftIO $ R.newStdGen
