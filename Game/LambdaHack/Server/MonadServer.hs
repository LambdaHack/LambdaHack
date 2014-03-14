-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.MonadServer
  ( -- * The server monad
    MonadServer( getServer, getsServer, modifyServer, putServer, saveServer
               , liftIO  -- exposed only to be implemented, not used
               )
  , tryRestore, speedupCOps
    -- * Assorted primitives
  , debugPrint, dumpRngs
  , getSetGen, restoreScore, revealItems, deduceQuits
  , rndToAction, resetSessionStart, resetGameStart, elapsedSessionTimeGT
  , tellAllClipPS, tellGameClipPS
  , resetFidPerception, getPerFid, saveName
  ) where

import qualified Control.Exception as Ex hiding (handle)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
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
import Game.LambdaHack.Common.Save
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
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
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m Perception
resetFidPerception fid lid = do
  cops <- getsState scops
  lvl <- getLevel lid
  fovMode <- getsServer $ sfovMode . sdebugSer
  per <- getsState
         $ levelPerception cops (fromMaybe (Digital 12) fovMode) fid lid lvl
  let upd = EM.adjust (EM.adjust (const per) lid) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}
  return $! per

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
