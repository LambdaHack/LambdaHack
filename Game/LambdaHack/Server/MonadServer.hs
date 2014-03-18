-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.MonadServer
  ( -- * The server monad
    MonadServer( getServer, getsServer, modifyServer, putServer
               , saveChanServer  -- exposed only to be implemented, not used
               , liftIO  -- exposed only to be implemented, not used
               )
    -- * Assorted primitives
  , debugPrint, saveServer, saveName, dumpRngs
  , restoreScore, registerScore
  , resetSessionStart, resetGameStart, elapsedSessionTimeGT
  , tellAllClipPS, tellGameClipPS
  , tryRestore, speedupCOps, rndToAction, getSetGen
  ) where

import qualified Control.Exception as Ex hiding (handle)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Save
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.File

class MonadStateRead m => MonadServer m where
  getServer      :: m StateServer
  getsServer     :: (StateServer -> a) -> m a
  modifyServer   :: (StateServer -> StateServer) -> m ()
  putServer      :: StateServer -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO         :: IO a -> m a
  saveChanServer :: m (Save.ChanSave (State, StateServer))

debugPrint :: MonadServer m => Text -> m ()
debugPrint t = do
  debug <- getsServer $ sdbgMsgSer . sdebugSer
  when debug $ liftIO $ do
    T.hPutStrLn stderr t
    hFlush stderr

saveServer :: MonadServer m => m ()
saveServer = do
  s <- getState
  ser <- getServer
  toSave <- saveChanServer
  liftIO $ Save.saveToChan toSave (s, ser)

saveName :: String
saveName = serverSaveName

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
