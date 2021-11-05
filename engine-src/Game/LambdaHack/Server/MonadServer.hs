-- | Basic server monads and related operations.
module Game.LambdaHack.Server.MonadServer
  ( -- * The server monad
    MonadServer( getsServer
               , modifyServer
               , chanSaveServer  -- exposed only to be implemented, not used
               , liftIO  -- exposed only to be implemented, not used
               )
  , MonadServerAtomic(..)
    -- * Assorted primitives
  , getServer, putServer, debugPossiblyPrint, debugPossiblyPrintAndExit
  , serverPrint, saveServer, dumpRngs, restoreScore, registerScore
  , rndToAction, getSetGen
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Control.Exception as Ex
import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.Exit (exitFailure)
import           System.FilePath
import           System.IO (hFlush, stdout)
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions (sbenchmark)
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.File
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.PlayerKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

class MonadStateRead m => MonadServer m where
  getsServer     :: (StateServer -> a) -> m a
  modifyServer   :: (StateServer -> StateServer) -> m ()
  chanSaveServer :: m (Save.ChanSave (State, StateServer))
  -- We do not provide a MonadIO instance, so that outside
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO         :: IO a -> m a

-- | The monad for executing atomic game state transformations.
class MonadServer m => MonadServerAtomic m where
  -- | Execute an atomic command that changes the state
  -- on the server and on all clients that can notice it.
  execUpdAtomic :: UpdAtomic -> m ()
  -- | Execute an atomic command that changes the state
  -- on the server only.
  execUpdAtomicSer :: UpdAtomic -> m Bool
  -- | Execute an atomic command that changes the state
  -- on the given single client only.
  execUpdAtomicFid :: FactionId -> UpdAtomic -> m ()
  -- | Execute an atomic command that changes the state
  -- on the given single client only.
  -- Catch 'AtomicFail' and indicate if it was in fact raised.
  execUpdAtomicFidCatch :: FactionId -> UpdAtomic -> m Bool
  -- | Execute an atomic command that only displays special effects.
  execSfxAtomic :: SfxAtomic -> m ()
  execSendPer :: FactionId -> LevelId
              -> Perception -> Perception -> Perception -> m ()

getServer :: MonadServer m => m StateServer
getServer = getsServer id

putServer :: MonadServer m => StateServer -> m ()
putServer s = modifyServer (const s)

debugPossiblyPrint :: MonadServer m => Text -> m ()
debugPossiblyPrint t = do
  debug <- getsServer $ sdbgMsgSer . soptions
  when debug $ liftIO $ do
    T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
    hFlush stdout

-- No moving savefiles aside, to debug more easily.
debugPossiblyPrintAndExit :: MonadServer m => Text -> m ()
debugPossiblyPrintAndExit t = do
  debug <- getsServer $ sdbgMsgSer . soptions
  when debug $ liftIO $ do
    T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
    hFlush stdout
    exitFailure

serverPrint :: MonadServer m => Text -> m ()
serverPrint t = liftIO $ do
  T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
  hFlush stdout

saveServer :: MonadServer m => m ()
saveServer = do
  s <- getState
  ser <- getServer
  toSave <- chanSaveServer
  liftIO $ Save.saveToChan toSave (s, ser)

-- | Dumps to stdout the RNG states from the start of the game.
dumpRngs :: MonadServer m => RNGs -> m ()
dumpRngs rngs = liftIO $ do
  T.hPutStr stdout $! tshow rngs <> "\n"  -- hPutStrLn not atomic enough
  hFlush stdout

-- | Read the high scores dictionary. Return the empty table if no file.
restoreScore :: forall m. MonadServer m => COps -> m HighScore.ScoreDict
restoreScore COps{corule} = do
  benchmark <- getsServer $ sbenchmark . sclientOptions . soptions
  mscore <- if benchmark then return Nothing else do
    let scoresFile = rscoresFile corule
    dataDir <- liftIO appDataDir
    let path bkp = dataDir </> bkp <> scoresFile
    configExists <- liftIO $ doesFileExist (path "")
    res <- liftIO $ Ex.try $
      if configExists then do
        (vlib2, s) <- strictDecodeEOF (path "")
        if Save.compatibleVersion vlib2 Self.version
        then return $! s `seq` Just s
        else do
          let msg =
                "High score file from incompatible version of game detected."
          fail msg
      else return Nothing
    savePrefix <- getsServer $ ssavePrefixSer . soptions
    let defPrefix = ssavePrefixSer defServerOptions
        moveAside = savePrefix == defPrefix
        handler :: Ex.SomeException -> m (Maybe a)
        handler e = do
          when moveAside $
            liftIO $ renameFile (path "") (path "bkp.")
          let msg = "High score restore failed."
                    <+> (if moveAside
                        then "The wrong file moved aside."
                        else "")
                    <+> "The error message is:"
                    <+> (T.unwords . T.lines) (tshow e)
          serverPrint msg
          return Nothing
    either handler return res
  maybe (return HighScore.empty) return mscore

-- | Generate a new score, register it and save.
registerScore :: MonadServer m => Status -> FactionId -> m ()
registerScore status fid = do
  cops@COps{corule} <- getsState scops
  total <- getsState $ snd . calculateTotal fid
  let scoresFile = rscoresFile corule
  dataDir <- liftIO appDataDir
  -- Re-read the table in case it's changed by a concurrent game.
  scoreDict <- restoreScore cops
  gameModeId <- getsState sgameModeId
  time <- getsState stime
  dungeonTotal <- getsState sgold
  date <- liftIO getPOSIXTime
  tz <- liftIO $ getTimeZone $ posixSecondsToUTCTime date
  curChalSer <- getsServer $ scurChalSer . soptions
  factionD <- getsState sfactionD
  bench <- getsServer $ sbenchmark . sclientOptions . soptions
  noConfirmsGame <- isNoConfirmsGame
  sbandSpawned <- getsServer sbandSpawned
  let fact = factionD EM.! fid
      path = dataDir </> scoresFile
      outputScore (worthMentioning, (ntable, pos)) =
        -- If testing or fooling around, dump instead of registering.
        -- In particular don't register score for the auto-* scenarios.
        if bench || noConfirmsGame || gunderAI fact then
          debugPossiblyPrint $ T.intercalate "\n"
          $ HighScore.showScore tz pos (HighScore.getRecord pos ntable)
            ++ ["           Spawned groups:"
                <+> T.unwords (tail (T.words (tshow sbandSpawned)))]
        else
          let nScoreDict = EM.insert gameModeId ntable scoreDict
          in when worthMentioning $ liftIO $
               encodeEOF path Self.version (nScoreDict :: HighScore.ScoreDict)
      chal | fhasUI $ gplayer fact = curChalSer
           | otherwise = curChalSer
                           {cdiff = difficultyInverse (cdiff curChalSer)}
      theirVic (fi, fa) | isFoe fid fact fi
                          && not (isHorrorFact fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isFriend fid fact fi = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      table = HighScore.getTable gameModeId scoreDict
      registeredScore =
        HighScore.register table total dungeonTotal time status date chal
                           (T.unwords $ tail $ T.words $ gname fact)
                           ourVictims theirVictims
                           (fhiCondPoly $ gplayer fact)
  outputScore registeredScore

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  gen1 <- getsServer srandom
  let (a, gen2) = St.runState r gen1
  modifyServer $ \ser -> ser {srandom = gen2}
  return a

-- | Gets a random generator from the user-submitted options or, if not present,
-- generates one.
getSetGen :: MonadServer m => Maybe SM.SMGen -> m SM.SMGen
getSetGen mrng = case mrng of
  Just rnd -> return rnd
  Nothing -> liftIO SM.newSMGen
