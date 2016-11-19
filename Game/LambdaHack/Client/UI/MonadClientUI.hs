-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getsSession, modifySession, putSession
                 , liftIO  -- exposed only to be implemented, not used,
                 )
    -- * Assorted primitives
  , getSession, clientPrintUI, mapStartY, displayFrames
  , setFrontAutoYes, anyKeyPressed, discardPressedKey, addPressedEsc
  , connFrontendFrontKey, frontendShutdown, chanFrontend
  , getReportUI, getLeaderUI, getArenaUI, viewedLevelUI
  , leaderTgtToPos, leaderTgtAims, xhairToPos
  , scoreToSlideshow, defaultHistory
  , tellAllClipPS, tellGameClipPS, elapsedSessionTimeGT
  , resetSessionStart, resetGameStart
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import System.IO (hFlush, stdout)

import Game.LambdaHack.Client.CommonM
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind

-- Assumes no interleaving with other clients, because each UI client
-- in a different terminal/window/machine.
clientPrintUI :: MonadClientUI m => Text -> m ()
{-# INLINABLE clientPrintUI #-}
clientPrintUI t = liftIO $ do
  T.hPutStrLn stdout t
  hFlush stdout

-- | The row where the dungeon map starts.
mapStartY :: Y
mapStartY = 1

-- | The monad that gives the client access to UI operations.
class MonadClient m => MonadClientUI m where
  getsSession   :: (SessionUI -> a) -> m a
  modifySession :: (SessionUI -> SessionUI) -> m ()
  putSession    :: SessionUI -> m ()
  liftIO        :: IO a -> m a

getSession :: MonadClientUI m => m SessionUI
{-# INLINABLE getSession #-}
getSession = getsSession id

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
{-# INLINABLE connFrontend #-}
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

displayFrame :: MonadClientUI m => Maybe FrameForall -> m ()
{-# INLINABLE displayFrame #-}
displayFrame mf = do
  frame <- case mf of
    Nothing -> return $! FrontDelay 1
    Just fr -> do
      modifySession $ \cli -> cli {snframes = snframes cli + 1}
      return $! FrontFrame fr
  connFrontend frame

-- | Push frames or delays to the frame queue. The frames depict
-- the @lid@ level.
displayFrames :: MonadClientUI m => LevelId -> Frames -> m ()
{-# INLINABLE displayFrames #-}
displayFrames lid frs = do
  mapM_ displayFrame frs
  -- Can be different than @blid b@, e.g., when our actor is attacked
  -- on a remote level.
  lidV <- viewedLevelUI
  when (lidV == lid) $
    modifySession $ \sess -> sess {sdisplayNeeded = False}

-- | Write 'FrontKey' UI request to the frontend, read the reply,
-- set pointer, return key.
connFrontendFrontKey :: MonadClientUI m => [K.KM] -> FrameForall -> m K.KM
{-# INLINABLE connFrontendFrontKey #-}
connFrontendFrontKey frontKeyKeys frontKeyFrame = do
  kmp <- connFrontend $ FrontKey{..}
  modifySession $ \sess -> sess {spointer = kmpPointer kmp}
  return $! kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
{-# INLINABLE setFrontAutoYes #-}
setFrontAutoYes b = connFrontend $ FrontAutoYes b

anyKeyPressed :: MonadClientUI m => m Bool
{-# INLINABLE anyKeyPressed #-}
anyKeyPressed = connFrontend FrontPressed

discardPressedKey :: MonadClientUI m => m ()
{-# INLINABLE discardPressedKey #-}
discardPressedKey = connFrontend FrontDiscard

addPressedKey :: MonadClientUI m => KMP -> m ()
{-# INLINABLE addPressedKey #-}
addPressedKey = connFrontend . FrontAdd

addPressedEsc :: MonadClientUI m => m ()
{-# INLINABLE addPressedEsc #-}
addPressedEsc = addPressedKey$ KMP { kmpKeyMod = K.escKM
                                   , kmpPointer = originPoint }

frontendShutdown :: MonadClientUI m => m ()
{-# INLINABLE frontendShutdown #-}
frontendShutdown = connFrontend FrontShutdown

chanFrontend :: MonadClientUI m => DebugModeCli -> m ChanFrontend
{-# INLINABLE chanFrontend #-}
chanFrontend = liftIO . Frontend.chanFrontendIO

getReportUI :: MonadClientUI m => m Report
{-# INLINABLE getReportUI #-}
getReportUI = do
  report <- getsSession _sreport
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      promptAI = toPrompt $ stringToAL "[press any key for Main Menu]"
  return $! if underAI then consReportNoScrub promptAI report else report

getLeaderUI :: MonadClientUI m => m ActorId
{-# INLINABLE getLeaderUI #-}
getLeaderUI = do
  cli <- getClient
  case _sleader cli of
    Nothing -> assert `failure` "leader expected but not found" `twith` cli
    Just leader -> return leader

getArenaUI :: MonadClientUI m => m LevelId
{-# INLINABLE getArenaUI #-}
getArenaUI = do
  let fallback = do
        side <- getsClient sside
        fact <- getsState $ (EM.! side) . sfactionD
        case gquit fact of
          Just Status{stDepth} -> return $! toEnum stDepth
          Nothing -> getEntryArena fact
  mleader <- getsClient _sleader
  case mleader of
    Just leader -> do
      -- The leader may just be teleporting (e.g., due to displace
      -- over terrain not in FOV) so not existent momentarily.
      mem <- getsState $ EM.member leader . sactorD
      if mem
      then getsState $ blid . getActorBody leader
      else fallback
    Nothing -> fallback

viewedLevelUI :: MonadClientUI m => m LevelId
{-# INLINABLE viewedLevelUI #-}
viewedLevelUI = do
  arena <- getArenaUI
  saimMode <- getsSession saimMode
  return $! maybe arena aimLevelId saimMode

leaderTgtToPos :: MonadClientUI m => m (Maybe Point)
{-# INLINABLE leaderTgtToPos #-}
leaderTgtToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtToPos aid lidV tgt

leaderTgtAims :: MonadClientUI m => m (Either Text Int)
{-# INLINABLE leaderTgtAims #-}
leaderTgtAims = do
  lidV <- viewedLevelUI
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return $ Left "no leader to aim with"
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtAims aid lidV tgt

xhairToPos :: MonadClientUI m => m (Maybe Point)
{-# INLINABLE xhairToPos #-}
xhairToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient _sleader
  sxhair <- getsClient sxhair
  case mleader of
    Nothing -> return Nothing
    Just aid -> aidTgtToPos aid lidV $ Just sxhair

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
{-# INLINABLE scoreToSlideshow #-}
scoreToSlideshow total status = do
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
  fid <- getsClient sside
  fact <- getsState $ (EM.! fid) . sfactionD
  -- TODO: Re-read the table in case it's changed by a concurrent game.
  -- TODO: we should do this, and make sure we do that after server
  -- saved the updated score table, and not register, but read from it.
  -- Otherwise the score is not accurate, e.g., the number of victims.
  scoreDict <- getsState shigh
  gameModeId <- getsState sgameModeId
  gameMode <- getGameMode
  time <- getsState stime
  date <- liftIO getPOSIXTime
  tz <- liftIO $ getTimeZone $ posixSecondsToUTCTime date
  scurDiff <- getsClient scurDiff
  factionD <- getsState sfactionD
  let table = HighScore.getTable gameModeId scoreDict
      gameModeName = mname gameMode
      diff | fhasUI $ gplayer fact = scurDiff
           | otherwise = difficultyInverse scurDiff
      theirVic (fi, fa) | isAtWar fact fi
                          && not (isHorrorFact fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isAllied fact fi || fi == fid = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      (worthMentioning, (ntable, pos)) =
        HighScore.register table total time status date diff
                           (fname $ gplayer fact)
                           ourVictims theirVictims
                           (fhiCondPoly $ gplayer fact)
      (msg, tts) = HighScore.highSlideshow ntable pos gameModeName tz
      al = textToAL msg
      splitScreen ts =
        splitOKX lxsize (lysize + 3) al [K.spaceKM, K.escKM] (ts, [])
      sli = toSlideshow $ concat $ map (splitScreen . map textToAL) tts
  return $! if worthMentioning
            then sli
            else emptySlideshow

defaultHistory :: MonadClientUI m => Int -> m History
{-# INLINABLE defaultHistory #-}
defaultHistory configHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = show $ utcToLocalTime timezone utcTime
      emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $ singletonReport $ toMsg $ stringToAL
         $ "Human history log started on " ++ curDate ++ "."

tellAllClipPS :: MonadClientUI m => m ()
{-# INLINABLE tellAllClipPS #-}
tellAllClipPS = do
  bench <- getsClient $ sbenchmark . sdebugCli
  when bench $ do
    sstartPOSIX <- getsSession sstart
    curPOSIX <- liftIO $ getPOSIXTime
    allTime <- getsSession sallTime
    gtime <- getsState stime
    allNframes <- getsSession sallNframes
    gnframes <- getsSession snframes
    let time = absoluteTimeAdd allTime gtime
        nframes = allNframes + gnframes
        diff = fromRational $ toRational $ curPOSIX - sstartPOSIX
        cps = fromIntegral (timeFit time timeClip) / diff :: Double
        fps = fromIntegral nframes / diff :: Double
    clientPrintUI $
      "Session time:" <+> tshow diff <> "s; frames:" <+> tshow nframes <> "."
      <+> "Average clips per second:" <+> tshow cps <> "."
      <+> "Average FPS:" <+> tshow fps <> "."

-- TODO: dedup
tellGameClipPS :: MonadClientUI m => m ()
{-# INLINABLE tellGameClipPS #-}
tellGameClipPS = do
  bench <- getsClient $ sbenchmark . sdebugCli
  when bench $ do
    sgstartPOSIX <- getsSession sgstart
    curPOSIX <- liftIO $ getPOSIXTime
    -- If loaded game, don't report anything.
    unless (sgstartPOSIX == 0) $ do
      time <- getsState stime
      nframes <- getsSession snframes
      let diff = fromRational $ toRational $ curPOSIX - sgstartPOSIX
          cps = fromIntegral (timeFit time timeClip) / diff :: Double
          fps = fromIntegral nframes / diff :: Double
      -- This means: "Game portion after last reload time:...".
      clientPrintUI $
        "Game time:" <+> tshow diff <> "s; frames:" <+> tshow nframes <> "."
        <+> "Average clips per second:" <+> tshow cps <> "."
        <+> "Average FPS:" <+> tshow fps <> "."

elapsedSessionTimeGT :: MonadClientUI m => Int -> m Bool
{-# INLINABLE elapsedSessionTimeGT #-}
elapsedSessionTimeGT stopAfter = do
  current <- liftIO getPOSIXTime
  sstartPOSIX <- getsSession sstart
  return $! fromIntegral stopAfter + sstartPOSIX <= current

resetSessionStart :: MonadClientUI m => m ()
{-# INLINABLE resetSessionStart #-}
resetSessionStart = do
  sstart <- liftIO getPOSIXTime
  modifySession $ \sess -> sess {sstart}
  resetGameStart

resetGameStart :: MonadClientUI m => m ()
{-# INLINABLE resetGameStart #-}
resetGameStart = do
  sgstart <- liftIO getPOSIXTime
  time <- getsState stime
  nframes <- getsSession snframes
  modifySession $ \cli ->
    cli { sgstart
        , sallTime = absoluteTimeAdd (sallTime cli) time
        , snframes = 0
        , sallNframes = sallNframes cli + nframes }
