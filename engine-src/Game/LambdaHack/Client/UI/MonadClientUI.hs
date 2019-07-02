-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getsSession
                 , modifySession
                 , updateClientLeader
                 , getCacheBfs
                 , getCachePath
                 )
    -- * Assorted primitives
  , clientPrintUI, getSession, putSession, displayFrames
  , connFrontendFrontKey, setFrontAutoYes, frontendShutdown, printScreen
  , chanFrontend, anyKeyPressed, discardPressedKey, resetPressedKeys
  , addPressedControlEsc, revCmdMap
  , getReportUI, getLeaderUI, getArenaUI, viewedLevelUI
  , leaderTgtToPos, xhairToPos, clearAimMode
  , getFontSetup, scoreToSlideshow, defaultHistory
  , tellAllClipPS, tellGameClipPS, elapsedSessionTimeGT
  , resetSessionStart, resetGameStart, partActorLeader, partPronounLeader
  , tryRestore, leaderSkillsClientUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connFrontend, displayFrame, addPressedKey
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import qualified Data.Vector.Unboxed as U
import qualified NLP.Miniutter.English as MU
import           System.FilePath
import           System.IO (hFlush, stdout)

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.File
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Definition.Ability as Ability

-- Assumes no interleaving with other clients, because each UI client
-- in a different terminal/window/machine.
clientPrintUI :: MonadClientUI m => Text -> m ()
clientPrintUI t = liftIO $ do
  T.hPutStrLn stdout t
  hFlush stdout

-- | The monad that gives the client access to UI operations,
-- but not to modifying client state.
class MonadClientRead m => MonadClientUI m where
  getsSession :: (SessionUI -> a) -> m a
  modifySession :: (SessionUI -> SessionUI) -> m ()
  updateClientLeader :: ActorId -> m ()
  getCacheBfs :: ActorId -> m (PointArray.Array BfsDistance)
  getCachePath :: ActorId -> Point -> m (Maybe AndPath)

getSession :: MonadClientUI m => m SessionUI
getSession = getsSession id

putSession :: MonadClientUI m => SessionUI -> m ()
putSession s = modifySession (const s)

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

displayFrame :: MonadClientUI m => Maybe Frame -> m ()
displayFrame mf = do
  frame <- case mf of
    Nothing -> return $! FrontDelay 1
    Just fr -> do
      modifySession $ \cli -> cli {snframes = snframes cli + 1}
      return $! FrontFrame fr
  connFrontend frame

-- | Push frames or delays to the frame queue. The frames depict
-- the @lid@ level.
displayFrames :: MonadClientUI m => LevelId -> PreFrames3 -> m ()
displayFrames lid frs = do
  let frames = case frs of
        [] -> []
        [Just ((bfr, ffr), (ovProp, ovMono))] ->
          [Just ((FrameBase $ U.unsafeThaw bfr, ffr), (ovProp, ovMono))]
        _ ->
          -- Due to the frames coming from the same base frame,
          -- we have to copy it to avoid picture corruption.
          map (fmap $ \((bfr, ffr), (ovProp, ovMono)) ->
                ((FrameBase $ U.thaw bfr, ffr), (ovProp, ovMono))) frs
  mapM_ displayFrame frames
  -- Can be different than @blid b@, e.g., when our actor is attacked
  -- on a remote level.
  lidV <- viewedLevelUI
  when (lidV == lid) $
    modifySession $ \sess -> sess {sdisplayNeeded = False}

-- | Write 'FrontKey' UI request to the frontend, read the reply,
-- set pointer, return key.
connFrontendFrontKey :: MonadClientUI m => [K.KM] -> PreFrame3 -> m K.KM
connFrontendFrontKey frontKeyKeys ((bfr, ffr), (ovProp, ovMono)) = do
  let frontKeyFrame = ((FrameBase $ U.unsafeThaw bfr, ffr), (ovProp, ovMono))
  kmp <- connFrontend $ FrontKey frontKeyKeys frontKeyFrame
  modifySession $ \sess -> sess {spointer = K.kmpPointer kmp}
  return $! K.kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ FrontAutoYes b

frontendShutdown :: MonadClientUI m => m ()
frontendShutdown = connFrontend FrontShutdown

printScreen :: MonadClientUI m => m ()
printScreen = connFrontend FrontPrintScreen

-- | Initialize the frontend chosen by the player via client options.
chanFrontend :: MonadClientUI m
             => ScreenContent -> ClientOptions -> m ChanFrontend
chanFrontend coscreen soptions =
  liftIO $ Frontend.chanFrontendIO coscreen soptions

anyKeyPressed :: MonadClientUI m => m Bool
anyKeyPressed = connFrontend FrontPressed

discardPressedKey :: MonadClientUI m => m ()
discardPressedKey = connFrontend FrontDiscardKey

resetPressedKeys :: MonadClientUI m => m ()
resetPressedKeys = connFrontend FrontResetKeys

addPressedKey :: MonadClientUI m => K.KMP -> m ()
addPressedKey = connFrontend . FrontAdd

addPressedControlEsc :: MonadClientUI m => m ()
addPressedControlEsc = addPressedKey K.KMP { K.kmpKeyMod = K.controlEscKM
                                           , K.kmpPointer = K.PointUI 0 0 }

revCmdMap :: MonadClientUI m => m (K.KM -> HumanCmd.HumanCmd -> K.KM)
revCmdMap = do
  CCUI{coinput=InputContent{brevMap}} <- getsSession sccui
  let revCmd dflt cmd = case M.lookup cmd brevMap of
        Nothing -> dflt
        Just (k : _) -> k
        Just [] -> error $ "" `showFailure` brevMap
  return revCmd

getReportUI :: MonadClientUI m => m Report
getReportUI = do
  sUIOptions <- getsSession sUIOptions
  report <- getsSession $ newReport . shistory
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      mem = EM.fromList <$> uMessageColors sUIOptions
      promptAI = toMsg mem MsgAlert "<press any key for main menu> "
  return $! if underAI then consReport promptAI report else report

getLeaderUI :: MonadClientUI m => m ActorId
getLeaderUI = do
  cli <- getClient
  case sleader cli of
    Nothing -> error $ "leader expected but not found" `showFailure` cli
    Just leader -> return leader

getArenaUI :: MonadClientUI m => m LevelId
getArenaUI = do
  let fallback = do
        side <- getsClient sside
        fact <- getsState $ (EM.! side) . sfactionD
        case gquit fact of
          Just Status{stDepth} -> return $! toEnum stDepth
          Nothing -> getEntryArena fact
  mleader <- getsClient sleader
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
viewedLevelUI = do
  arena <- getArenaUI
  saimMode <- getsSession saimMode
  return $! maybe arena aimLevelId saimMode

leaderTgtToPos :: MonadClientUI m => m (Maybe Point)
leaderTgtToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      mtgt <- getsClient $ getTarget aid
      getsState $ aidTgtToPos aid lidV mtgt

xhairToPos :: MonadClientUI m => m (Maybe Point)
xhairToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  sxhair <- getsSession sxhair
  case mleader of
    Nothing -> return Nothing  -- e.g., when game start and no leader yet
    Just aid -> getsState $ aidTgtToPos aid lidV sxhair
                  -- e.g., xhair on another level

-- If aim mode is exited, usually the player had the opportunity to deal
-- with xhair on a foe spotted on another level, so now move xhair
-- back to the leader level.
clearAimMode :: MonadClientUI m => m ()
clearAimMode = do
  lidVOld <- viewedLevelUI  -- not in aiming mode at this point
  mxhairPos <- xhairToPos  -- computed while still in aiming mode
  modifySession $ \sess -> sess {saimMode = Nothing}
  lidV <- viewedLevelUI  -- not in aiming mode at this point
  when (lidVOld /= lidV) $ do
    leader <- getLeaderUI
    lpos <- getsState $ bpos . getActorBody leader
    sxhairOld <- getsSession sxhair
    let xhairPos = fromMaybe lpos mxhairPos
        sxhair = case sxhairOld of
          Just TPoint{} -> Just $ TPoint TUnknown lidV xhairPos
            -- the point is possibly unknown on this level; unimportant anyway
          _ -> sxhairOld
    modifySession $ \sess -> sess {sxhair}

getFontSetup :: MonadClientUI m => m FontSetup
getFontSetup = do
  ClientOptions{sdlPropFontFile} <- getsClient soptions
  let multiFont = frontendName == "sdl"
                  && maybe False (not . T.null) sdlPropFontFile
  return $! if multiFont then multiFontSetup else singleFontSetup

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  fid <- getsClient sside
  scoreDict <- getsState shigh
  gameModeId <- getsState sgameModeId
  gameMode <- getGameMode
  time <- getsState stime
  dungeonTotal <- getsState sgold
  date <- liftIO getPOSIXTime
  tz <- liftIO $ getTimeZone $ posixSecondsToUTCTime date
  curChalSer <- getsClient scurChal
  factionD <- getsState sfactionD
  let fact = factionD EM.! fid
      table = HighScore.getTable gameModeId scoreDict
      gameModeName = mname gameMode
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
      (worthMentioning, (ntable, pos)) =
        HighScore.register table total dungeonTotal time status date chal
                           (T.unwords $ tail $ T.words $ gname fact)
                           ourVictims theirVictims
                           (fhiCondPoly $ gplayer fact)
  fontSetup <- getFontSetup
  let sli = highSlideshow fontSetup rwidth (rheight - 1) ntable pos
                          gameModeName tz
  return $! if worthMentioning
            then sli
            else emptySlideshow

defaultHistory :: MonadClientUI m => m History
defaultHistory = do
  sUIOptions <- getsSession sUIOptions
  liftIO $ do
    utcTime <- getCurrentTime
    timezone <- getTimeZone utcTime
    let curDate = T.pack $ take 19 $ show $ utcToLocalTime timezone utcTime
        emptyHist = emptyHistory $ uHistoryMax sUIOptions
        mem = EM.fromList <$> uMessageColors sUIOptions
        msg = toMsg mem MsgAdmin
              $ "History log started on " <> curDate <> "."
    return $! fst $ addToReport emptyHist msg 1 timeZero

tellAllClipPS :: MonadClientUI m => m ()
tellAllClipPS = do
  bench <- getsClient $ sbenchmark . soptions
  when bench $ do
    sstartPOSIX <- getsSession sstart
    curPOSIX <- liftIO getPOSIXTime
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

tellGameClipPS :: MonadClientUI m => m ()
tellGameClipPS = do
  bench <- getsClient $ sbenchmark . soptions
  when bench $ do
    sgstartPOSIX <- getsSession sgstart
    curPOSIX <- liftIO getPOSIXTime
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
elapsedSessionTimeGT stopAfter = do
  current <- liftIO getPOSIXTime
  sstartPOSIX <- getsSession sstart
  return $! fromIntegral stopAfter + sstartPOSIX <= current

resetSessionStart :: MonadClientUI m => m ()
resetSessionStart = do
  sstart <- liftIO getPOSIXTime
  modifySession $ \sess -> sess {sstart}
  resetGameStart

resetGameStart :: MonadClientUI m => m ()
resetGameStart = do
  sgstart <- liftIO getPOSIXTime
  time <- getsState stime
  nframes <- getsSession snframes
  modifySession $ \cli ->
    cli { sgstart
        , sallTime = absoluteTimeAdd (sallTime cli) time
        , snframes = 0
        , sallNframes = sallNframes cli + nframes }

-- | The part of speech describing the actor or the "you" pronoun if he is
-- the leader of the observer's faction.
partActorLeader :: MonadClientUI m => ActorId -> m MU.Part
partActorLeader aid = do
  mleader <- getsClient sleader
  bUI <- getsSession $ getActorUI aid
  b <- getsState $ getActorBody aid
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ | bhp b <= 0 -> MU.Phrase ["the fallen", partActor bUI]
    _ -> partActor bUI

-- | The part of speech with the actor's pronoun or "you" if a leader
-- of the client's faction.
partPronounLeader :: MonadClientUI m => ActorId -> m MU.Part
partPronounLeader aid = do
  mleader <- getsClient sleader
  bUI <- getsSession $ getActorUI aid
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partPronoun bUI

-- | Try to read saved client game state from the file system.
tryRestore :: MonadClientUI m => m (Maybe (StateClient, Maybe SessionUI))
tryRestore = do
  cops@COps{corule} <- getsState scops
  bench <- getsClient $ sbenchmark . soptions
  if bench then return Nothing
  else do
    side <- getsClient sside
    prefix <- getsClient $ ssavePrefixCli . soptions
    let fileName = prefix <> Save.saveNameCli cops side
    res <- liftIO $ Save.restoreGame cops fileName
    let cfgUIName = rcfgUIName corule
        content = rcfgUIDefault corule
    dataDir <- liftIO appDataDir
    liftIO $ tryWriteFile (dataDir </> cfgUIName) content
    return res

leaderSkillsClientUI :: MonadClientUI m => m Ability.Skills
leaderSkillsClientUI = do
  leader <- getLeaderUI
  getsState $ getActorMaxSkills leader
