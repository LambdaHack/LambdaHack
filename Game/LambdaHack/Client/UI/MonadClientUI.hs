-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getsSession
                 , modifySession
                 , liftIO  -- exposed only to be implemented, not used,
                 )
    -- * Assorted primitives
  , clientPrintUI, mapStartY, getSession, putSession, displayFrames
  , connFrontendFrontKey, setFrontAutoYes, frontendShutdown, printScreen
  , chanFrontend, anyKeyPressed, discardPressedKey, addPressedEsc, revCmdMap
  , getReportUI, getLeaderUI, getArenaUI, viewedLevelUI
  , leaderTgtToPos, xhairToPos, clearXhair, clearAimMode
  , scoreToSlideshow, defaultHistory
  , tellAllClipPS, tellGameClipPS, elapsedSessionTimeGT
  , resetSessionStart, resetGameStart
  , partActorLeader, partActorLeaderFun, partPronounLeader, partAidLeader
  , tryRestore, leaderSkillsClientUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connFrontend, displayFrame, addPressedKey
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import qualified NLP.Miniutter.English as MU
import           System.FilePath
import           System.IO (hFlush, stdout)

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient hiding (liftIO)
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.KeyBindings
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.File
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind

-- Assumes no interleaving with other clients, because each UI client
-- in a different terminal/window/machine.
clientPrintUI :: MonadClientUI m => Text -> m ()
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
  liftIO        :: IO a -> m a

getSession :: MonadClientUI m => m SessionUI
getSession = getsSession id

putSession :: MonadClientUI m => SessionUI -> m ()
putSession s = modifySession (const s)

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

displayFrame :: MonadClientUI m => Maybe FrameForall -> m ()
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
connFrontendFrontKey frontKeyKeys frontKeyFrame = do
  kmp <- connFrontend FrontKey{..}
  modifySession $ \sess -> sess {spointer = K.kmpPointer kmp}
  return $! K.kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ FrontAutoYes b

frontendShutdown :: MonadClientUI m => m ()
frontendShutdown = connFrontend FrontShutdown

printScreen :: MonadClientUI m => m ()
printScreen = connFrontend FrontPrintScreen

-- | Initialize the frontend chosen by the player via client options.
chanFrontend :: MonadClientUI m => ClientOptions -> m ChanFrontend
chanFrontend = liftIO . Frontend.chanFrontendIO

anyKeyPressed :: MonadClientUI m => m Bool
anyKeyPressed = connFrontend FrontPressed

discardPressedKey :: MonadClientUI m => m ()
discardPressedKey = connFrontend FrontDiscard

addPressedKey :: MonadClientUI m => K.KMP -> m ()
addPressedKey = connFrontend . FrontAdd

addPressedEsc :: MonadClientUI m => m ()
addPressedEsc = addPressedKey K.KMP { K.kmpKeyMod = K.escKM
                                    , K.kmpPointer = originPoint }

revCmdMap :: MonadClientUI m => m (K.KM -> HumanCmd.HumanCmd -> K.KM)
revCmdMap = do
  Binding{brevMap} <- getsSession sbinding
  let revCmd dflt cmd = case M.lookup cmd brevMap of
        Nothing -> dflt
        Just (k : _) -> k
        Just [] -> error $ "" `showFailure` brevMap
  return revCmd

getReportUI :: MonadClientUI m => m Report
getReportUI = do
  report <- getsSession $ newReport . shistory
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      promptAI = toPrompt $ stringToAL "[press ESC for main menu]"
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
      case mtgt of
        Nothing -> return Nothing
        Just tgt -> getsState $ aidTgtToPos aid lidV tgt

xhairToPos :: MonadClientUI m => m (Maybe Point)
xhairToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  sxhair <- getsSession sxhair
  case mleader of
    Nothing -> return Nothing  -- e.g., when game start and no leader yet
    Just aid -> getsState $ aidTgtToPos aid lidV sxhair
                  -- e.g., xhair on another level

-- Reset xhair and move it to actor's position.
clearXhair :: MonadClientUI m => m ()
clearXhair = do
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  lidV <- viewedLevelUI  -- don't assume aiming mode is or will be off
  modifySession $ \sess -> sess {sxhair = TPoint TAny lidV lpos}

-- If aim mode is exited, usually the player had the opportunity to deal
-- with xhair on a foe spotted on another level, so now move xhair
-- back to the leader level.
clearAimMode :: MonadClientUI m => m ()
clearAimMode = do
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  xhairPos <- xhairToPos  -- computed while still in aiming mode
  modifySession $ \sess -> sess {saimMode = Nothing}
  lidV <- viewedLevelUI  -- not in aiming mode at this point
  sxhairOld <- getsSession sxhair
  let cpos = fromMaybe lpos xhairPos
      sxhair = case sxhairOld of
        TEnemy{} -> sxhairOld
        TVector{} -> sxhairOld
        _ -> TPoint TAny lidV cpos
  modifySession $ \sess -> sess {sxhair}

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
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
      (msg, tts) = HighScore.highSlideshow ntable pos gameModeName tz
      al = textToAL msg
      splitScreen ts =
        splitOKX lxsize (lysize + 3) al [K.spaceKM, K.escKM] (ts, [])
      sli = toSlideshow $ concat $ map (splitScreen . map textToAL) tts
  return $! if worthMentioning
            then sli
            else emptySlideshow

defaultHistory :: MonadClientUI m => Int -> m History
defaultHistory uHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = take 19 $ show $ utcToLocalTime timezone utcTime
      emptyHist = emptyHistory uHistoryMax
      msg = toMsg $ stringToAL $ "History log started on " ++ curDate ++ "."
  return $! fst $ addToReport emptyHist msg 0

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

-- | The part of speech describing the actor or "you" if a leader
-- of the client's faction. The actor may be not present in the dungeon.
partActorLeader :: MonadClientUI m => ActorId -> ActorUI -> m MU.Part
partActorLeader aid b = do
  mleader <- getsClient sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor b

partActorLeaderFun :: MonadClientUI m => m (ActorId -> MU.Part)
partActorLeaderFun = do
  mleader <- getsClient sleader
  sess <- getSession
  return $! \aid ->
    if mleader == Just aid
    then "you"
    else partActor $ getActorUI aid sess

-- | The part of speech with the actor's pronoun or "you" if a leader
-- of the client's faction. The actor may be not present in the dungeon.
partPronounLeader :: MonadClient m => ActorId -> ActorUI -> m MU.Part
partPronounLeader aid b = do
  mleader <- getsClient sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partPronoun b

-- | The part of speech describing the actor (designated by actor id
-- and present in the dungeon) or a special name if a leader
-- of the observer's faction.
partAidLeader :: MonadClientUI m => ActorId -> m MU.Part
partAidLeader aid = do
  b <- getsSession $ getActorUI aid
  partActorLeader aid b

-- | Try to read saved client game state from the file system.
tryRestore :: MonadClientUI m => m (Maybe (StateClient, Maybe SessionUI))
tryRestore = do
  cops <- getsState scops
  bench <- getsClient $ sbenchmark . soptions
  if bench then return Nothing
  else do
    side <- getsClient sside
    prefix <- getsClient $ ssavePrefixCli . soptions
    let fileName = prefix <> Save.saveNameCli cops side
    res <- liftIO $ Save.restoreGame cops fileName
    let stdRuleset = getStdRuleset cops
        cfgUIName = rcfgUIName stdRuleset
        content = rcfgUIDefault stdRuleset
    dataDir <- liftIO appDataDir
    liftIO $ tryWriteFile (dataDir </> cfgUIName) content
    return res

leaderSkillsClientUI :: MonadClientUI m => m Ability.Skills
leaderSkillsClientUI = do
  leader <- getLeaderUI
  maxActorSkillsClient leader
