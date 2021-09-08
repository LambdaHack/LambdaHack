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
  , clientPrintUI, debugPossiblyPrintUI, getSession, putSession, displayFrames
  , connFrontendFrontKey, setFrontAutoYes, frontendShutdown, printScreen
  , chanFrontend, anyKeyPressed, discardPressedKey, resetPressedKeys
  , addPressedControlEsc, revCmdMap, getReportUI, computeChosenLore
  , miniHintAimingBare, miniHintAimingLore
  , getLeaderUI, getArenaUI, viewedLevelUI
  , xhairToPos, setXHairFromGUI, clearAimMode
  , getFontSetup, scoreToSlideshow, defaultHistory
  , tellAllClipPS, tellGameClipPS, elapsedSessionTimeGT
  , resetSessionStart, resetGameStart, partActorLeader, partPronounLeader
  , tryRestore, leaderSkillsClientUI, rndToActionUI, tryOpenBrowser
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connFrontend, displayFrame, addPressedKey
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import qualified Data.Vector.Unboxed as U
import qualified NLP.Miniutter.English as MU
import           System.FilePath
import           System.IO (hFlush, stdout)
import           Web.Browser (openBrowser)

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.File
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Item
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
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability

-- Assumes no interleaving with other clients, because each UI client
-- in a different terminal/window/machine.
clientPrintUI :: MonadClientUI m => Text -> m ()
clientPrintUI t = liftIO $ do
  T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
  hFlush stdout

debugPossiblyPrintUI :: MonadClientUI m => Text -> m ()
debugPossiblyPrintUI t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . soptions
  when sdbgMsgCli $ liftIO $ do
    T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
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
connFrontend :: MonadClientUI m => Frontend.FrontReq a -> m a
connFrontend req = do
  Frontend.ChanFrontend f <- getsSession schanF
  liftIO $ f req

displayFrame :: MonadClientUI m => Maybe Frame -> m ()
displayFrame mf = do
  frame <- case mf of
    Nothing -> return $! Frontend.FrontDelay 1
    Just fr -> do
      modifySession $ \cli -> cli {snframes = snframes cli + 1}
      return $! Frontend.FrontFrame fr
  connFrontend frame

-- | Push frames or delays to the frame queue. The frames depict
-- the @lid@ level.
displayFrames :: MonadClientUI m => LevelId -> PreFrames3 -> m ()
displayFrames lid frs = do
  let framesRaw = case frs of
        [] -> []
        [Just ((bfr, ffr), (ovProp, ovMono))] ->
          [Just ((FrameBase $ U.unsafeThaw bfr, ffr), (ovProp, ovMono))]
        _ ->
          -- Due to the frames coming from the same base frame,
          -- we have to copy it to avoid picture corruption.
          map (fmap $ \((bfr, ffr), (ovProp, ovMono)) ->
                ((FrameBase $ U.thaw bfr, ffr), (ovProp, ovMono))) frs
  -- If display level different than the man viewed level,
  -- e.g., when our actor is attacked on a remote level,
  -- then pad with tripple delay to give more time to see the remote frames(s).
  lidV <- viewedLevelUI
  frames <- if lidV == lid
            then do
              modifySession $ \sess -> sess { sdisplayNeeded = False
                                            , sturnDisplayed = True }
              return framesRaw
            else return $ framesRaw ++ [Nothing, Nothing, Nothing]
  mapM_ displayFrame frames

-- | Write 'Frontend.FrontKey' UI request to the frontend, read the reply,
-- set pointer, return key.
connFrontendFrontKey :: MonadClientUI m => [K.KM] -> PreFrame3 -> m K.KM
connFrontendFrontKey frontKeyKeys ((bfr, ffr), (ovProp, ovMono)) = do
  let frontKeyFrame = ((FrameBase $ U.unsafeThaw bfr, ffr), (ovProp, ovMono))
  kmp <- connFrontend $ Frontend.FrontKey frontKeyKeys frontKeyFrame
  modifySession $ \sess -> sess {spointer = K.kmpPointer kmp}
  return $! K.kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ Frontend.FrontAutoYes b

frontendShutdown :: MonadClientUI m => m ()
frontendShutdown = connFrontend Frontend.FrontShutdown

printScreen :: MonadClientUI m => m ()
printScreen = connFrontend Frontend.FrontPrintScreen

-- | Initialize the frontend chosen by the player via client options.
chanFrontend :: MonadClientUI m
             => ScreenContent -> ClientOptions -> m Frontend.ChanFrontend
chanFrontend coscreen soptions =
  liftIO $ Frontend.chanFrontendIO coscreen soptions

anyKeyPressed :: MonadClientUI m => m Bool
anyKeyPressed = connFrontend Frontend.FrontPressed

discardPressedKey :: MonadClientUI m => m ()
discardPressedKey = connFrontend Frontend.FrontDiscardKey

resetPressedKeys :: MonadClientUI m => m ()
resetPressedKeys = connFrontend Frontend.FrontResetKeys

addPressedKey :: MonadClientUI m => K.KMP -> m ()
addPressedKey = connFrontend . Frontend.FrontAdd

addPressedControlEsc :: MonadClientUI m => m ()
addPressedControlEsc = addPressedKey K.KMP { K.kmpKeyMod = K.controlEscKM
                                           , K.kmpPointer = PointUI 0 0 }

revCmdMap :: MonadClientUI m => m (HumanCmd.HumanCmd -> K.KM)
revCmdMap = do
  CCUI{coinput=InputContent{brevMap}} <- getsSession sccui
  let revCmd cmd = case M.lookup cmd brevMap of
        Nothing -> K.undefinedKM
        Just (k : _) -> k
        Just [] -> error $ "" `showFailure` brevMap
  return revCmd

getReportUI :: MonadClientUI m => Bool -> m Report
getReportUI insideMenu = do
  side <- getsClient sside
  saimMode <- getsSession saimMode
  (inhabitants, embeds) <-
    if isJust saimMode then computeChosenLore else return ([], [])
  sUIOptions <- getsSession sUIOptions
  report <- getsSession $ newReport . shistory
  fact <- getsState $ (EM.! side) . sfactionD
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  -- Different from ordinary tutorial hints in that shown more than once.
  let newcomerHelp = fromMaybe curTutorial overrideTut
      detailAtDefault = (detailLevel <$> saimMode) == Just defaultDetailLevel
      detailMinimal = (detailLevel <$> saimMode) == Just minBound
      underAI = isAIFact fact
      prefixColors = uMessageColors sUIOptions
      -- Here we assume newbies don't override default keys.
      miniHintAiming = if null inhabitants && null embeds
                       then miniHintAimingBare
                       else miniHintAimingLore
      promptAim = toMsgShared prefixColors MsgPromptGeneric
                  $ miniHintAiming <> "\n"
      promptAI = toMsgShared prefixColors MsgPromptAction
                             "<press any key for main menu>"
  return $! if | newcomerHelp && not insideMenu
                 && detailAtDefault && not detailMinimal ->
                   consReport promptAim report
               | underAI -> consReport promptAI report
               | otherwise -> report

computeChosenLore :: MonadClientUI m
                  => m ([(ActorId, Actor)], [(ItemId, ItemQuant)])
computeChosenLore = do
  side <- getsClient sside
  mxhairPos <- xhairToPos
  leader0 <- getLeaderUI
  b0 <- getsState $ getActorBody leader0
  let xhairPos = fromMaybe (bpos b0) mxhairPos
  lidV <- viewedLevelUI
  let isOurs (_, b) = bfid b == side
  inhabitants0 <- getsState $ filter (not . isOurs)
                  . posToAidAssocs xhairPos lidV
  embeds0 <- getsState $ EM.assocs . getEmbedBag lidV xhairPos
  return (inhabitants0, embeds0)

miniHintAimingBare :: Text
miniHintAimingBare = "Aiming mode: press 'f' to fling, SPACE or RMB to cycle detail, ESC to cancel."

miniHintAimingLore :: Text
miniHintAimingLore = "Aiming mode: '~' for lore, 'f' to fling, SPACE or RMB to hush, ESC to cancel."

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

xhairToPos :: MonadClientUI m => m (Maybe Point)
xhairToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  sxhair <- getsSession sxhair
  case mleader of
    Nothing -> return Nothing  -- e.g., when game start and no leader yet
    Just aid -> getsState $ aidTgtToPos aid lidV sxhair
                  -- e.g., xhair on another level

setXHairFromGUI :: MonadClientUI m => Maybe Target -> m ()
setXHairFromGUI xhair2 = do
  xhair0 <- getsSession sxhair
  modifySession $ \sess -> sess {sxhairGoTo = Nothing}
  when (xhair0 /= xhair2) $ modifySession $ \sess -> sess {sxhair = xhair2}

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
    setXHairFromGUI sxhair

getFontSetup :: MonadClientUI m => m FontSetup
getFontSetup = do
  soptions@ClientOptions{schosenFontset, sfontsets} <- getsClient soptions
  let chosenFontsetID = fromJust schosenFontset
      chosenFontset = case lookup chosenFontsetID sfontsets of
        Nothing -> error $ "Fontset not defined in config file"
                           `showFailure` chosenFontsetID
        Just fs -> fs
      multiFont = Frontend.frontendName soptions == "sdl"
                  && not (T.null (fontPropRegular chosenFontset))
  return $! if | not multiFont -> singleFontSetup
               | fontPropRegular chosenFontset == fontMono chosenFontset
                 && fontPropBold chosenFontset == fontMono chosenFontset ->
                 monoFontSetup
               | otherwise -> multiFontSetup

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

defaultHistory :: (MonadClient m, MonadClientUI m) => m History
defaultHistory = do
  sUIOptions <- getsSession sUIOptions
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  let displayHints = fromMaybe curTutorial overrideTut
  liftIO $ do
    utcTime <- getCurrentTime
    timezone <- getTimeZone utcTime
    let curDate = T.pack $ take 19 $ show $ utcToLocalTime timezone utcTime
        emptyHist = emptyHistory $ uHistoryMax sUIOptions
        msg = toMsgShared (uMessageColors sUIOptions) MsgBookKeeping
              $ "History log started on " <> curDate <> "."
        -- Tuturial hints from initial message can be repeated.
        (_, nhistory, _) =
          addToReport S.empty displayHints False emptyHist msg timeZero
    return nhistory

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
        cps = intToDouble (timeFit time timeClip) / diff
        fps = intToDouble nframes / diff
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
          cps = intToDouble (timeFit time timeClip) / diff
          fps = intToDouble nframes / diff
      -- This means: "Game portion after last reload time:...".
      clientPrintUI $
        "Game time:" <+> tshow diff <> "s; frames:" <+> tshow nframes <> "."
        <+> "Average clips per second:" <+> tshow cps <> "."
        <+> "Average FPS:" <+> tshow fps <> "."

elapsedSessionTimeGT :: MonadClientUI m => Int -> m Bool
elapsedSessionTimeGT stopAfter = do
  current <- liftIO getPOSIXTime
  sstartPOSIX <- getsSession sstart
  return $! (fromIntegralWrap :: Int -> NominalDiffTime) stopAfter
            + sstartPOSIX
            <= current

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
  modifySession $ \sess ->
    sess { sgstart
        , sallTime = absoluteTimeAdd (sallTime sess) time
        , snframes = 0
        , sallNframes = sallNframes sess + nframes }

-- | The part of speech describing the actor or the "you" pronoun if he is
-- the leader of the observer's faction.
partActorLeader :: MonadClientUI m => ActorId -> m MU.Part
partActorLeader aid = do
  mleader <- getsClient sleader
  bUI <- getsSession $ getActorUI aid
  b <- getsState $ getActorBody aid
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ | bhp b <= 0
        && not (bproj b) ->  -- avoid "the fallen falling" projectiles
      MU.Phrase ["the fallen", partActor bUI]
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
  COps{corule} <- getsState scops
  clientOptions <- getsClient soptions
  if sbenchmark clientOptions then return Nothing
  else do
    side <- getsClient sside
    prefix <- getsClient $ ssavePrefixCli . soptions
    let fileName = prefix <> Save.saveNameCli corule side
    res <- liftIO $ Save.restoreGame corule clientOptions fileName
    let cfgUIName = rcfgUIName corule
        (configString, _) = rcfgUIDefault corule
    dataDir <- liftIO appDataDir
    liftIO $ tryWriteFile (dataDir </> cfgUIName) configString
    return res

-- For a leader, the skills are both current and max skills.
leaderSkillsClientUI :: MonadClientUI m => m Ability.Skills
{-# INLINE leaderSkillsClientUI #-}
leaderSkillsClientUI = do
  leader <- getLeaderUI
  getsState $ getActorMaxSkills leader

-- | Invoke pseudo-random computation with the generator kept in the session.
rndToActionUI :: MonadClientUI m => Rnd a -> m a
rndToActionUI r = do
  gen1 <- getsSession srandomUI
  let (a, gen2) = St.runState r gen1
  modifySession $ \sess -> sess {srandomUI = gen2}
  return a

tryOpenBrowser :: MonadClientUI m => String -> m Bool
tryOpenBrowser address = liftIO $ openBrowser address
