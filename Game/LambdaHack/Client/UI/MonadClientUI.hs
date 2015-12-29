{-# LANGUAGE RankNTypes #-}
-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getSession
                 , getsSession
                 , modifySession
                 , putSession
                 )
    -- * Display and key input
  , ColorMode(..)
  , mapStartY, promptGetKey, promptGetInt
  , getInitConfirms, getConfirms, getConfirmsKey
  , displayFrame, displayDelay, displayActorStart, drawOverlay
    -- * Assorted primitives
  , stopPlayBack, askConfig, askBinding
  , setFrontAutoYes, anyKeyPressed, discardPressedKey, frontendShutdown
  , scoreToSlideshow, msgPromptAI, defaultHistory
  , getLeaderUI, getArenaUI, viewedLevel
  , targetDescLeader, targetDescCursor
  , leaderTgtToPos, leaderTgtAims, cursorToPos, splitOKX
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad (when)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

-- | The row where the dungeon map starts.
mapStartY :: Y
mapStartY = 1

-- | The monad that gives the client access to UI operations.
class MonadClient m => MonadClientUI m where
  getSession  :: m SessionUI
  getsSession  :: (SessionUI -> a) -> m a
  modifySession :: (SessionUI -> SessionUI) -> m ()
  putSession  :: SessionUI -> m ()

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

promptGetKey :: MonadClientUI m => Overlay -> Bool -> [K.KM] -> m K.KM
promptGetKey ov sfBlank frontKeyKeys = do
  keyPressed <- anyKeyPressed
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed
               && (null frontKeyKeys
                   || km{K.pointer=Nothing} `elem` frontKeyKeys) -> do
      frontKeyFrame <- drawOverlay ColorFull sfBlank ov
      displayFrame $ Just frontKeyFrame
      modifySession $ \sess -> sess {slastPlay = kms}
      return km
    _ -> do
      -- We can't continue playback; wipe out old srunning, etc.
      interrupted <- stopPlayBack
      ov2 <- if keyPressed && interrupted then do
               discardPressedKey
               return $! toOverlay ["*interrupted*"] <> ov
             else return ov
      frontKeyFrame <- drawOverlay ColorFull sfBlank ov2
      km <- connFrontend FrontKey{..}
      modifySession $ \sess -> sess {slastKM = km}
      return km
  (seqCurrent, seqPrevious, k) <- getsSession slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifySession $ \sess -> sess {slastRecord}
  return km

promptGetInt :: MonadClientUI m => SingleFrame -> m K.KM
promptGetInt frontKeyFrame = do
  -- We assume this is used inside a menu, so delays and cutoffs
  -- are already taken care of.
  let frontKeyKeys = K.escKM : K.returnKM : K.backspaceKM
                     : map (K.toKM K.NoModifier)
                         (map (K.Char . Char.intToDigit) [0..9])
  connFrontend FrontKey{..}

-- | Display a slideshow, awaiting confirmation for each slide
-- or not awaiting at all if there is only one.
getInitConfirms :: MonadClientUI m
                => ColorMode -> [K.KM] -> [K.KM] -> Slideshow -> m Bool
getInitConfirms dm trueKeys falseKeys slides = do
  case slideshow slides of
    [ov] -> do
      frame <- drawOverlay dm False ov
      connFrontend $ FrontFrame frame
      return True
    _ -> getConfirms dm trueKeys falseKeys slides

-- | Display a slideshow, awaiting confirmation for each slide
-- and returning a boolean.
getConfirms :: MonadClientUI m
            => ColorMode -> [K.KM] -> [K.KM] -> Slideshow -> m Bool
getConfirms dm trueKeys falseKeys slides = do
  km <- getConfirmsKey dm (trueKeys ++ falseKeys) slides
  return $! km `elem` trueKeys

-- | Display a slideshow, awaiting confirmation for each slide
-- and returning a key.
getConfirmsKey :: MonadClientUI m
               => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirmsKey dm extraKeys slides = do
  let ovs = slideshow slides
      keys = [K.spaceKM, K.pgupKM, K.pgdnKM, K.homeKM, K.endKM]
             ++ extraKeys
  frontSlides <- drawOverlays dm False ovs
  let displayFrs frs srf = case frs of
        [] -> assert `failure` slides
        x : xs -> do
          km@K.KM{..} <- getConfirmGeneric keys x
          case key of
            K.Home -> displayFrs frontSlides []
            K.End -> case reverse frontSlides of
              [] -> assert `failure` slides
              y : ys -> displayFrs [y] ys
            K.PgUp -> case srf of
              [] -> displayFrs frs srf
              y : ys -> displayFrs (y : frs) ys
            K.PgDn -> case xs of
              [] -> displayFrs frs srf
              _ -> displayFrs xs (x : srf)
            K.Space -> case xs of
              -- If Space permitted, only exits at the end of slideshow.
              [] | K.spaceKM `elem` extraKeys -> return km
              [] -> displayFrs frs srf
              _ -> displayFrs xs (x : srf)
            _ | km `elem` extraKeys -> return km
            _ -> assert `failure` "unknown key" `twith` km
  displayFrs frontSlides []

getConfirmGeneric :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
getConfirmGeneric clearKeys frontKeyFrame = do
  let extraKeys = [K.spaceKM, K.escKM, K.pgupKM, K.pgdnKM]
      frontKeyKeys = clearKeys ++ extraKeys
  connFrontend FrontKey {..}

displayFrame :: MonadClientUI m => Maybe SingleFrame -> m ()
displayFrame mf = do
  let frame = case mf of
        Nothing -> FrontDelay
        Just fr -> FrontFrame fr
  connFrontend frame

displayDelay :: MonadClientUI m =>  m ()
displayDelay = displayFrame Nothing

-- | Push frames or delays to the frame queue. Additionally set @sdisplayed@.
-- because animations not always happen after @SfxActorStart@ on the leader's
-- level (e.g., death can lead to leader change to another level mid-turn,
-- and there could be melee and animations on that level at the same moment).
-- Insert delays, so that the animations don't look rushed.
displayActorStart :: MonadClientUI m => Actor -> Frames -> m ()
displayActorStart b frs = do
  timeCutOff <- getsSession $ EM.findWithDefault timeZero (blid b) . sdisplayed
  localTime <- getsState $ getLocalTime (blid b)
  let delta = localTime `timeDeltaToFrom` timeCutOff
  when (delta > Delta timeClip && not (bproj b))
    displayDelay
  let ageDisp = EM.insert (blid b) (btime b)
  modifySession $ \sess -> sess {sdisplayed = ageDisp $ sdisplayed sess}
  mapM_ displayFrame frs

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> Overlay -> m SingleFrame
drawOverlay dm sfBlank sfTop = do
  mbaseFrame <- if sfBlank then return Nothing else Just <$> drawBaseFrame dm
  return $! overlayFrame sfTop mbaseFrame
  -- TODO: here sfTop is possibly truncated wrt length

drawBaseFrame :: MonadClientUI m => ColorMode -> m SingleFrame
drawBaseFrame dm = do
  lid <- viewedLevel
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
        -- if cursor invalid, e.g., on a wrong level; @draw@ ignores it later on
      pathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return ("------", Nothing)) targetDescLeader mleader
  cursorDesc <- targetDescCursor
  SessionUI{sselected, stgtMode, smarkVision, smarkSmell, swaitTimes}
    <- getSession
  draw dm lid cursorPos tgtPos bfsmpath cursorDesc tgtDesc
       sselected stgtMode smarkVision smarkSmell swaitTimes

drawOverlays :: MonadClientUI m
             => ColorMode -> Bool -> [Overlay] -> m [SingleFrame]
drawOverlays _ _ [] = return []
drawOverlays dm sfBlank ovs = do
  mbaseFrame <- if sfBlank then return Nothing else Just <$> drawBaseFrame dm
  let f topNext = overlayFrame topNext mbaseFrame
  return $! map f ovs  -- keep lazy for responsiveness

stopPlayBack :: MonadClientUI m => m Bool
stopPlayBack = do
  lastPlay <- getsSession slastPlay
  modifySession $ \sess -> sess
    { slastPlay = []
    , slastRecord = ([], [], 0)
        -- TODO: not ideal, but needed to cancel macros that contain apostrophes
    , swaitTimes = - abs (swaitTimes sess)
    }
  srunning <- getsSession srunning
  case srunning of
    Nothing -> return $! not $ null lastPlay
    Just RunParams{runLeader} -> do
      -- Switch to the original leader, from before the run start,
      -- unless dead or unless the faction never runs with multiple
      -- (but could have the leader changed automatically meanwhile).
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      arena <- getArenaUI
      s <- getState
      when (memActor runLeader arena s && not (noRunWithMulti fact)) $
        modifyClient $ updateLeader runLeader s
      modifySession (\sess -> sess {srunning = Nothing})
      return True

askConfig :: MonadClientUI m => m Config
askConfig = getsSession sconfig

-- | Get the key binding.
askBinding :: MonadClientUI m => m Binding
askBinding = getsSession sbinding

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ FrontAutoYes b

anyKeyPressed :: MonadClientUI m => m Bool
anyKeyPressed = connFrontend FrontPressed

discardPressedKey :: MonadClientUI m => m ()
discardPressedKey = connFrontend FrontDiscard

frontendShutdown :: MonadClientUI m => m ()
frontendShutdown = connFrontend FrontShutdown

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
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
      showScore (ntable, pos) =
        HighScore.highSlideshow ntable pos gameModeName tz
      diff | fhasUI $ gplayer fact = scurDiff
           | otherwise = difficultyInverse scurDiff
      theirVic (fi, fa) | isAtWar fact fi
                          && not (isHorrorFact fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isAllied fact fi || fi == fid = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      (worthMentioning, rScore) =
        HighScore.register table total time status date diff
                           (fname $ gplayer fact)
                           ourVictims theirVictims
                           (fhiCondPoly $ gplayer fact)
  return $! toSlideshow  -- TODO: split dynamically, for changing ysize
         $ if worthMentioning then showScore rScore else mempty

getLeaderUI :: MonadClientUI m => m ActorId
getLeaderUI = do
  cli <- getClient
  case _sleader cli of
    Nothing -> assert `failure` "leader expected but not found" `twith` cli
    Just leader -> return leader

getArenaUI :: MonadClientUI m => m LevelId
getArenaUI = do
  mleader <- getsClient _sleader
  case mleader of
    Just leader -> getsState $ blid . getActorBody leader
    Nothing -> do
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      case gquit fact of
        Just Status{stDepth} -> return $! toEnum stDepth
        Nothing -> getEntryArena fact

viewedLevel :: MonadClientUI m => m LevelId
viewedLevel = do
  arena <- getArenaUI
  stgtMode <- getsSession stgtMode
  return $! maybe arena tgtLevelId stgtMode

targetDesc :: MonadClientUI m => Maybe Target -> m (Text, Maybe Text)
targetDesc target = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case target of
    Just (TEnemy aid _) -> do
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      maxHP <- sumOrganEqpClient IK.EqpSlotAddMaxHP aid
      let percentage = 100 * bhp b `div` xM (max 5 maxHP)
          stars | percentage < 20  = "[____]"
                | percentage < 40  = "[*___]"
                | percentage < 60  = "[**__]"
                | percentage < 80  = "[***_]"
                | otherwise        = "[****]"
          hpIndicator = if bfid b == side then Nothing else Just stars
      return (bname b, hpIndicator)
    Just (TEnemyPos _ lid p _) -> do
      let hotText = if lid == lidV
                    then "hot spot" <+> tshow p
                    else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
      return (hotText, Nothing)
    Just (TPoint lid p) -> do
      pointedText <-
        if lid == lidV
        then do
          bag <- getsState $ getCBag (CFloor lid p)
          case EM.assocs bag of
            [] -> return $! "exact spot" <+> tshow p
            [(iid, kit@(k, _))] -> do
              localTime <- getsState $ getLocalTime lid
              itemToF <- itemToFullClient
              let (_, name, stats) = partItem CGround localTime (itemToF iid kit)
              return $! makePhrase $ if k == 1
                                     then [name, stats]  -- "a sword" too wordy
                                     else [MU.CarWs k name, stats]
            _ -> return $! "many items at" <+> tshow p
        else return $! "an exact spot on level" <+> tshow (abs $ fromEnum lid)
      return (pointedText, Nothing)
    Just TVector{} ->
      case mleader of
        Nothing -> return ("a relative shift", Nothing)
        Just aid -> do
          tgtPos <- aidTgtToPos aid lidV target
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> tshow p
          return (maybe invalidMsg validMsg tgtPos, Nothing)
    Nothing -> return ("crosshair location", Nothing)

targetDescLeader :: MonadClientUI m => ActorId -> m (Text, Maybe Text)
targetDescLeader leader = do
  tgt <- getsClient $ getTarget leader
  targetDesc tgt

targetDescCursor :: MonadClientUI m => m (Text, Maybe Text)
targetDescCursor = do
  scursor <- getsClient scursor
  targetDesc $ Just scursor

leaderTgtToPos :: MonadClientUI m => m (Maybe Point)
leaderTgtToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtToPos aid lidV tgt

leaderTgtAims :: MonadClientUI m => m (Either Text Int)
leaderTgtAims = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return $ Left "no leader to target with"
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtAims aid lidV tgt

cursorToPos :: MonadClientUI m => m (Maybe Point)
cursorToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  scursor <- getsClient scursor
  case mleader of
    Nothing -> return Nothing
    Just aid -> aidTgtToPos aid lidV $ Just scursor

splitOKX :: MonadClientUI m => Y -> Msg -> OKX -> m [OKX]
splitOKX y prompt okx = do
  promptAI <- msgPromptAI
  lid <- getArenaUI
  Level{lxsize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsSession sreport
  let msg = splitReport lxsize (prependMsg promptAI (addMsg sreport prompt))
  return $! splitOverlayOKX y msg okx

msgPromptAI :: MonadClientUI m => m Msg
msgPromptAI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
  return $! if underAI then "[press ESC for Main Menu]" else ""

defaultHistory :: MonadClient m => Int -> m History
defaultHistory configHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = MU.Text $ tshow $ utcToLocalTime timezone utcTime
  let emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $! singletonReport
         $! makeSentence ["Human history log started on", curDate]
