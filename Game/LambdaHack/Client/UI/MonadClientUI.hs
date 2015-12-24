{-# LANGUAGE RankNTypes #-}
-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( putSession
                 , getsSession -- exposed only to be implemented, not used
                 )
  , SessionUI(..)
    -- * Display and key input
  , ColorMode(..)
  , mapStartY, promptGetKey, promptGetInt
  , getKeyOverlayCommand, getInitConfirms, getConfirms, getConfirmsKey
  , displayFrame, displayDelay, displayActorStart, drawOverlay
    -- * Assorted primitives
  , stopPlayBack, askConfig, askBinding
  , setFrontAutoYes, clearPressed, frontendShutdown
  , scoreToSlideshow
  , getLeaderUI, getArenaUI, viewedLevel
  , targetDescLeader, targetDescCursor
  , leaderTgtToPos, leaderTgtAims, cursorToPos
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad (replicateM_, when)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
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

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- This includes a frontend session and keybinding info.
data SessionUI = SessionUI
  { schanF   :: !ChanFrontend       -- ^ connection with the frontend
  , sbinding :: !Binding            -- ^ binding of keys to commands
  , sconfig  :: !Config
  }

-- | The monad that gives the client access to UI operations.
class MonadClient m => MonadClientUI m where
  putSession  :: SessionUI -> m ()
  getsSession  :: (SessionUI -> a) -> m a

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey frontKeyKeys frontKeyFrame = do
  -- Assume we display the arena when we prompt for a key and possibly
  -- insert a delay and reset cutoff.
  arena <- getArenaUI
  localTime <- getsState $ getLocalTime arena
  -- No delay, because this is before the UI actor acts. Ideally the frame
  -- would not be changed either.
  -- However, set sdisplayed so that there's no extra delay after the actor
  -- acts either, because waiting for the key introduces enough delay.
  -- Or this is running, etc., which we want fast.
  let ageDisp = EM.insert arena localTime
  modifyClient $ \cli -> cli {sdisplayed = ageDisp $ sdisplayed cli}
  keyPressed <- clearPressed
    -- this also clears the key-pressed marker
  lastPlayOld <- getsClient slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed
               && (null frontKeyKeys
                   || km{K.pointer=Nothing} `elem` frontKeyKeys) -> do
      displayFrame $ Just frontKeyFrame
      modifyClient $ \cli -> cli {slastPlay = kms}
      return km
    _ -> do
      stopPlayBack  -- we can't continue playback; wipe out old srunning
      km <- connFrontend FrontKey{..}
      modifyClient $ \cli -> cli {slastKM = km}
      return km
  (seqCurrent, seqPrevious, k) <- getsClient slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifyClient $ \cli -> cli {slastRecord}
  return km

promptGetInt :: MonadClientUI m => SingleFrame -> m K.KM
promptGetInt frontKeyFrame = do
  -- We assume this is used inside a menu, so delays and cutoffs
  -- are already taken care of.
  let frontKeyKeys = K.escKM : K.returnKM : K.backspaceKM
                     : map (K.toKM K.NoModifier)
                         (map (K.Char . Char.intToDigit) [0..9])
  connFrontend FrontKey{..}

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Overlay -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull False overlay
  promptGetKey [] frame

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
displayDelay = replicateM_ 4 $ connFrontend FrontDelay

-- | Push frames or delays to the frame queue. Additionally set @sdisplayed@.
-- because animations not always happen after @SfxActorStart@ on the leader's
-- level (e.g., death can lead to leader change to another level mid-turn,
-- and there could be melee and animations on that level at the same moment).
-- Insert delays, so that the animations don't look rushed.
displayActorStart :: MonadClientUI m => Actor -> Frames -> m ()
displayActorStart b frs = do
  timeCutOff <- getsClient $ EM.findWithDefault timeZero (blid b) . sdisplayed
  localTime <- getsState $ getLocalTime (blid b)
  let delta = localTime `timeDeltaToFrom` timeCutOff
  when (delta > Delta timeClip && not (bproj b))
    displayDelay
  let ageDisp = EM.insert (blid b) localTime
  modifyClient $ \cli -> cli {sdisplayed = ageDisp $ sdisplayed cli}
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
  draw dm lid cursorPos tgtPos bfsmpath cursorDesc tgtDesc

drawOverlays :: MonadClientUI m
             => ColorMode -> Bool -> [Overlay] -> m [SingleFrame]
drawOverlays _ _ [] = return []
drawOverlays dm sfBlank ovs = do
  mbaseFrame <- if sfBlank then return Nothing else Just <$> drawBaseFrame dm
  let f topNext = overlayFrame topNext mbaseFrame
  return $! map f ovs  -- keep lazy for responsiveness

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifyClient $ \cli -> cli
    { slastPlay = []
    , slastRecord = ([], [], 0)
        -- TODO: not ideal, but needed to cancel macros that contain apostrophes
    , swaitTimes = - abs (swaitTimes cli)
    }
  srunning <- getsClient srunning
  case srunning of
    Nothing -> return ()
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
      modifyClient (\cli -> cli {srunning = Nothing})

askConfig :: MonadClientUI m => m Config
askConfig = getsSession sconfig

-- | Get the key binding.
askBinding :: MonadClientUI m => m Binding
askBinding = getsSession sbinding

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ FrontAutoYes b

clearPressed :: MonadClientUI m => m Bool
clearPressed = connFrontend FrontPressed

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
  return $! toSlideshow
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
  stgtMode <- getsClient stgtMode
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
