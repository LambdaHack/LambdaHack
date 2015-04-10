{-# LANGUAGE RankNTypes #-}
-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getsSession  -- exposed only to be implemented, not used
                 , liftIO  -- exposed only to be implemented, not used
                 )
  , SessionUI(..)
    -- * Display and key input
  , ColorMode(..)
  , promptGetKey, getKeyOverlayCommand, getInitConfirms
  , displayFrame, displayDelay, displayActorStart, drawOverlay
    -- * Assorted primitives
  , stopPlayBack, askConfig, askBinding
  , syncFrames, setFrontAutoYes, tryTakeMVarSescMVar, scoreToSlideshow
  , getLeaderUI, getArenaUI, viewedLevel
  , targetDescLeader, targetDescCursor
  , leaderTgtToPos, leaderTgtAims, cursorToPos
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU
import System.Time

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.Frontend as Frontend
import Game.LambdaHack.Client.UI.KeyBindings
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

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- This includes a frontend session and keybinding info.
data SessionUI = SessionUI
  { schanF   :: !ChanFrontend       -- ^ connection with the frontend
  , sbinding :: !Binding            -- ^ binding of keys to commands
  , sescMVar :: !(Maybe (MVar ()))
  , sconfig  :: !Config
  }

-- | The monad that gives the client access to UI operations.
class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a
  liftIO       :: IO a -> m a

-- | Read a keystroke received from the frontend.
readConnFrontend :: MonadClientUI m => m K.KM
readConnFrontend = do
  ChanFrontend{responseF} <- getsSession schanF
  liftIO $ atomically $ readTQueue responseF

-- | Write a UI request to the frontend.
writeConnFrontend :: MonadClientUI m => FrontReq -> m ()
writeConnFrontend efr = do
  ChanFrontend{requestF} <- getsSession schanF
  liftIO $ atomically $ writeTQueue requestF efr

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey frontKM frontFr = do
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
  escPressed <- tryTakeMVarSescMVar  -- this also clears the ESC-pressed marker
  lastPlayOld <- getsClient slastPlay
  km <- case lastPlayOld of
    km : kms | not escPressed && (null frontKM || km `elem` frontKM) -> do
      displayFrame $ Just frontFr
      -- Sync frames so that ESC doesn't skip frames.
      syncFrames
      modifyClient $ \cli -> cli {slastPlay = kms}
      return km
    _ -> do
      stopPlayBack  -- we can't continue playback; wipe out old srunning
      writeConnFrontend FrontKey{..}
      km <- readConnFrontend
      modifyClient $ \cli -> cli {slastKM = km}
      return km
  (seqCurrent, seqPrevious, k) <- getsClient slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifyClient $ \cli -> cli {slastRecord}
  return km

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Maybe Bool -> Overlay -> m K.KM
getKeyOverlayCommand onBlank overlay = do
  frame <- drawOverlay (isJust onBlank) ColorFull overlay
  promptGetKey [] frame

-- | Display a slideshow, awaiting confirmation for each slide except the last.
getInitConfirms :: MonadClientUI m
                => ColorMode -> [K.KM] -> Slideshow -> m Bool
getInitConfirms dm frontClear slides = do
  let (onBlank, ovs) = slideshow slides
      frontFromTop = onBlank
  frontSlides <- drawOverlays (isJust onBlank) dm ovs
  case frontSlides of
    [] -> return True
    _ -> do
      writeConnFrontend FrontSlides{..}
      km <- readConnFrontend
      -- Don't clear ESC marker here, because the wait for confirms may
      -- block a ping and the ping would not see the ESC.
      return $! km /= K.escKM

displayFrame :: MonadClientUI m => Maybe SingleFrame -> m ()
displayFrame mf = do
  let frame = case mf of
        Nothing -> FrontDelay
        Just fr -> FrontNormalFrame fr
  writeConnFrontend frame

displayDelay :: MonadClientUI m =>  m ()
displayDelay = sequence_ $ replicate 4 $ writeConnFrontend FrontDelay

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
            => Bool -> ColorMode -> Overlay -> m SingleFrame
drawOverlay sfBlank@True _ sfTop = do
  let sfLevel = []
      sfBottom = []
  return $! SingleFrame {..}
drawOverlay False dm sfTop = do
  lid <- viewedLevel
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return ("------", Nothing)) targetDescLeader mleader
  cursorDesc <- targetDescCursor
  draw dm lid cursorPos tgtPos bfsmpath cursorDesc tgtDesc sfTop

drawOverlays :: MonadClientUI m
             => Bool -> ColorMode -> [Overlay] -> m [SingleFrame]
drawOverlays _ _ [] = return []
drawOverlays sfBlank dm (topFirst : rest) = do
  fistFrame <- drawOverlay sfBlank dm topFirst
  let f topNext = fistFrame {sfTop = topNext}
  return $! fistFrame : map f rest  -- keep @rest@ lazy for responsiveness

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifyClient $ \cli -> cli
    { slastPlay = []
    , slastRecord = let (seqCurrent, seqPrevious, _) = slastRecord cli
                    in (seqCurrent, seqPrevious, 0)
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

-- | Sync frames display with the frontend.
syncFrames :: MonadClientUI m => m ()
syncFrames = do
  -- Hack.
  writeConnFrontend
    FrontSlides{frontClear=[], frontSlides=[], frontFromTop=Nothing}
  km <- readConnFrontend
  let !_A = assert (km == K.spaceKM) ()
  return ()

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = writeConnFrontend $ FrontAutoYes b

tryTakeMVarSescMVar :: MonadClientUI m => m Bool
tryTakeMVarSescMVar = do
  mescMVar <- getsSession sescMVar
  case mescMVar of
    Nothing -> return False
    Just escMVar -> do
      mUnit <- liftIO $ tryTakeMVar escMVar
      return $! isJust mUnit

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
  date <- liftIO getClockTime
  scurDifficulty <- getsClient scurDifficulty
  factionD <- getsState sfactionD
  let table = HighScore.getTable gameModeId scoreDict
      gameModeName = mname gameMode
      showScore (ntable, pos) =
        HighScore.highSlideshow ntable pos gameModeName
      diff | not $ fhasUI $ gplayer fact = difficultyDefault
           | otherwise = scurDifficulty
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
  return $! if worthMentioning then showScore rScore else mempty

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
              let (_, name, stats) = partItem CGround lid localTime (itemToF iid kit)
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
