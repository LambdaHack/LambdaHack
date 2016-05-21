{-# LANGUAGE RankNTypes #-}
-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getSession, getsSession, modifySession, putSession
                 , liftIO  -- exposed only to be implemented, not used,
                 )
    -- * Display and key input
  , mapStartY, getReport, displayFrame, displayActorStart
    -- * Assorted primitives
  , setFrontAutoYes, anyKeyPressed, discardPressedKey, addPressedKey
  , connFrontendFrontKey, frontendShutdown, chanFrontend
  , scoreToSlideshow, defaultHistory
  , getLeaderUI, getArenaUI, viewedLevel
  , targetDescLeader, targetDescXhair
  , leaderTgtToPos, leaderTgtAims, xhairToPos, splitOKX
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
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
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
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
  liftIO        :: IO a -> m a

getReport :: MonadClientUI m => m Report
getReport = do
  report <- getsSession _sreport
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      promptAI = toPrompt $ toAttrLine $ "[press any key for Main Menu]"
  return $! if underAI then consReport promptAI report else report

displayFrame :: MonadClientUI m => Maybe SingleFrame -> m ()
displayFrame mf = do
  let frame = case mf of
        Nothing -> FrontDelay 1
        Just fr -> FrontFrame fr
  connFrontend frame

-- | Push frames or delays to the frame queue.
displayActorStart :: MonadClientUI m => Actor -> Frames -> m ()
displayActorStart b frs = do
  mapM_ displayFrame frs
  -- Can be different than @blid b@, e.g., when our actor is attacked
  -- on a remote level.
  arena <- getArenaUI
  when (arena == blid b) $
    modifySession $ \sess -> sess {sdisplayNeeded = False}

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

-- | Write 'FrontKey' UI request to the frontend, read the reply,
-- set pointer, return key.
connFrontendFrontKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
connFrontendFrontKey frontKeyKeys frontKeyFrame = do
  kmp <- connFrontend $ FrontKey{..}
  modifySession $ \sess -> sess {spointer = kmpPointer kmp}
  return $! kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = connFrontend $ FrontAutoYes b

anyKeyPressed :: MonadClientUI m => m Bool
anyKeyPressed = connFrontend FrontPressed

discardPressedKey :: MonadClientUI m => m ()
discardPressedKey = connFrontend FrontDiscard

addPressedKey :: MonadClientUI m => KMP -> m ()
addPressedKey = connFrontend . FrontAdd

frontendShutdown :: MonadClientUI m => m ()
frontendShutdown = connFrontend FrontShutdown

chanFrontend :: MonadClientUI m => DebugModeCli -> m ChanFrontend
chanFrontend = liftIO . Frontend.chanFrontendIO




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
  return $! if worthMentioning
            then textsToSlideshow $ showScore rScore
            else mempty

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
  saimMode <- getsSession saimMode
  return $! maybe arena aimLevelId saimMode

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

targetDescXhair :: MonadClientUI m => m (Text, Maybe Text)
targetDescXhair = do
  sxhair <- getsClient sxhair
  targetDesc $ Just sxhair

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
    Nothing -> return $ Left "no leader to aim with"
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtAims aid lidV tgt

xhairToPos :: MonadClientUI m => m (Maybe Point)
xhairToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  sxhair <- getsClient sxhair
  case mleader of
    Nothing -> return Nothing
    Just aid -> aidTgtToPos aid lidV $ Just sxhair

splitOKX :: MonadClientUI m => Y -> OKX -> m Slideshow
splitOKX y okx = do
  lid <- getArenaUI
  Level{lxsize} <- getLevel lid  -- TODO: screen length or viewLevel
  report <- getReport
  return $! splitOverlay lxsize y report okx

defaultHistory :: MonadClientUI m => Int -> m History
defaultHistory configHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = MU.Text $ tshow $ utcToLocalTime timezone utcTime
  let emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $ singletonReport $ toMsg $ toAttrLine
         $ makeSentence ["Human history log started on", curDate]
