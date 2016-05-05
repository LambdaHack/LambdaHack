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
  , msgAdd, promptAdd, promptAddAttr, recordHistory
  , mapStartY, getReport, promptGetKey, promptGetInt
  , getInitConfirms, getConfirms, getConfirmsKey
  , displayFrame, displayActorStart, drawBaseFrame, drawOverlay
    -- * Assorted primitives
  , stopPlayBack, askConfig, askBinding
  , setFrontAutoYes, anyKeyPressed, discardPressedKey, addPressedKey
  , frontendShutdown
  , scoreToSlideshow, defaultHistory
  , getLeaderUI, getArenaUI, viewedLevel
  , targetDescLeader, targetDescXhair
  , leaderTgtToPos, leaderTgtAims, xhairToPos, splitOKX
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
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
import Game.LambdaHack.Client.UI.Msg
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

getReport :: MonadClientUI m => m Report
getReport = do
  report <- getsSession _sreport
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      promptAI = toPrompt $ toAttrLine $ "[press any key for Main Menu]"
  return $! if underAI then consReport promptAI report else report

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toMsg msg)}

-- | Add a prompt to the current report.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt $ toAttrLine msg)}

-- | Add a prompt to the current report.
promptAddAttr :: MonadClientUI m => AttrLine -> m ()
promptAddAttr msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt msg)}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  time <- getsState stime
  SessionUI{_sreport, shistory} <- getSession
  unless (nullReport _sreport) $ do
    let nhistory = addReport shistory time _sreport
    modifySession $ \sess -> sess { _sreport = emptyReport
                                  , shistory = nhistory }

-- | Write a UI request to the frontend and read a corresponding reply.
connFrontend :: MonadClientUI m => FrontReq a -> m a
connFrontend req = do
  ChanFrontend f <- getsSession schanF
  liftIO $ f req

-- | Write 'FrontKey' UI request to the frontend, read the reply,
-- set pointer, return key.
connFrontendFrontKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
connFrontendFrontKey frontKeyKeys frontKeyFrame = do
  ChanFrontend f <- getsSession schanF
  kmp <- liftIO $ f FrontKey{..}
  modifySession $ \sess -> sess {spointer = kmpPointer kmp}
  return $! kmpKeyMod kmp

promptGetKey :: MonadClientUI m => Overlay -> Bool -> [K.KM] -> m K.KM
promptGetKey ov sfBlank frontKeyKeys = do
  keyPressed <- anyKeyPressed
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed && km `K.elemOrNull` frontKeyKeys -> do
      frontKeyFrame <- drawOverlay ColorFull sfBlank ov
      displayFrame $ Just frontKeyFrame
      modifySession $ \sess -> sess {slastPlay = kms}
      Config{configRunStopMsgs} <- askConfig
      when configRunStopMsgs $ promptAdd $ "Voicing '" <> tshow km <> "'."
      return km
    _ : _ -> do
      -- We can't continue playback, so wipe out old slastPlay, srunning, etc.
      stopPlayBack
      discardPressedKey
      let ov2 = ov <> if keyPressed then toOverlay ["*interrupted*"] else mempty
      frontKeyFrame <- drawOverlay ColorFull sfBlank ov2
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    [] -> do
      frontKeyFrame <- drawOverlay ColorFull sfBlank ov
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  (seqCurrent, seqPrevious, k) <- getsSession slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifySession $ \sess -> sess { slastRecord
                                , sdisplayNeeded = False }
  return km

promptGetInt :: MonadClientUI m => SingleFrame -> m K.KM
promptGetInt frontKeyFrame = do
  -- We assume this is used inside a menu, so delays and cutoffs
  -- are already taken care of.
  let frontKeyKeys = K.escKM : K.returnKM : K.backspaceKM
                     : map (K.KM K.NoModifier)
                         (map (K.Char . Char.intToDigit) [0..9])
  connFrontendFrontKey frontKeyKeys frontKeyFrame

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
  return $! km `K.elemOrNull` trueKeys

-- | Display a slideshow, awaiting confirmation for each slide
-- and returning a key.
getConfirmsKey :: MonadClientUI m
               => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirmsKey dm extraKeys slides = do
  let ovs = slideshow slides
      keys = [ K.spaceKM, K.pgupKM, K.pgdnKM, K.wheelNorthKM, K.wheelSouthKM
             , K.homeKM, K.endKM ]
             ++ extraKeys
  frontSlides <- drawOverlays dm False ovs
  let displayFrs frs srf = case frs of
        [] -> assert `failure` slides
        x : xs -> do
          km@K.KM{..} <- connFrontendFrontKey keys x
          case key of
            K.Space -> case xs of
              -- Space exits at the end of slideshow and only if in @extraKeys@.
              [] | km `elem` extraKeys -> return km
              [] -> displayFrs frs srf
              _ -> displayFrs xs (x : srf)
            _ | km `K.elemOrNull` extraKeys -> return km
              -- Other keys exit only if in @extraKeys@.
            K.Home -> displayFrs frontSlides []
            K.End -> case reverse frontSlides of
              [] -> assert `failure` slides
              y : ys -> displayFrs [y] ys
            _ | key `elem` [K.PgUp, K.WheelNorth] -> case srf of
              [] -> displayFrs frs srf
              y : ys -> displayFrs (y : frs) ys
            _ | key `elem` [K.PgDn, K.WheelSouth] -> case xs of
              [] -> displayFrs frs srf
              _ -> displayFrs xs (x : srf)
            _ -> assert `failure` "unknown key" `twith` km
  displayFrs frontSlides []

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

drawBaseFrame :: MonadClientUI m => ColorMode -> LevelId -> m SingleFrame
drawBaseFrame dm lid = do
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  xhairPos <- xhairToPos
  let anyPos = fromMaybe originPoint xhairPos
        -- if xhair invalid, e.g., on a wrong level; @draw@ ignores it later on
      pathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return ("------", Nothing)) targetDescLeader mleader
  sitemSel <- getsSession sitemSel
  xhairDesc <- targetDescXhair
  SessionUI{sselected, saimMode, smarkVision, smarkSmell, swaitTimes}
    <- getSession
  draw dm lid xhairPos tgtPos bfsmpath xhairDesc tgtDesc
       sselected saimMode sitemSel smarkVision smarkSmell swaitTimes

drawBaseFrameViewed :: MonadClientUI m => ColorMode -> m SingleFrame
drawBaseFrameViewed dm = do
  lid <- viewedLevel
  drawBaseFrame dm lid

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> Overlay -> m SingleFrame
drawOverlay dm sfBlank sfTop = do
  mbaseFrame <- if sfBlank
                then return Nothing
                else Just <$> drawBaseFrameViewed dm
  return $! overlayFrame sfTop mbaseFrame
  -- TODO: here sfTop is possibly truncated wrt length

drawOverlays :: MonadClientUI m
             => ColorMode -> Bool -> [Overlay] -> m [SingleFrame]
drawOverlays _ _ [] = return []
drawOverlays dm sfBlank ovs = do
  mbaseFrame <- if sfBlank
                then return Nothing
                else Just <$> drawBaseFrameViewed dm
  let f topNext = overlayFrame topNext mbaseFrame
  return $! map f ovs  -- keep lazy for responsiveness

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifySession $ \sess -> sess
    { slastPlay = []
    , slastRecord = ([], [], 0)
        -- Needed to cancel macros that contain apostrophes.
    , swaitTimes = - abs (swaitTimes sess)
    }
  srunning <- getsSession srunning
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
      modifySession (\sess -> sess {srunning = Nothing})

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

addPressedKey :: MonadClientUI m => KMP -> m ()
addPressedKey = connFrontend . FrontAdd

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

splitOKX :: MonadClientUI m => Y -> OKX -> m [OKX]
splitOKX y okx = do
  lid <- getArenaUI
  Level{lxsize} <- getLevel lid  -- TODO: screen length or viewLevel
  report <- getReport
  let msg = splitReport lxsize report
  return $! splitOverlayOKX y msg okx

defaultHistory :: MonadClient m => Int -> m History
defaultHistory configHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = MU.Text $ tshow $ utcToLocalTime timezone utcTime
  let emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $ singletonReport
         $ toMsg $ makeSentence ["Human history log started on", curDate]
