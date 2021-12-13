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
  , revCmdMap, getReportUI, getMiniHintAiming, computeChosenLore
  , getArenaUI, viewedLevelUI, mxhairToPos, xhairToPos, setXHairFromGUI
  , clearAimMode, getFontSetup, scoreToSlideshow, defaultHistory
  , tellAllClipPS, tellGameClipPS, elapsedSessionTimeGT
  , resetSessionStart, resetGameStart, partActorLeader, partPronounLeader
  , tryRestore, rndToActionUI, tryOpenBrowser
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connFrontend, displayFrame
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
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
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
import           Game.LambdaHack.Content.FactionKind
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Core.Random

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
-- but not to modifying client state, except for the client-side pointman
-- (as opposed to pointman stores in faction data in main game state),
-- which is more of a UI concept, but is shared with AI to be able
-- to keep it when switching AI on/off and to save on typing.
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
displayFrames _ [] = return ()
displayFrames lid frs = do
  let framesRaw = case frs of
        [] -> []
        [Just ((bfr, ffr), (ovProp, ovSquare, ovMono))] ->
          [Just ( (FrameBase $ U.unsafeThaw bfr, ffr)
                , (ovProp, ovSquare, ovMono) )]
        _ ->
          -- Due to the frames coming from the same base frame,
          -- we have to copy it to avoid picture corruption.
          map (fmap $ \((bfr, ffr), (ovProp, ovSquare, ovMono)) ->
                ((FrameBase $ U.thaw bfr, ffr), (ovProp, ovSquare, ovMono))) frs
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
connFrontendFrontKey frontKeyKeys ((bfr, ffr), (ovProp, ovSquare, ovMono)) = do
  let frontKeyFrame =
        ((FrameBase $ U.unsafeThaw bfr, ffr), (ovProp, ovSquare, ovMono))
  sautoYes <- getsSession sautoYes
  if sautoYes && (null frontKeyKeys || K.spaceKM `elem` frontKeyKeys) then do
    connFrontend $ Frontend.FrontFrame frontKeyFrame
    return K.spaceKM
  else do
    kmp <- connFrontend $ Frontend.FrontKey frontKeyKeys frontKeyFrame
    modifySession $ \sess -> sess {spointer = K.kmpPointer kmp}
    return $! K.kmpKeyMod kmp

setFrontAutoYes :: MonadClientUI m => Bool -> m ()
setFrontAutoYes b = modifySession $ \sess -> sess {sautoYes = b}

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
  saimMode <- getsSession saimMode
  sUIOptions <- getsSession sUIOptions
  report <- getsSession $ newReport . shistory
  sreqDelay <- getsSession sreqDelay
  miniHintAiming <- getMiniHintAiming
  -- Different from ordinary tutorial hints in that shown more than once.
  let detailAtDefault = (detailLevel <$> saimMode) == Just defaultDetailLevel
      detailMinimal = (detailLevel <$> saimMode) == Just minBound
      prefixColors = uMessageColors sUIOptions
      promptAim = toMsgShared prefixColors MsgPromptGeneric
                              (miniHintAiming <> "\n")
      promptDelay = toMsgShared prefixColors MsgPromptAction
                                "<press any key to regain control>"
  return $! if | not insideMenu && detailAtDefault && not detailMinimal ->
                   consReport promptAim report
               | sreqDelay == ReqDelayAlarm && not insideMenu ->
                   consReport promptDelay report
               | otherwise -> report

getMiniHintAiming :: MonadClientUI m => m Text
getMiniHintAiming = do
  saimMode <- getsSession saimMode
  (inhabitants, embeds) <-
    if isJust saimMode then computeChosenLore else return ([], [])
  sreqDelay <- getsSession sreqDelay
  mleader <- getsClient sleader
  let loreCommandAvailable = not (null inhabitants && null embeds)
                             && isJust mleader
  -- Here we assume newbies don't override default keys.
  return $! T.unwords $ concat
    [ ["Aiming mode:"]
    , ["'~' for lore," | loreCommandAvailable ]
    , ["'f' to fling," | sreqDelay /= ReqDelayHandled]
    , [if loreCommandAvailable && sreqDelay /= ReqDelayHandled
       then "SPACE or RMB to hush,"  -- shorter, because less space left
       else "SPACE or RMB to cycle detail,"]
    , ["ESC to cancel."] ]

computeChosenLore :: MonadClientUI m
                  => m ([(ActorId, Actor)], [(ItemId, ItemQuant)])
computeChosenLore = do
  side <- getsClient sside
  xhairPos <- xhairToPos
  lidV <- viewedLevelUI
  let isOurs (_, b) = bfid b == side
  inhabitants0 <- getsState $ filter (not . isOurs)
                              . posToAidAssocs xhairPos lidV
  embeds0 <- getsState $ EM.assocs . getEmbedBag lidV xhairPos
  return (inhabitants0, embeds0)

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

mxhairToPos :: MonadClientUI m => m (Maybe Point)
mxhairToPos = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  sxhair <- getsSession sxhair
  getsState $ aidTgtToPos mleader lidV sxhair

xhairToPos :: MonadClientUI m => m Point
xhairToPos = do
  mxhairPos <- mxhairToPos
  mleader <- getsClient sleader
  fallback <- case mleader of
    Nothing -> return originPoint
    Just leader -> getsState $ bpos . getActorBody leader
  return $! fromMaybe fallback mxhairPos

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
  xhairPos <- xhairToPos  -- computed while still in aiming mode
  modifySession $ \sess -> sess {saimMode = Nothing}
  lidV <- viewedLevelUI  -- not in aiming mode at this point
  when (lidVOld /= lidV) $ do
    sxhairOld <- getsSession sxhair
    let sxhair = case sxhairOld of
          Just TPoint{} -> Just $ TPoint TUnknown lidV xhairPos
            -- the point is possibly unknown on this level; unimportant anyway
          _ -> sxhairOld
    setXHairFromGUI sxhair

-- We can't support setup @FontSetup SquareFont MonoFont MonoFont@
-- at this time, because the mono layer needs to overwrite the prop layer
-- and so has to be distinct even if the underlying font is mono for both.
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
      theirVic (fi, fa) | isFoe fid fact fi
                          && not (isHorrorFact fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isFriend fid fact fi = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      (worthMentioning, (ntable, pos)) =
        HighScore.register table total dungeonTotal time status date curChalSer
                           (T.unwords $ tail $ T.words $ gname fact)
                           ourVictims theirVictims
                           (fhiCondPoly $ gkind fact)
  fontSetup <- getFontSetup
  let sli = highSlideshow fontSetup False rwidth (rheight - 1) ntable pos
                          gameModeName tz
  return $! if worthMentioning
            then sli
            else emptySlideshow

defaultHistory :: MonadClientUI m => m History
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

-- TODO: for speed and resolution use
-- https://hackage.haskell.org/package/chronos
-- or the number_of_nanonseconds functionality
-- in Data.Time.Clock.System, once it arrives there
elapsedSessionTimeGT :: MonadClientRead m => POSIXTime -> Int -> m Bool
elapsedSessionTimeGT sstartPOSIX stopAfter = do
  current <- liftIO getPOSIXTime
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
    liftIO $ Save.restoreGame corule clientOptions fileName

-- | Invoke pseudo-random computation with the generator kept in the session.
rndToActionUI :: MonadClientUI m => Rnd a -> m a
rndToActionUI r = do
  gen1 <- getsSession srandomUI
  let (a, gen2) = St.runState r gen1
  modifySession $ \sess -> sess {srandomUI = gen2}
  return a

tryOpenBrowser :: MonadClientUI m => String -> m Bool
tryOpenBrowser address = liftIO $ openBrowser address
