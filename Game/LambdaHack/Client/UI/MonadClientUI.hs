{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MonadClientUI
  ( -- * Client UI monad
    MonadClientUI( getsSession  -- exposed only to be implemented, not used
                 , liftIO  -- exposed only to be implemented, not used
                 )
  , SessionUI(..), connFrontend
    -- * History and report
  , msgAdd, msgReset, recordHistory
    -- * Key input
  , getKeyOverlayCommand, getInitConfirms, stopPlayBack, stopRunning
    -- * Display and key input
  , displayFrames, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow, overlayToBlankSlideshow
    -- * Draw frames
  , drawOverlay, animate
    -- * Assorted primitives
  , displayPush, scoreToSlideshow
  , getArenaUI, getLeaderUI, targetDescLeader, viewedLevel
  , tryTakeMVarSescMVar
  , leaderTgtToPos, leaderTgtAims, cursorToPos, askBinding, syncFrames
  , debugPrint
  , SlideOrCmd, failWith, failSlides, failSer, lookAt, itemOverlay, getConfig
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified NLP.Miniutter.English as MU
import System.Time

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Frontend as Frontend

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- Auxiliary AI and computer player clients have no @sfs@ nor @sbinding@.
data SessionUI = SessionUI
  { sfconn   :: !ConnFrontend  -- ^ connection with the frontend
  , sbinding :: !Binding       -- ^ binding of keys to commands
  , sescMVar :: !(Maybe (MVar ()))
  , sconfig  :: !Config
  }

-- | Connection method between a client and a frontend.
data ConnFrontend = ConnFrontend
  { readConnFrontend  :: MonadClientUI m => m K.KM
      -- ^ read a keystroke received from the frontend
  , writeConnFrontend :: MonadClientUI m => Frontend.FrontReq -> m ()
      -- ^ write a UI request to the frontend
  }

class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a
  liftIO       :: IO a -> m a

connFrontend :: FactionId -> Frontend.ChanFrontend -> ConnFrontend
connFrontend fid fromF = ConnFrontend
  { readConnFrontend =
      liftIO $ atomically $ readTQueue fromF
  , writeConnFrontend = \efr -> do
      let toF = Frontend.toMulti Frontend.connMulti
      liftIO $ atomically $ writeTQueue toF (fid, efr)
  }

getConfig :: MonadClientUI m => m Config
getConfig = getsSession sconfig

displayFrame :: MonadClientUI m => Bool -> Maybe SingleFrame -> m ()
displayFrame isRunning mf = do
  ConnFrontend{writeConnFrontend} <- getsSession sfconn
  let frame = case mf of
        Nothing -> AcDelay
        Just fr | isRunning -> AcRunning fr
        Just fr -> AcNormal fr
  writeConnFrontend $ Frontend.FrontFrame frame

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey frontKM frontFr = do
  escPressed <- tryTakeMVarSescMVar  -- this also clears the ESC marker
  lastPlayOld <- getsClient slastPlay
  km <- case lastPlayOld of
    km : kms | not escPressed && (null frontKM || km `elem` frontKM) -> do
      displayFrame False $ Just frontFr
      modifyClient $ \cli -> cli {slastPlay = kms}
      return km
    _ -> do
      unless (null lastPlayOld) stopPlayBack  -- we can't continue playback
      ConnFrontend{..} <- getsSession sfconn
      writeConnFrontend Frontend.FrontKey {..}
      readConnFrontend
  (seqCurrent, seqPrevious, k) <- getsClient slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifyClient $ \cli -> cli {slastRecord}
  return km

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifyClient $ \cli -> cli
    { slastPlay = []
    , slastRecord = let (seqCurrent, seqPrevious, _) = slastRecord cli
                    in (seqCurrent, seqPrevious, 0)
    , swaitTimes = - swaitTimes cli
    }
  stopRunning

stopRunning :: MonadClientUI m => m ()
stopRunning = do
  srunning <- getsClient srunning
  case srunning of
    Nothing -> return ()
    Just RunParams{runLeader} -> do
      -- Switch to the original leader, from before the run start, unless dead.
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      arena <- getArenaUI
      s <- getState
      when (memActor runLeader arena s && not (isSpawnFact fact)) $
        modifyClient $ updateLeader runLeader s
      modifyClient (\cli -> cli { srunning = Nothing })

-- | Display a slideshow, awaiting confirmation for each slide except the last.
getInitConfirms :: MonadClientUI m
                => ColorMode -> [K.KM] -> Slideshow -> m Bool
getInitConfirms dm frontClear slides = do
  ConnFrontend{..} <- getsSession sfconn
  let (onBlank, ovs) = slideshow slides
  frontSlides <- mapM (drawOverlay onBlank dm) ovs
  -- The first two cases are optimizations:
  case frontSlides of
    [] -> return True
    [x] -> do
      displayFrame False $ Just x
      return True
    _ -> do
      writeConnFrontend Frontend.FrontSlides{..}
      km <- readConnFrontend
      -- Don't clear ESC marker here, because the wait for confirms may
      -- block a ping and the ping would not see the ESC.
      return $! km /= K.escKey

-- | Sync frames display with the frontend.
syncFrames :: MonadClientUI m => m ()
syncFrames = do
  ConnFrontend{..} <- getsSession sfconn
  -- Hack.
  writeConnFrontend Frontend.FrontSlides{frontClear=[], frontSlides=[]}
  km <- readConnFrontend
  assert (km == K.spaceKey) skip

-- | Get the key binding.
askBinding :: MonadClientUI m => m Binding
askBinding = getsSession sbinding

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadClientUI m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    Config{configHistoryMax} <- getConfig
    msgReset ""
    let nhistory = takeHistory configHistoryMax $! addReport sreport shistory
    modifyClient $ \cli -> cli {shistory = nhistory}

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Bool -> Overlay -> m K.KM
getKeyOverlayCommand onBlank overlay = do
  frame <- drawOverlay onBlank ColorFull overlay
  -- Give the previous client time to display his frames.
  liftIO $ threadDelay 1000
  promptGetKey [] frame

-- | Push frames or delays to the frame queue.
displayFrames :: MonadClientUI m => Frames -> m ()
displayFrames = mapM_ (displayFrame False)

-- | A yes-no confirmation.
getYesNo :: MonadClientUI m => SingleFrame -> m Bool
getYesNo frame = do
  let keys = [ K.KM {key=K.Char 'y', modifier=K.NoModifier}
             , K.KM {key=K.Char 'n', modifier=K.NoModifier}
             , K.escKey
             ]
  K.KM {key} <- promptGetKey keys frame
  case key of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  slides <- promptToSlideshow $ prompt <+> moreMsg
  -- Two frames drawn total (unless 'prompt' very long).
  getInitConfirms dm [] $ slides <> toSlideshow False [[]]

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayYesNo dm prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay False dm $ head . snd $ slideshow sli
  getYesNo frame

-- TODO: generalize getInitConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadClientUI m
                => Msg -> Overlay -> [K.KM] -> m (Either Slideshow K.KM)
displayChoiceUI prompt ov keys = do
  (_, ovs) <- fmap slideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys = [K.spaceKey, K.escKey]
        ++ keys
      loop [] = fmap Left $ promptToSlideshow "never mind"
      loop (x : xs) = do
        frame <- drawOverlay False ColorFull x
        km@K.KM {..} <- promptGetKey legalKeys frame
        case key of
          K.Esc -> fmap Left $ promptToSlideshow "never mind"
          K.Space -> loop xs
          _ -> return $ Right km
  loop ovs

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientUI m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt emptyOverlay

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  let msg = splitReport lxsize (addMsg sreport prompt)
  return $! splitOverlay False (lysize + 1) msg overlay

overlayToBlankSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToBlankSlideshow prompt overlay = do
  lid <- getArenaUI
  Level{lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  return $! splitOverlay True (lysize + 3) (toOverlay [prompt]) overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m => Bool -> ColorMode -> Overlay -> m SingleFrame
drawOverlay onBlank dm over = do
  cops <- getsState scops
  lid <- viewedLevel
  mleader <- getsClient _sleader
  s <- getState
  cli <- getClient
  per <- getPerFid lid
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = fmap Just $ getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "------") targetDescLeader mleader
  cursorDesc <- targetDescCursor
  return $! draw onBlank dm cops per lid mleader cursorPos tgtPos
                 bfsmpath cli s cursorDesc tgtDesc over

-- TODO: if more slides, don't take head, but do as in getInitConfirms,
-- but then we have to clear the messages or they get redisplayed
-- each time screen is refreshed.
-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  sls <- promptToSlideshow ""
  let slide = head . snd $ slideshow sls
  frame <- drawOverlay False ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  displayFrame (isJust srunning) $ Just frame

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
  cops <- getsState scops
  fid <- getsClient sside
  fact <- getsState $ (EM.! fid) . sfactionD
  -- TODO: Re-read the table in case it's changed by a concurrent game.
  -- TODO: we should do this, and make sure we do that after server
  -- saved the updated score table, and not register, but read from it.
  -- Otherwise the score is not accurate, e.g., the number of victims.
  table <- getsState shigh
  time <- getsState stime
  date <- liftIO getClockTime
  scurDifficulty <- getsClient scurDifficulty
  factionD <- getsState sfactionD
  dungeon <- getsState sdungeon
  let showScore (ntable, pos) = HighScore.highSlideshow ntable pos
      diff | not $ playerUI $ gplayer fact = difficultyDefault
           | otherwise = scurDifficulty
      theirVic (fi, fa) | isAtWar fact fi
                          && not (isHorrorFact cops fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isAllied fact fi || fi == fid = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      fightsAgainstSpawners =
        let escape = any lescape $ EM.elems dungeon
            isSpawner = isSpawnFact fact
        in escape && not isSpawner
      (worthMentioning, rScore) =
        HighScore.register table total time status date diff
                           (playerName $ gplayer fact)
                           ourVictims theirVictims fightsAgainstSpawners
  return $! if worthMentioning then showScore rScore else mempty

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  cops <- getsState scops
  sreport <- getsClient sreport
  mleader <- getsClient _sleader
  Level{lxsize, lysize} <- getLevel arena
  cli <- getClient
  s <- getState
  per <- getPerFid arena
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = fmap Just $ getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "------") targetDescLeader mleader
  cursorDesc <- targetDescCursor
  let over = renderReport sreport
      topLineOnly = truncateToOverlay lxsize over
      basicFrame =
        draw False ColorFull cops per arena mleader
             cursorPos tgtPos bfsmpath cli s cursorDesc tgtDesc topLineOnly
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else renderAnim lxsize lysize basicFrame anim

targetDesc :: MonadClientUI m => Maybe Target -> m Text
targetDesc target = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case target of
    Just (TEnemy a _) ->
      getsState $ bname . getActorBody a
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV
                then "hot spot" <+> (T.pack . show) p
                else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
    Just (TPoint lid p) ->
      return $! if lid == lidV
                then "exact spot" <+> (T.pack . show) p
                else "an exact spot on level" <+> tshow (abs $ fromEnum lid)
    Just TVector{} ->
      case mleader of
        Nothing -> return "a relative shift"
        Just aid -> do
          tgtPos <- aidTgtToPos aid lidV target
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> (T.pack . show) p
          return $! maybe invalidMsg validMsg tgtPos
    Nothing -> return "cursor location"

targetDescLeader :: MonadClientUI m => ActorId -> m Text
targetDescLeader leader = do
  tgt <- getsClient $ getTarget leader
  targetDesc tgt

targetDescCursor :: MonadClientUI m => m Text
targetDescCursor = do
  scursor <- getsClient scursor
  targetDesc $ Just scursor

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
        Nothing -> do
          dungeon <- getsState sdungeon
          let (minD, maxD) =
                case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
                  (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
                  _ -> assert `failure` "empty dungeon" `twith` dungeon
          return $! max minD $ min maxD $ playerEntry $ gplayer fact

viewedLevel :: MonadClientUI m => m LevelId
viewedLevel = do
  arena <- getArenaUI
  stgtMode <- getsClient stgtMode
  return $! maybe arena tgtLevelId stgtMode

leaderTgtToPos :: MonadClientUI m => m (Maybe Point)
leaderTgtToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtToPos aid lidV tgt

leaderTgtAims :: MonadClientUI m => m (Maybe Text)
leaderTgtAims = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return $ Just "no leader to target with"
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

tryTakeMVarSescMVar :: MonadClientUI m => m Bool
tryTakeMVarSescMVar = do
  mescMVar <- getsSession sescMVar
  case mescMVar of
    Nothing -> return False
    Just escMVar -> do
      mUnit <- liftIO $ tryTakeMVar escMVar
      return $ isJust mUnit

type SlideOrCmd a = Either Slideshow a

failWith :: MonadClientUI m => Msg -> m (SlideOrCmd a)
failWith msg = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  assert (not $ T.null msg) $ fmap Left $ promptToSlideshow msg

failSlides :: MonadClientUI m => Slideshow -> m (SlideOrCmd a)
failSlides slides = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  return $ Left slides

failSer :: MonadClientUI m => ReqFailure -> m (SlideOrCmd a)
failSer = failWith . showReqFailure

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClientUI m
       => Bool       -- ^ detailed?
       -> Text       -- ^ how to start tile description
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> ActorId    -- ^ the actor that looks
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed tilePrefix canSee pos aid msg = do
  Kind.COps{coitem, cotile=cotile@Kind.Ops{okind}} <- getsState scops
  lidV <- viewedLevel
  lvl <- getLevel lidV
  subject <- partAidLeader aid
  s <- getState
  let is = lvl `atI` pos
      verb = MU.Text $ if canSee then "notice" else "remember"
  disco <- getsClient sdisco
  let nWs (iid, k) = partItemWs coitem disco k (getItemBody iid s)
      isd = case detailed of
              _ | EM.size is == 0 -> ""
              _ | EM.size is <= 2 ->
                makeSentence [ MU.SubjectVerbSg subject verb
                             , MU.WWandW $ map nWs $ EM.assocs is]
              True -> "Items:"
              _ -> "Items here."
      tile = lvl `at` pos
      obscured | tile /= hideTile cotile lvl pos = "partially obscured"
               | otherwise = ""
      tileText = obscured <+> tname (okind tile)
      tilePart | T.null tilePrefix = MU.Text tileText
               | otherwise = MU.AW $ MU.Text tileText
      tileDesc = [MU.Text tilePrefix, tilePart]
  if not (null (Tile.causeEffects cotile tile)) then
    return $! makeSentence ("activable:" : tileDesc)
              <+> msg <+> isd
  else if detailed then
    return $! makeSentence tileDesc
              <+> msg <+> isd
  else return $! msg <+> isd

-- | Create a list of item names.
itemOverlay :: MonadClient m => ItemBag -> ItemSlots -> m Overlay
itemOverlay bag sl = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let pr (l, iid) =
         makePhrase [ slotLabel l
                    , partItemWs coitem disco (bag EM.! iid)
                                 (getItemBody iid s) ]
         <> " "
  return $! toOverlay $ map pr $ EM.assocs sl
