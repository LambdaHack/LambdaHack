{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.Action
  ( -- * Action monads
    MonadClient( getClient, getsClient, putClient, modifyClient )
  , MonadClientUI
  , MonadClientConn
  , MonadClientAbort( abortWith, tryWith )
  , SessionUI(..)
    -- * Various ways to abort action
  , abort, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryRepeatedlyWith, tryIgnore, tryWithSlide
    -- * Executing actions
  , startup, mkConfigUI
    -- * Accessors to the game session Reader and the Perception Reader(-like)
  , askBinding, getPerFid
    -- * History and report
  , msgAdd, msgReset, recordHistory
    -- * Key input
  , getKeyCommand, getKeyOverlayCommand, getAllConfirms, getInitConfirms
    -- * Display and key input
  , displayFramesPush, displayMore, displayYesNo, displayChoiceUI
  , displayFadeFrames, lockUI, unlockUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow
    -- * Draw frames
  , drawOverlay, animate
    -- * Assorted primitives
  , flushFrames, clientGameSave, restoreGame, displayPush, scoreToSlideshow
  , readConnToClient, writeConnFromClient
  , rndToAction, getArenaUI, getLeaderUI
  , targetToPos, frontendName, fadeD, partAidLeader, partActorLeader
  , debugPrint
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NLP.Miniutter.English as MU
import System.IO (hFlush, stderr)
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.Action.ConfigIO
import Game.LambdaHack.Client.Action.Frontend (frontendName, startup)
import qualified Game.LambdaHack.Client.Action.Frontend as Frontend
import qualified Game.LambdaHack.Client.Action.Save as Save
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Utils.Assert

debugPrint :: MonadClient m => Text -> m ()
debugPrint t = do
  debug <- getsClient sdebugCli
  when debug $ liftIO $ do
    delay <- R.randomRIO (0, 1000000)
    threadDelay delay
    T.hPutStrLn stderr t
    hFlush stderr

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadClientAbort m => m a
abort = abortWith ""

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadClientAbort m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadClientAbort m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadClientAbort m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadClientAbort m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a slide with the abort message (even if message empty).
tryWithSlide :: (MonadClientAbort m, MonadClientUI m)
             => m a -> WriterT Slideshow m a -> WriterT Slideshow m a
tryWithSlide exc h =
  let excMsg msg = do
        msgReset ""
        slides <- promptToSlideshow msg
        tell slides
        lift exc
  in tryWith excMsg h

lockUI :: MonadClientUI m => m ()
lockUI = do
  nH <- nHumans
  when (nH > 1) $ do
    mvarUI <- getsSession smvarUI
    liftIO $ putMVar mvarUI ()

unlockUI :: MonadClientUI m => m ()
unlockUI = do
  nH <- nHumans
  when (nH > 1) $ do
    mvarUI <- getsSession smvarUI
    liftIO $ void $ tryTakeMVar mvarUI

displayFrame :: MonadClientUI m => Bool -> Maybe SingleFrame -> m ()
displayFrame isRunning mf = do
  fs <- askFrontendSession
  nH <- nHumans
  if nH > 1 then do
    -- More than one human players, don't mix their output.
    let frame = case mf of
          Nothing -> AcDelay
          Just fr | isRunning -> AcRunning fr
          Just fr -> AcNormal fr
    modifyClient $ \cli -> cli {sframe = frame : sframe cli}
  else do
    -- At most one human player, display everything at once.
    liftIO $ Frontend.displayFrame fs isRunning mf

displayFadeFrames :: MonadClient m => Frames -> m ()
displayFadeFrames frames = do
  let translateFr frame = case frame of
        Nothing -> AcDelay
        Just fr -> AcNormal fr
      fades = reverse $ map translateFr frames
  modifyClient $ \cli -> cli {sfade = fades ++ sfade cli}

flushFrames :: MonadClientUI m => m ()
flushFrames = do
  fs <- askFrontendSession
  let pGetKey keys frame = liftIO $ Frontend.promptGetKey fs keys frame
      displayAc (AcConfirm fr) =
        void $ getConfirmGeneric pGetKey [] fr
      displayAc (AcRunning fr) =
        liftIO $ Frontend.displayFrame fs True (Just fr)
      displayAc (AcNormal fr) =
        liftIO $ Frontend.displayFrame fs False (Just fr)
      displayAc AcDelay =
        liftIO $ Frontend.displayFrame fs False Nothing
  sfade <- getsClient sfade
  sframe <- getsClient sframe
  mapM_ displayAc $ reverse sfade
  mapM_ displayAc $ reverse sframe
  modifyClient $ \cli -> cli {sfade = [], sframe = []}

nextEvent :: MonadClientUI m => Maybe Bool -> m K.KM
nextEvent mb = do
  fs <- askFrontendSession
  flushFrames
  liftIO $ Frontend.nextEvent fs mb

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey keys frame = do
  fs <- askFrontendSession
  flushFrames
  liftIO $ Frontend.promptGetKey fs keys frame

getLeaderUI :: MonadClientUI m => m ActorId
getLeaderUI = do
  cli <- getClient
  case _sleader cli of
    Nothing -> assert `failure` cli
    Just leader -> return leader

getArenaUI :: MonadClientUI m => m LevelId
getArenaUI = do
  leader <- getLeaderUI
  getsState $ blid . getActorBody leader

-- | Calculate the position of leader's target.
targetToPos :: MonadClientUI m => m (Maybe Point)
targetToPos = do
  scursor <- getsClient scursor
  leader <- getLeaderUI
  lid <- getsState $ blid . getActorBody leader
  target <- getsClient $ getTarget leader
  case target of
    Just (TPos pos) -> return $ Just pos
    Just (TEnemy a _ll) -> do
      mem <- getsState $ memActor a lid  -- alive and visible?
      if mem then do
        pos <- getsState $ bpos . getActorBody a
        return $ Just pos
      else return Nothing
    Nothing -> return scursor

-- | Get the frontend session.
askFrontendSession :: MonadClientUI m => m Frontend.FrontendSession
askFrontendSession = getsSession sfs

-- | Get the key binding.
askBinding :: MonadClientUI m => m Binding
askBinding = getsSession sbinding

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadClient m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Store current report in the history and reset report.
recordHistory :: MonadClient m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    ConfigUI{configHistoryMax} <- getsClient sconfigUI
    msgReset ""
    let nhistory = takeHistory configHistoryMax $! addReport sreport shistory
    modifyClient $ \cli -> cli {shistory = nhistory}

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  return $! fromMaybe (assert `failure` lid) $ EM.lookup lid fper

-- | Wait for a human player command.
getKeyCommand :: MonadClientUI m => Maybe Bool -> m K.KM
getKeyCommand doPush = do
  keyb <- askBinding
  km <- nextEvent doPush
  return $! fromMaybe km $ M.lookup km $ kmacro keyb

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Overlay -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull overlay
  keyb <- askBinding
  km <- promptGetKey [] frame
  return $! fromMaybe km $ M.lookup km $ kmacro keyb

-- | Ignore unexpected kestrokes until a SPACE or ESC or others are pressed.
getConfirmGeneric :: MonadClientUI m
                  => ([K.KM] -> SingleFrame -> m K.KM)
                  -> [K.KM] -> SingleFrame -> m Bool
getConfirmGeneric pGetKey clearKeys frame = do
  let keys = [ K.KM {key=K.Space, modifier=K.NoModifier}
             , K.KM {key=K.Esc, modifier=K.NoModifier}]
             ++ clearKeys
  km <- pGetKey keys frame
  case km of
    K.KM {key=K.Space, modifier=K.NoModifier} -> return True
    _ | km `elem` clearKeys -> return True
    _ -> return False

getConfirm :: MonadClientUI m => [K.KM] -> SingleFrame -> m Bool
getConfirm = getConfirmGeneric promptGetKey

-- | Display a slideshow, awaiting confirmation for each slide.
getAllConfirms :: MonadClientUI m => [K.KM] -> Slideshow -> m Bool
getAllConfirms clearKeys slides =
  case runSlideshow slides of
    [] -> return True
    x : xs -> do
      frame <- drawOverlay ColorFull x
      b <- getConfirm clearKeys frame
      if b
        then getAllConfirms clearKeys (toSlideshow xs)
        else return False

-- | Display a slideshow, awaiting confirmation for each slide except the last.
getInitConfirms :: MonadClientUI m => [K.KM] -> Slideshow -> m Bool
getInitConfirms clearKeys slides =
  case runSlideshow slides of
    [] -> return True
    [x] -> do
      frame <- drawOverlay ColorFull x
      displayFrame False $ Just frame
      return True
    x : xs -> do
      frame <- drawOverlay ColorFull x
      b <- getConfirm clearKeys frame
      if b
        then getInitConfirms clearKeys (toSlideshow xs)
        else return False

-- | Push frames or frame's worth of delay to the frame queue.
displayFramesPush :: MonadClientUI m => Frames -> m ()
displayFramesPush frames = mapM_ (displayFrame False) frames

-- | A yes-no confirmation.
getYesNo :: MonadClientUI m => SingleFrame -> m Bool
getYesNo frame = do
  let keys = [ K.KM {key=K.Char 'y', modifier=K.NoModifier}
             , K.KM {key=K.Char 'n', modifier=K.NoModifier}
             , K.KM {key=K.Esc, modifier=K.NoModifier}
             ]
  K.KM {key} <- promptGetKey keys frame
  case key of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  sli <- promptToSlideshow $ prompt <+> moreMsg
  frame <- drawOverlay dm $ head $ runSlideshow sli
  getConfirm [] frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => Msg -> m Bool
displayYesNo prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay ColorBW $ head $ runSlideshow sli
  getYesNo frame

-- TODO: generalize getAllConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: (MonadClientAbort m, MonadClientUI m)
                => Msg -> Overlay -> [K.KM] -> m K.KM
displayChoiceUI prompt ov keys = do
  slides <- fmap runSlideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys =
        [ K.KM {key=K.Space, modifier=K.NoModifier}
        , K.KM {key=K.Esc, modifier=K.NoModifier} ]
        ++ keys
      loop [] = neverMind True
      loop (x : xs) = do
        frame <- drawOverlay ColorFull x
        km@K.KM {..} <- promptGetKey legalKeys frame
        case key of
          K.Esc -> neverMind True
          K.Space -> loop xs
          _ -> return km
  loop slides

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientUI m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt []

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lid <- getArenaUI
  lysize <- getsLevel lid lysize  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  let msg = splitReport (addMsg sreport prompt)
  return $! splitOverlay lysize msg overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m => ColorMode -> Overlay -> m SingleFrame
drawOverlay dm over = do
  cops <- getsState scops
  stgtMode <- getsClient stgtMode
  arena <- getArenaUI
  let lid = maybe arena tgtLevelId stgtMode
  leader <- getLeaderUI
  s <- getState
  cli <- getClient
  per <- getPerFid lid
  return $! draw dm cops per lid leader cli s over

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  sls <- promptToSlideshow ""
  let slide = head $ runSlideshow sls
  frame <- drawOverlay ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  displayFrame (isJust srunning) $ Just frame

scoreToSlideshow :: MonadClientUI m => Status -> m Slideshow
scoreToSlideshow status = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  total <- getsState $ snd . calculateTotal (bfid b) (blid b)
  if total == 0 then return Monoid.mempty
  else do
    table <- getsState shigh
    time <- getsState stime
    date <- liftIO $ getClockTime
    let (ntable, pos) = HighScore.register table total time status date
    return $! HighScore.slideshow ntable pos status

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

clientGameSave :: MonadClient m => Bool -> m ()
clientGameSave toBkp = do
  s <- getState
  cli <- getClient
  configUI <- getsClient sconfigUI
  side <- getsClient sside
  isAI <- getsClient sisAI
  liftIO $ Save.saveGameCli (saveName side isAI) toBkp configUI s cli

restoreGame :: MonadClient m => m (Either (State, StateClient, Msg) Msg)
restoreGame = do
  Kind.COps{corule} <- getsState scops
  configUI <- getsClient sconfigUI
  let pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
      title = rtitle $ Kind.stdRuleset corule
  side <- getsClient sside
  isAI <- getsClient sisAI
  let sName = saveName side isAI
  liftIO $ Save.restoreGameCli sName configUI pathsDataFile title

readConnToClient :: (MonadClientConn c m) => m c
readConnToClient = do
  toClient <- getsConn toClient
  liftIO $ atomically $ readTQueue toClient

writeConnFromClient :: (MonadClientConn c m) => CmdSer -> m ()
writeConnFromClient cmds = do
  toServer <- getsConn toServer
  liftIO $ atomically $ writeTQueue toServer cmds

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a

-- TODO: perhaps draw viewed level, not arena
-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  cops <- getsState scops
  sreport <- getsClient sreport
  leader <- getLeaderUI
  Level{lxsize, lysize} <- getsLevel arena id
  cli <- getClient
  s <- getState
  per <- getPerFid arena
  let over = renderReport sreport
      topLineOnly = truncateMsg lxsize over
      basicFrame = draw ColorFull cops per arena leader cli s [topLineOnly]
  return $ renderAnim lxsize lysize basicFrame anim

fadeD :: MonadClientUI m => Bool -> Bool -> m ()
fadeD out topRight = do
  srunning <- getsClient srunning
  case srunning of
    Just (_, k) | k > 1 -> return ()
    _ -> do
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      arena <- getArenaUI
      lvl <- getsLevel arena id
      report <- getsClient sreport
      unless out $ msgReset $ gname fact <> ", get ready!"
      animMap <- rndToAction $ fadeout out topRight (lxsize lvl) (lysize lvl)
      animFrs <- animate arena animMap
      modifyClient $ \d -> d {sreport = report}
      let frs | out = animFrs
                -- Empty frame to mark the fade-in end,
                -- to trim only to here if SPACE pressed.
              | otherwise = animFrs ++ [Nothing]
      displayFadeFrames frs

-- | The part of speech describing the actor or a special name if a leader
-- of the observer's faction. The actor may not be present in the dungeon.
partActorLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partActorLeader aid b = do
  Kind.COps{coactor} <- getsState scops
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor coactor b

-- | The part of speech describing the actor (designated by actor id
-- and present in the dungeon) or a special name if a leader
-- of the observer's faction.
partAidLeader :: MonadClient m => ActorId -> m MU.Part
partAidLeader aid = do
  b <- getsState $ getActorBody aid
  partActorLeader aid b
