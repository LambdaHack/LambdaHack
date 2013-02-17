{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.Action
  ( -- * Action monads
    MonadClient( getClient, getsClient, putClient, modifyClient )
  , MonadClientUI
  , MonadClientChan
  , executorCli, exeFrontend, frontendName
    -- * Abort exception handlers
  , tryWithSlide
    -- * Accessors to the game session Reader and the Perception Reader(-like)
  , askBinding, askPerception
    -- * History and report
  , msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyOverlayCommand, getManyConfirms
    -- * Display and key input
  , displayFramesPush, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow
    -- * Draw frames
  , drawOverlay
    -- * Turn init operations
  , rememberLevel, displayPush
    -- * Assorted primitives
  , flushFrames, clientGameSave, clientDisconnect, restoreGame
  , readChanFromSer, writeChanToSer, rndToAction, getArenaCli
  ) where

import Control.Concurrent
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.Action.ActionType (executorCli)
import Game.LambdaHack.Client.Action.ConfigIO
import Game.LambdaHack.Client.Action.Frontend (frontendName, startup)
import qualified Game.LambdaHack.Client.Action.Frontend as Frontend
import qualified Game.LambdaHack.Client.Action.Save as Save
import Game.LambdaHack.Client.Animation (Frames, SingleFrame)
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert

withUI :: MonadClientUI m => m a -> m a
withUI m = do
  mvarUI <- getsSession smvarUI
  liftIO $ putMVar mvarUI ()
  a <- m
  liftIO $ takeMVar mvarUI
  return a

displayFrame :: MonadClientUI m => Bool -> Maybe SingleFrame -> m ()
displayFrame isRunning mf = do
  fs <- askFrontendSession
  faction <- getsState sfaction
  case filter isHumanFact $ EM.elems faction of
    _ : _ : _ ->
      -- More than one human player; don't mix the output
      modifyClient $ \cli -> cli {sframe = (mf, isRunning) : sframe cli}
    _ ->
      -- At most one human player, display everything at once.
      withUI $ liftIO $ Frontend.displayFrame fs isRunning mf

flushFrames :: MonadClientUI m => m ()
flushFrames = do
  fs <- askFrontendSession
  sframe <- getsClient sframe
  liftIO $ mapM_ (\(mf, b) -> Frontend.displayFrame fs b mf) $ reverse sframe
  modifyClient $ \cli -> cli {sframe = []}

nextEvent :: MonadClientUI m => Maybe Bool -> m K.KM
nextEvent mb = withUI $ do
  fs <- askFrontendSession
  flushFrames
  liftIO $ Frontend.nextEvent fs mb

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey keys frame = withUI $ do
  fs <- askFrontendSession
  flushFrames
  liftIO $ Frontend.promptGetKey fs keys frame

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a slide with the abort message (even if message empty).
tryWithSlide :: MonadClient m
             => m a -> WriterT Slideshow m a -> WriterT Slideshow m a
tryWithSlide exc h =
  let excMsg msg = do
        msgReset ""
        slides <- promptToSlideshow msg
        tell slides
        lift exc
  in tryWith excMsg h

getArenaCli :: MonadClient m => m LevelId
getArenaCli = do
  cli <- getClient
  s <- getState
  return $! getArena cli s

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
askPerception :: MonadClient m => m Perception
askPerception = do
  stgtMode <- getsClient stgtMode
  arena <- getArenaCli
  let lid = maybe arena tgtLevelId stgtMode
  factionPers <- getsClient sper
  return $! fromMaybe (assert `failure` lid) $ EM.lookup lid factionPers

-- | Wait for a human player command.
getKeyCommand :: MonadClientUI m => Maybe Bool -> m K.KM
getKeyCommand doPush = do
  keyb <- askBinding
  (nc, modifier) <- nextEvent doPush
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Overlay -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull overlay
  keyb <- askBinding
  (nc, modifier) <- promptGetKey [] frame
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: MonadClientUI m => [K.KM] -> SingleFrame -> m Bool
getConfirm clearKeys frame = do
  let keys = [(K.Space, K.NoModifier), (K.Esc, K.NoModifier)] ++ clearKeys
  km <- promptGetKey keys frame
  case km of
    (K.Space, K.NoModifier) -> return True
    _ | km `elem` clearKeys -> return True
    _ -> return False

-- | Display a slideshow, awaiting confirmation for each slide.
getManyConfirms :: MonadClientUI m => [K.KM] -> Slideshow -> m Bool
getManyConfirms clearKeys slides =
  case runSlideshow slides of
    [] -> return True
    x : xs -> do
      frame <- drawOverlay ColorFull x
      b <- getConfirm clearKeys frame
      if b
        then getManyConfirms clearKeys (toSlideshow xs)
        else return False

-- | Push frames or frame's worth of delay to the frame queue.
displayFramesPush :: MonadClientUI m => Frames -> m ()
displayFramesPush frames = mapM_ (displayFrame False) frames

-- | A yes-no confirmation.
getYesNo :: MonadClientUI m => SingleFrame -> m Bool
getYesNo frame = do
  let keys = [ (K.Char 'y', K.NoModifier)
             , (K.Char 'n', K.NoModifier)
             , (K.Esc, K.NoModifier)
             ]
  (k, _) <- promptGetKey keys frame
  case k of
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

-- TODO: generalize getManyConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadClientUI m => Msg -> Overlay -> [K.KM] -> m K.KM
displayChoiceUI prompt ov keys = do
  slides <- fmap runSlideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys = (K.Space, K.NoModifier) : (K.Esc, K.NoModifier) : keys
      loop [] = neverMind True
      loop (x : xs) = do
        frame <- drawOverlay ColorFull x
        (key, modifier) <- promptGetKey legalKeys frame
        case key of
          K.Esc -> neverMind True
          K.Space -> loop xs
          _ -> return (key, modifier)
  loop slides

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClient m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt []

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClient m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lid <- getArenaCli
  lysize <- getsLevel lid lysize  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  let msg = splitReport (addMsg sreport prompt)
  return $! splitOverlay lysize msg overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClient m => ColorMode -> Overlay -> m SingleFrame
drawOverlay dm over = do
  cops <- getsState scops
  per <- askPerception
  cli <- getClient
  loc <- getState
  return $! draw dm cops per cli loc over

-- -- | Draw the current level using server data, for debugging.
-- drawOverlayDebug :: MonadActionAbort m
--                  => ColorMode -> Overlay -> m SingleFrame
-- drawOverlayDebug dm over = do
--   cops <- getsState scops
--   per <- askPerception
--   cli <- getClient
--   glo <- getState
--   return $! draw dm cops per cli glo over

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  sls <- promptToSlideshow ""
  let slide = head $ runSlideshow sls
--  DebugModeCli{somniscient} <- getsClient sdebugCli
  frame <- drawOverlay ColorFull slide
--  frame <- if somniscient
--           then drawOverlayDebug ColorFull slide
--           else drawOverlay ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  displayFrame (isJust srunning) $ Just frame

-- | Update faction memory at the given set of positions, given new visibility.
rememberLevel :: Kind.COps -> ActorDict -> ES.EnumSet Point -> Level -> Level
              -> Level
rememberLevel Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}}
              actorD visible nlvl olvl =
  -- TODO: handle invisible actors, but then change also broadcastPosCli, etc.
  let nprio =
        EM.filter (not . null)
        $ EM.map (filter (\aid ->
            bpos (actorD EM.! aid) `ES.member` visible)) (lprio nlvl)
      nvis = EM.filterWithKey (\p _ -> p `ES.member` visible) (lfloor nlvl)
      ovis = EM.filterWithKey (\p _ -> p `ES.notMember` visible) (lfloor olvl)
      nfloor = EM.union nvis ovis
      vis = ES.toList visible
      rememberTile = [(pos, nlvl `at` pos) | pos <- vis]
      unknownId = ouniqGroup "unknown space"
      wasUnknown (pos, _) = olvl `at` pos == unknownId
      isExplorable (_, tk) = Tile.isExplorable cotile tk
      newTile = filter wasUnknown rememberTile
      extraSeen = length $ filter isExplorable newTile
  in olvl { lprio = nprio
          , lfloor = nfloor
          , ltile = ltile olvl Kind.// newTile
          , lseen = lseen olvl + extraSeen
  -- TODO: let factions that spawn see hidden features and open all hidden
  -- doors (they built and hid them). Hide the Hidden feature in ltile.
  -- Wait with all that until the semantics of (repeated) searching
  -- is changed.
  --        , lsecret = ...
          }

saveName :: FactionId -> Bool -> String
saveName side isAI = show (fromEnum side)
                     ++ if isAI then ".ai.sav" else ".human.sav"

clientGameSave :: MonadClient m => Bool -> m ()
clientGameSave toBkp = do
  s <- getState
  cli <- getClient
  configUI <- getsClient sconfigUI
  side <- getsClient sside
  isAI <- getsClient sisAI
  liftIO $ Save.saveGameCli (saveName side isAI) toBkp configUI s cli

clientDisconnect :: MonadClient m => m ()
clientDisconnect = do
  modifyClient $ \cli -> cli {squit = True}
  clientGameSave False

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

readChanFromSer :: MonadClientChan m => m (Either CmdCli CmdUI)
readChanFromSer = do
  toClient <- getsChan toClient
  liftIO $ readChan toClient

writeChanToSer :: MonadClientChan m => [CmdSer] -> m ()
writeChanToSer cmd = do
  toServer <- getsChan toServer
  liftIO $ writeChan toServer cmd

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
exeFrontend :: Kind.COps
            -> (Bool -> SessionUI -> State -> StateClient -> ConnCli -> IO ())
            -> ((FactionId -> ConnCli -> Bool -> IO ()) -> IO ())
            -> IO ()
exeFrontend cops@Kind.COps{corule} exeClient exeServer = do
  -- UI config reloaded at each client start.
  sconfigUI <- mkConfigUI corule
  smvarUI <- newEmptyMVar
  let !sbinding = stdBinding sconfigUI  -- evaluate to check for errors
      font = configFont sconfigUI
  defHist <- defHistory
  let cli = defStateClient defHist sconfigUI
      loc = defStateLocal cops
      executorC sfs fid chanCli isAI =
        let sess | isAI = undefined
                 | otherwise = SessionUI{..}
        in exeClient isAI sess loc (cli fid isAI) chanCli
  startup font $ \sfs -> exeServer (executorC sfs)

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a
