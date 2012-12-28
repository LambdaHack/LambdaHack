{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for player and monster
-- actions. Has no access to the the main action type @Action@ nor
-- to the implementation details of the action monads @MonadActionRO@
-- and @MonadAction@.
module Game.LambdaHack.Action
  ( -- * Action monads
    MonadActionPure(getGlobal, getsGlobal, getClient, getsClient)
  , MonadActionRO(putClient, modifyClient)
  , MonadAction(putGlobal, modifyGlobal)
    -- * The Perception Reader
  , withPerception, askPerception
    -- * Accessors to the game session Reader
  , askBinding, askConfigUI
    -- * Actions returning frames
  , tryWithSlide
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * StateClient and report
  , getClient, msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyOverlayCommand, getManyConfirms
    -- * Display and key input
  , displayFramesPush, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow
    -- * Draw frames
  , drawOverlay
    -- * Clip init operations
  , startClip, remember, rememberList
    -- * Assorted primitives
  , saveGameBkp, dumpCfg, endOrLoop, frontendName, startFrontend
  , debug
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Reader.Class
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, tell, lift)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Time
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import qualified Game.LambdaHack.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.MonadAction
  ( MonadActionPure( tryWith, abortWith, getsSession
                   , getGlobal, getsGlobal, getClient, getsClient )
  , MonadActionRO(liftIO, putClient, modifyClient)
  , MonadAction(putGlobal, modifyGlobal)
  , Session (..))
import Game.LambdaHack.Action.HighScore (register)
import qualified Game.LambdaHack.Action.Save as Save
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (Frames, SingleFrame(..))
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Draw
import qualified Game.LambdaHack.DungeonState as DungeonState
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert

-- | Get the frontend session.
askFrontendSession :: MonadActionPure m => m FrontendSession
askFrontendSession = getsSession sfs

-- | Get the key binding.
askBinding :: MonadActionPure m => m Binding
askBinding = getsSession sbinding

-- | Get the config from the config file.
askConfigUI :: MonadActionPure m => m ConfigUI
askConfigUI = getsSession sconfigUI

-- | Get the current cli.
getClient :: MonadActionPure m => m StateClient
getClient = getClient

-- | Add a message to the current report.
msgAdd :: MonadActionRO m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the history.
historyReset :: MonadActionRO m => History -> m ()
historyReset shistory = modifyClient $ \StateClient{sreport} -> StateClient{..}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadActionRO m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Update the cached perception for the given computation.
withPerception :: MonadActionPure m => m () -> m ()
withPerception m = do
  cops <- getsGlobal scops
  s <- getGlobal
  let per = dungeonPerception cops s
  local (const per) m

-- | Get the current perception.
askPerception :: MonadActionPure m => m Perception
askPerception = do
  lid <- getsGlobal slid
  pers <- ask
  sfaction <- getsGlobal sfaction
  return $! pers IM.! sfaction M.! lid

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadActionPure m => m a
abort = abortWith ""

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadActionPure m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadActionPure m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadActionPure m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadActionPure m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a slide with the abort message (even if message empty).
tryWithSlide :: MonadActionRO m
             => m a -> WriterT Slideshow m a -> WriterT Slideshow m a
tryWithSlide exc h =
  let excMsg msg = do
        msgReset ""
        slides <- promptToSlideshow msg
        tell slides
        lift exc
  in tryWith excMsg h

-- | Store current report in the history and reset report.
recordHistory :: MonadActionRO m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    ConfigUI{configHistoryMax} <- askConfigUI
    msgReset ""
    historyReset $! takeHistory configHistoryMax $! addReport sreport shistory

-- | Wait for a player command.
getKeyCommand :: MonadActionRO m => Maybe Bool -> m K.KM
getKeyCommand doPush = do
  fs <- askFrontendSession
  keyb <- askBinding
  (nc, modifier) <- liftIO $ nextEvent fs doPush
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Display an overlay and wait for a player command.
getKeyOverlayCommand :: MonadActionRO m => Overlay -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull overlay
  fs <- askFrontendSession
  keyb <- askBinding
  (nc, modifier) <- liftIO $ promptGetKey fs [] frame
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: MonadActionRO m => [K.KM] -> SingleFrame -> m Bool
getConfirm clearKeys frame = do
  fs <- askFrontendSession
  let keys = [(K.Space, K.NoModifier), (K.Esc, K.NoModifier)] ++ clearKeys
  km <- liftIO $ promptGetKey fs keys frame
  case km of
    (K.Space, K.NoModifier) -> return True
    _ | km `elem` clearKeys -> return True
    _ -> return False

-- | Display a slideshow, awaiting confirmation for each slide.
getManyConfirms :: MonadActionRO m => [K.KM] -> Slideshow -> m Bool
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
displayFramesPush :: MonadActionRO m => Frames -> m ()
displayFramesPush frames = do
  fs <- askFrontendSession
  liftIO $ mapM_ (displayFrame fs False) frames

-- | A yes-no confirmation.
getYesNo :: MonadActionRO m => SingleFrame -> m Bool
getYesNo frame = do
  fs <- askFrontendSession
  let keys = [ (K.Char 'y', K.NoModifier)
             , (K.Char 'n', K.NoModifier)
             , (K.Esc, K.NoModifier)
             ]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: MonadActionRO m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  sli <- promptToSlideshow $ prompt <+> moreMsg
  frame <- drawOverlay dm $ head $ runSlideshow sli
  getConfirm [] frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadActionRO m => Msg -> m Bool
displayYesNo prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay ColorBW $ head $ runSlideshow sli
  getYesNo frame

-- TODO: generalize getManyConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadActionRO m => Msg -> Overlay -> [K.KM] -> m K.KM
displayChoiceUI prompt ov keys = do
  slides <- fmap runSlideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  fs <- askFrontendSession
  let legalKeys = (K.Space, K.NoModifier) : (K.Esc, K.NoModifier) : keys
      loop [] = neverMind True
      loop (x : xs) = do
        frame <- drawOverlay ColorFull x
        (key, modifier) <- liftIO $ promptGetKey fs legalKeys frame
        case key of
          K.Esc -> neverMind True
          K.Space -> loop xs
          _ -> return (key, modifier)
  loop slides

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadActionPure m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt []

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadActionPure m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lysize <- getsGlobal (lysize . slevel)
  StateClient{sreport} <- getClient
  let msg = splitReport (addMsg sreport prompt)
  return $! splitOverlay lysize msg overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadActionPure m => ColorMode -> Overlay -> m SingleFrame
drawOverlay dm over = do
  cops <- getsGlobal scops
  per <- askPerception
  s <- getGlobal
  return $! draw dm cops per s over

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadActionRO m => m ()
displayPush = do
  fs <- askFrontendSession
  s  <- getGlobal
  pl <- getsGlobal splayer
  sli <- promptToSlideshow ""
  frame <- drawOverlay ColorFull $ head $ runSlideshow sli
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  let (_, Actor{bdir}, _) = findActorAnyLevel pl s
      isRunning = isJust bdir
  liftIO $ displayFrame fs isRunning $ Just frame

-- | Initialize perception, etc., display level and run the action.
startClip :: MonadAction m => m () -> m ()
startClip action =
  -- Determine perception before running player command, in case monsters
  -- have opened doors, etc.
  withPerception $! do
    remember  -- heroes notice their surroundings, before they get displayed
    displayPush  -- draw the current surroundings
    action  -- let the actor act

-- | Update heroes memory.
remember :: MonadAction m => m ()
remember = do
  per <- askPerception
  rememberList $ totalVisible per

-- | Update heroes memory at the given list of locations.
rememberList :: MonadAction m => IS.IntSet -> m ()
rememberList visible = do
  let vis = IS.toList visible
  Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getsGlobal scops
  lvl <- getsGlobal slevel
  clvl <- getsGlobal slevelClient
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
      unknownId = ouniqGroup "unknown space"
      newClear (loc, tk) = clvl `rememberAt` loc == unknownId
                           && Tile.isExplorable cotile tk
      clearN = length $ filter newClear rememberTile
  modifyGlobal (updateLevelClient (updateLRMap (Kind.// rememberTile)))
  modifyGlobal (updateLevelClient
                  (\ l@LevelClient{lcseen} -> l {lcseen = lcseen + clearN}))
  let alt Nothing   _ = Nothing
      alt (Just []) _ = assert `failure` lvl
      alt x         _ = x
      rememberItem p m = IM.alter (alt $ IM.lookup p $ litem lvl) p m
  modifyGlobal (updateLevelClient
                  (updateIRMap (\ m -> foldr rememberItem m vis)))
  let cactor = IM.filter (\m -> bloc m `IS.member` visible) (lactor lvl)
      cinv   = IM.filterWithKey (\p _ -> p `IM.member` cactor) (linv lvl)
  modifyGlobal (updateLevelClient
                  (updateCActor (const cactor)
                  . updateCInv (const cinv)))

-- | Save the cli and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadActionRO m => m ()
saveGameBkp = do
  state <- getGlobal
  cli <- getClient
  configUI <- askConfigUI
  liftIO $ Save.saveGameBkp configUI state cli

-- | Dumps the current game rules configuration to a file.
dumpCfg :: MonadActionRO m => FilePath -> m ()
dumpCfg fn = do
  config <- getsGlobal sconfig
  liftIO $ ConfigIO.dump config fn

-- | Handle current score and display it with the high scores.
-- Aborts if display of the scores was interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
handleScores :: MonadActionRO m => Bool -> Status -> Int -> m ()
handleScores write status total =
  when (total /= 0) $ do
    configUI <- askConfigUI
    time <- getsGlobal stime
    curDate <- liftIO getClockTime
    slides <- liftIO $ register configUI write total time curDate status
    go <- getManyConfirms [] slides
    when (not go) abort

-- | Continue or restart or exit the game.
endOrLoop :: MonadAction m => m () -> m ()
endOrLoop handleTurn = do
  squit <- getsGlobal squit
  s <- getGlobal
  configUI <- askConfigUI
  let (_, total) = calculateTotal s
  -- The first, boolean component of squit determines
  -- if ending screens should be shown, the other argument describes
  -- the cause of the disruption of game flow.
  case squit of
    Nothing -> handleTurn  -- just continue
    Just (_, status@Camping) -> do
      -- Save and display in parallel.
      mv <- liftIO newEmptyMVar
      liftIO $ void $ forkIO (Save.saveGameFile configUI s
                              `finally` putMVar mv ())
      tryIgnore $ do
        handleScores False status total
        void $ displayMore ColorFull "See you soon, stronger and braver!"
      liftIO $ takeMVar mv  -- wait until saved
      -- Do nothing, that is, quit the game loop.
    Just (showScreens, status@Killed{}) -> do
      StateClient{sreport} <- getClient
      unless (nullReport sreport) $ do
        -- Sisplay any leftover report. Suggest it could be the cause of death.
        void $ displayMore ColorBW "Who would have thought?"
        recordHistory  -- prevent repeating the report
      tryWith
        (\ finalMsg ->
          let highScoreMsg = "Let's hope another party can save the day!"
              msg = if T.null finalMsg then highScoreMsg else finalMsg
          in void $ displayMore ColorBW msg
          -- Do nothing, that is, quit the game loop.
        )
        (do
           when showScreens $ handleScores True status total
           go <- displayMore ColorBW "Next time will be different."
           when (not go) $ abortWith "You could really win this time."
           restartGame handleTurn
        )
    Just (showScreens, status@Victor) -> do
      StateClient{sreport} <- getClient
      unless (nullReport sreport) $ do
        -- Sisplay any leftover report. Suggest it could be the master move.
        void $ displayMore ColorFull "Brilliant, wasn't it?"
        recordHistory  -- prevent repeating the report
      when showScreens $ do
        tryIgnore $ handleScores True status total
        void $ displayMore ColorFull "Can it be done better, though?"
      restartGame handleTurn
    Just (_, Restart) -> do
      void $ displayMore ColorBW "This time for real."
      restartGame handleTurn

restartGame :: MonadAction m => m () -> m ()
restartGame handleTurn = do
  -- Take the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  configUI <- askConfigUI
  cops <- getsGlobal scops
  state <- gameResetAction configUI cops
  modifyGlobal $ const state
  saveGameBkp
  handleTurn

-- TODO: do this inside Action ()
gameReset :: ConfigUI -> Kind.COps -> IO State
gameReset configUI cops@Kind.COps{ cofact=Kind.Ops{opick, ofoldrWithKey}
                                 , coitem=coitem@Kind.Ops{okind}
                                 , corule
                                 , costrat=Kind.Ops{opick=sopick}
                                 , cotile} = do
  -- Rules config reloaded at each new game start.
  (sconfig, dungeonGen, srandom) <- ConfigIO.mkConfigRules corule
  let rnd = do
        sflavour <- dungeonFlavourMap coitem
        (discoS, discoRev) <- serverDiscos coitem
        let f ik = isymbol (okind ik)
                   `notElem` (ritemProject $ Kind.stdRuleset corule)
            disco = M.filter f discoS
        DungeonState.FreshDungeon{..} <-
          DungeonState.generate cops sflavour discoRev sconfig
        let factionName = configFaction sconfig
        playerFactionKindId <- opick factionName (const True)
        let g gkind fk mk = do
              (m, k) <- mk
              let gname = Nothing
                  genemy = fenemy fk
                  gally = fally fk
              gAiSelected <-
                if gkind == playerFactionKindId
                then return Nothing
                else fmap Just $ sopick (fAiSelected fk) (const True)
              gAiIdle <- sopick (fAiIdle fk) (const True)
              return (IM.insert k Faction{..} m, k + 1)
        sfactions <- fmap fst $ ofoldrWithKey g (return (IM.empty, 0))
        let sfaction = fst $ fromJust
                       $ find (\(_, fa) -> isNothing (gAiSelected fa))
                       $ IM.toList sfactions
            state = defaultState cotile sflavour
                                 disco discoS discoRev
                                 freshDungeon freshDepth entryLevel srandom
                                 sconfig sfaction sfactions cops entryLoc
        return $ initialHeroes cops entryLoc configUI state
  return $! St.evalState rnd dungeonGen

gameResetAction :: MonadActionRO m
                => ConfigUI -> Kind.COps -> m State
gameResetAction configUI cops = liftIO $ gameReset configUI cops

-- | Wire together content, the definitions of game commands,
-- config and a high-level startup function
-- to form the starting game session. Evaluate to check for errors,
-- in particular verify content consistency.
-- Then create the starting game config from the default config file
-- and initialize the engine with the starting session.
startFrontend :: MonadActionRO m
              => (m () -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
                  -> State -> StateClient -> IO ())
              -> Kind.COps -> m () -> IO ()
startFrontend executor !cops@Kind.COps{corule, cotile=tile} handleTurn = do
  -- Compute and insert auxiliary optimized components into game content,
  -- to be used in time-critical sections of the code.
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
      scops = cops {Kind.cotile}
  -- UI config reloaded at each client start.
  sconfigUI <- ConfigIO.mkConfigUI corule
  let !sbinding = stdBinding sconfigUI
      font = configFont sconfigUI
      -- In addition to handling the turn, if the game ends or exits,
      -- handle the cli and backup savefile.
      handleGame = do
        handleTurn
        cli <- getClient
        -- Save cli often, at each game exit, in case of crashes.
        liftIO $ Save.rmBkpSaveStateClient sconfigUI cli
      loop sfs = start executor sfs scops sbinding sconfigUI handleGame
  startup font loop

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: MonadActionRO m
      => (m () -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
          -> State -> StateClient -> IO ())
      -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
      -> m () -> IO ()
start executor sfs scops@Kind.COps{corule} sbinding sconfigUI handleGame = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  restored <- Save.restoreGame sconfigUI pathsDataFile title
  case restored of
    Right (cli, msg) -> do  -- Starting a new game.
      state <- gameReset sconfigUI scops
      executor handleGame sfs scops sbinding sconfigUI
        state
        cli{sreport = singletonReport msg}
        -- TODO: gameReset >> handleTurn or defaultState {squit=Reset}
    Left (state, cli, msg) ->  -- Running a restored game.
      executor handleGame sfs scops sbinding sconfigUI
        state{scops}  -- overwritten by recreated cops
        -- This overwrites the "Really save/quit?" messages.
        cli{sreport = singletonReport msg}

-- | Debugging.
debug :: MonadActionRO m => Text -> m ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
