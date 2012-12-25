{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for player and monster
-- actions. Has no access to the the main action type @Action@ nor
-- to the implementation details of the action monads @MonadActionRO@
-- and @MonadAction@.
module Game.LambdaHack.Action
  ( -- * Action monads
    MonadActionPure(getServer, getsServer, getClient, getsClient)
  , MonadActionRO(putClient, modifyClient)
  , MonadAction(putServer, modifyServer)
    -- * The Perception Reader
  , withPerception, askPerception
    -- * Accessors to the game session Reader
  , askFrontendSession, askCOps, askBinding, askConfigUI
    -- * Actions returning frames
  , tryWithFrame
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * Diary and report
  , getDiary, msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyFrameCommand, getManyConfirms
    -- * Display each frame and confirm
  , displayMore, displayYesNo, displaySlideshowAbort
    -- * Assorted frame operations
  , submitSlideshow, displaySlideshow
  , displayChoiceUI, displayFramePush, drawPrompt
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
                   , getServer, getsServer, getClient, getsClient )
  , MonadActionRO(liftIO, putClient, modifyClient)
  , MonadAction(putServer, modifyServer)
  , Session (..))
import Game.LambdaHack.Action.HighScore (register)
import qualified Game.LambdaHack.Action.Save as Save
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (SingleFrame(..), Frames)
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Draw
import qualified Game.LambdaHack.DungeonState as DungeonState
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert

-- | Get the frontend session.
askFrontendSession :: MonadActionPure m => m FrontendSession
askFrontendSession = getsSession sfs

-- | Get the content operations.
askCOps :: MonadActionPure m => m Kind.COps
askCOps = getsSession scops

-- | Get the key binding.
askBinding :: MonadActionPure m => m Binding
askBinding = getsSession sbinding

-- | Get the config from the config file.
askConfigUI :: MonadActionPure m => m ConfigUI
askConfigUI = getsSession sconfigUI

-- | Get the current diary.
getDiary :: MonadActionPure m => m Diary
getDiary = getClient

-- | Add a message to the current report.
msgAdd :: MonadActionRO m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the history.
historyReset :: MonadActionRO m => History -> m ()
historyReset shistory = modifyClient $ \Diary{sreport} -> Diary{..}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadActionRO m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Update the cached perception for the given computation.
withPerception :: MonadActionPure m => m () -> m ()
withPerception m = do
  cops <- askCOps
  s <- getServer
  let per = dungeonPerception cops s
  local (const per) m

-- | Get the current perception.
askPerception :: MonadActionPure m => m Perception
askPerception = do
  lid <- getsServer slid
  pers <- ask
  return $! fromJust $! lookup lid pers

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
-- draw and pass along a frame with the abort message (even if empty).
tryWithFrame :: MonadActionRO m
             => m a -> WriterT Frames m a -> WriterT Frames m a
tryWithFrame exc h =
  let excMsg msg = do
        msgReset ""
        fr <- drawPrompt ColorFull msg
        tell [Just fr]
        lift exc
  in tryWith excMsg h

-- | Store current report in the history and reset report.
recordHistory :: MonadActionRO m => m ()
recordHistory = do
  Diary{sreport, shistory} <- getDiary
  unless (nullReport sreport) $ do
    ConfigUI{configHistoryMax} <- askConfigUI
    msgReset ""
    historyReset $! takeHistory configHistoryMax $! addReport sreport shistory

-- | Wait for a player command.
getKeyCommand :: MonadActionRO m
              => Maybe Bool -> m K.KM
getKeyCommand doPush = do
  fs <- askFrontendSession
  keyb <- askBinding
  (nc, modifier) <- liftIO $ nextEvent fs doPush
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Display frame and wait for a player command.
getKeyFrameCommand :: MonadActionRO m
                   => SingleFrame -> m K.KM
getKeyFrameCommand frame = do
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

-- | Display a series of frames, awaiting confirmation for each.
getManyConfirms :: MonadActionRO m => [K.KM] -> [SingleFrame] -> m Bool
getManyConfirms _ [] = return True
getManyConfirms clearKeys (x:xs) = do
  b <- getConfirm clearKeys x
  if b
    then getManyConfirms clearKeys xs
    else return False

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
  frame <- drawPrompt dm $ prompt <+> moreMsg
  getConfirm [] frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadActionRO m => Msg -> m Bool
displayYesNo prompt = do
  frame <- drawPrompt ColorBW $ prompt <+> yesnoMsg
  getYesNo frame

-- | Print a msg and several overlays, one per page.
-- All frames require confirmations. Raise @abort@ if the player presses ESC.
displaySlideshowAbort :: MonadActionRO m => Msg -> Slideshow -> m ()
displaySlideshowAbort prompt xs = do
  let f x = drawOverlay ColorFull prompt (x ++ [moreMsg])
  frames <- mapM f xs
  go <- getManyConfirms [] frames
  when (not go) abort

-- | Print a msg and several overlays, one per page.
-- The last frame does not expect a confirmation and so does not show
-- the invitation to press some keys.
displaySlideshow :: MonadActionRO m
                => [K.KM] -> Msg -> Msg -> Slideshow -> WriterT Frames m ()
displaySlideshow _ _      _ []  = return ()
displaySlideshow _ prompt _ [x] = do
  frame <- drawOverlay ColorFull prompt x
  tell [Just frame]
displaySlideshow clearKeys prompt pressKeys (x:xs) = do
  frame <- drawOverlay ColorFull (prompt <+> pressKeys) (x ++ [moreMsg])
  b <- getConfirm clearKeys frame
  if b
    then displaySlideshow clearKeys prompt pressKeys xs
    else return ()

-- | Submit frames for a msg and several overlays, one per page.
-- The last frame does not expect a confirmation and so does not show
-- the invitation to press some keys.
submitSlideshow :: MonadActionRO m
                => Msg -> Msg -> Slideshow -> WriterT Frames m ()
submitSlideshow _      _ []  = return ()
submitSlideshow prompt _ [x] = do
  frame <- drawOverlay ColorFull prompt x
  tell [Just frame]
submitSlideshow prompt pressKeys (x : xs) = do
  frame <- drawOverlay ColorFull (prompt <+> pressKeys) (x ++ [moreMsg])
  tell [Just frame]
  submitSlideshow prompt pressKeys xs

-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadActionRO m
                => Msg -> Slideshow -> [K.KM]
              -> m K.KM
displayChoiceUI prompt ovs keys = do
  let (over, rest, spc, more, keysS) = case ovs of
        [] -> ([], [], "", [], keys)
        [x] -> (x, [], "", [], keys)
        x:xs -> (x, xs, ", SPACE", [moreMsg], (K.Space, K.NoModifier) : keys)
      legalKeys =  (K.Esc, K.NoModifier) : keysS
  frame <- drawOverlay ColorFull (prompt <> spc <> ", ESC]") (over ++ more)
  fs <- askFrontendSession
  (key, modifier) <- liftIO $ promptGetKey fs legalKeys frame
  case key of
    K.Esc -> neverMind True
    K.Space | not (null rest) -> displayChoiceUI prompt rest keys
    _ -> return (key, modifier)

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFramePush :: MonadActionRO m => Maybe SingleFrame -> m ()
displayFramePush mframe = do
  fs <- askFrontendSession
  liftIO $ displayFrame fs False mframe

-- | Draw the current level. The prompt is displayed, but not added
-- to history. The prompt is appended to the current message
-- and only the first screenful of the resulting overlay is displayed.
drawPrompt :: MonadActionPure m => ColorMode -> Msg -> m SingleFrame
drawPrompt dm prompt = do
  cops <- askCOps
  per <- askPerception
  s <- getServer
  Diary{sreport} <- getDiary
  let over = splitReport $ addMsg sreport prompt
  return $! draw dm cops per s over

-- | Draw the current level. The prompt and the overlay are displayed,
-- but not added to history. The prompt is appended to the current message
-- and only the first line of the result is displayed.
-- The overlay starts on the second line.
drawOverlay :: MonadActionPure m => ColorMode -> Msg -> Overlay -> m SingleFrame
drawOverlay dm prompt overlay = do
  cops <- askCOps
  per <- askPerception
  s <- getServer
  Diary{sreport} <- getDiary
  let xsize = lxsize $ slevel s
      msgPrompt = renderReport $ addMsg sreport prompt
      over = padMsg xsize msgPrompt : overlay
  return $! draw dm cops per s over

-- | Initialize perception, etc., display level and run the action.
startClip :: MonadAction m => m () -> m ()
startClip action =
  -- Determine perception before running player command, in case monsters
  -- have opened doors, etc.
  withPerception $! do
    remember  -- heroes notice their surroundings, before they get displayed
    displayPush  -- draw the current surroundings
    action  -- let the actor act

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadActionRO m => m ()
displayPush = do
  fs <- askFrontendSession
  s  <- getServer
  pl <- getsServer splayer
  frame <- drawPrompt ColorFull ""
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  let (_, Actor{bdir}, _) = findActorAnyLevel pl s
      isRunning = isJust bdir
  liftIO $ displayFrame fs isRunning $ Just frame

-- | Update heroes memory.
remember :: MonadAction m => m ()
remember = do
  per <- askPerception
  let vis = IS.toList (totalVisible per)
  rememberList vis

-- | Update heroes memory at the given list of locations.
rememberList :: MonadAction m => [Point] -> m ()
rememberList vis = do
  Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- askCOps
  lvl <- getsServer slevel
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
      unknownId = ouniqGroup "unknown space"
      newClear (loc, tk) = lvl `rememberAt` loc == unknownId
                           && Tile.isExplorable cotile tk
      clearN = length $ filter newClear rememberTile
  modifyServer (updateLevel (updateLRMap (Kind.// rememberTile)))
  modifyServer (updateLevel (\ l@Level{lseen} -> l {lseen = lseen + clearN}))
  let alt Nothing      = Nothing
      alt (Just ([], _)) = Nothing
      alt (Just (t, _))  = Just (t, t)
      rememberItem = IM.alter alt
  modifyServer (updateLevel (updateIMap (\ m -> foldr rememberItem m vis)))

-- | Save the diary and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadActionRO m => m ()
saveGameBkp = do
  state <- getServer
  diary <- getDiary
  configUI <- askConfigUI
  liftIO $ Save.saveGameBkp configUI state diary

-- | Dumps the current game rules configuration to a file.
dumpCfg :: MonadActionRO m => FilePath -> m ()
dumpCfg fn = do
  config <- getsServer sconfig
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
    time <- getsServer stime
    curDate <- liftIO getClockTime
    let score = register configUI write total time curDate status
    (placeMsg, slideshow) <- liftIO score
    displaySlideshowAbort placeMsg slideshow

-- | Continue or restart or exit the game.
endOrLoop :: MonadAction m => m () -> m ()
endOrLoop handleTurn = do
  squit <- getsServer squit
  s <- getServer
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
      Diary{sreport} <- getDiary
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
      Diary{sreport} <- getDiary
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
  cops <- askCOps
  state <- gameResetAction configUI cops
  modifyServer $ const state
  saveGameBkp
  handleTurn

-- TODO: do this inside Action ()
gameReset :: ConfigUI -> Kind.COps -> IO State
gameReset configUI cops@Kind.COps{ coitem=coitem@Kind.Ops{okind}
                                 , corule
                                 , cofact=Kind.Ops{opick}} = do
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
        sfaction <- opick factionName (const True)
        let state = defaultState sflavour
                                 disco discoS discoRev
                                 freshDungeon entryLevel srandom
                                 sconfig sfaction entryLoc
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
                  -> State -> Diary -> IO ())
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
      -- handle the diary and backup savefile.
      handleGame = do
        handleTurn
        diary <- getDiary
        -- Save diary often, at each game exit, in case of crashes.
        liftIO $ Save.rmBkpSaveDiary sconfigUI diary
      loop sfs = start executor sfs scops sbinding sconfigUI handleGame
  startup font loop

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: MonadActionRO m
      => (m () -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
          -> State -> Diary -> IO ())
      -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
      -> m () -> IO ()
start executor sfs scops@Kind.COps{corule} sbinding sconfigUI handleGame = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  restored <- Save.restoreGame sconfigUI pathsDataFile title
  case restored of
    Right (diary, msg) -> do  -- Starting a new game.
      state <- gameReset sconfigUI scops
      executor handleGame sfs scops sbinding sconfigUI
        state
        diary{sreport = singletonReport msg}
        -- TODO: gameReset >> handleTurn or defaultState {squit=Reset}
    Left (state, diary, msg) ->  -- Running a restored game.
      executor handleGame sfs scops sbinding sconfigUI
        state
        -- This overwrites the "Really save/quit?" messages.
        diary{sreport = singletonReport msg}

-- | Debugging.
debug :: MonadActionRO m => Text -> m ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
