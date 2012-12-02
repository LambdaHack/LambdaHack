{-# LANGUAGE OverloadedStrings #-}
-- | Game action monad and basic building blocks for player and monster
-- actions. Uses @liftIO@ of the @Action@ monad, but does not export it.
-- Has no direct access to the Action monad implementation.
module Game.LambdaHack.Action
  ( -- * Actions and accessors
    Action, getPerception, getCOps, getBinding
    -- * Actions returning frames
  , ActionFrame, returnNoFrame, returnFrame, whenFrame, inFrame, tryWithFrame
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * Diary and report
  , getDiary, msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyFrameCommand, getOverConfirm
    -- * Display each frame and confirm
  , displayMore, displayYesNo, displayOverAbort
    -- * Assorted frame operations
  , displayOverlays, displayChoiceUI, displayFramePush, drawPrompt
    -- * Clip init operations
  , startClip, remember, rememberList
    -- * Assorted primitives
  , saveGameBkp, dumpCfg, endOrLoop, frontendName, startFrontend
  , debug
  ) where

import Control.Monad
import Control.Monad.State hiding (State, state, liftIO)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import System.Time
import Data.Maybe
import Control.Concurrent
import Control.Exception (finally)
import Data.Text (Text)
import qualified Data.Text as T
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Game.LambdaHack.Action.ActionLift
import Game.LambdaHack.Perception
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Draw
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Action.Save as Save
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Binding
import Game.LambdaHack.Action.HighScore (register)
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Animation (SingleFrame(..))
import Game.LambdaHack.Point
import qualified Game.LambdaHack.DungeonState as DungeonState
import Game.LambdaHack.Item
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Tile as Tile

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a frame with the abort message, if any.
tryWithFrame :: Action a -> ActionFrame a -> ActionFrame a
tryWithFrame exc h =
  let msgToFrames ""  = returnNoFrame ()
      msgToFrames msg = do
        msgReset ""
        fr <- drawPrompt ColorFull msg
        returnFrame fr
      excMsg msg = do
        ((), frames) <- msgToFrames msg
        a <- exc
        return (a, frames)
  in tryWith excMsg h

-- | Store current report in the history and reset report.
recordHistory :: Action ()
recordHistory = do
  Diary{sreport, shistory} <- getDiary
  unless (nullReport sreport) $ do
    Config{configHistoryMax} <- gets sconfig
    msgReset ""
    historyReset $ takeHistory configHistoryMax $ addReport sreport shistory

-- | Wait for a player command.
getKeyCommand :: Maybe Bool -> Action (K.Key, K.Modifier)
getKeyCommand doPush = do
  fs <- getFrontendSession
  keyb <- getBinding
  (nc, modifier) <- liftIO $ nextEvent fs doPush
  return $ case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Display frame and wait for a player command.
getKeyFrameCommand :: SingleFrame -> Action (K.Key, K.Modifier)
getKeyFrameCommand frame = do
  fs <- getFrontendSession
  keyb <- getBinding
  (nc, modifier) <- liftIO $ promptGetKey fs [] frame
  return $ case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: SingleFrame -> Action Bool
getConfirm frame = do
  fs <- getFrontendSession
  let keys = [ (K.Space, K.NoModifier), (K.Esc, K.NoModifier)]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Space -> return True
    _       -> return False

-- | A series of confirmations for all overlays.
getOverConfirm :: [SingleFrame] -> Action Bool
getOverConfirm []     = return True
getOverConfirm (x:xs) = do
  b <- getConfirm x
  if b
    then getOverConfirm xs
    else return False

-- | A yes-no confirmation.
getYesNo :: SingleFrame -> Action Bool
getYesNo frame = do
  fs <- getFrontendSession
  let keys = [ (K.Char 'y', K.NoModifier)
             , (K.Char 'n', K.NoModifier)
             , (K.Esc, K.NoModifier)
             ]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Char 'y' -> return True
    _          -> return False

-- TODO: perhaps add prompt to Report instead?
promptAdd :: Msg -> Msg -> Msg
promptAdd prompt msg = prompt <+> msg

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: ColorMode -> Msg -> Action Bool
displayMore dm prompt = do
  let newPrompt = promptAdd prompt moreMsg
  frame <- drawPrompt dm newPrompt
  getConfirm frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: Msg -> Action Bool
displayYesNo prompt = do
  frame <- drawPrompt ColorBW (promptAdd prompt yesnoMsg)
  getYesNo frame

-- | Print a msg and several overlays, one per page.
-- All frames require confirmations. Raise @abort@ if the player presses ESC.
displayOverAbort :: Msg -> [Overlay] -> Action ()
displayOverAbort prompt xs = do
  let newPrompt = promptAdd prompt ""
  let f x = drawOverlay ColorFull newPrompt (x ++ [moreMsg])
  frames <- mapM f xs
  go <- getOverConfirm frames
  when (not go) abort

-- | Print a msg and several overlays, one per page.
-- The last frame does not expect a confirmation and so does not show
-- the invitation to press some keys.
displayOverlays :: Msg -> Msg -> [Overlay] -> ActionFrame ()
displayOverlays _      _ []  = returnNoFrame ()
displayOverlays prompt _ [x] = do
  frame <- drawOverlay ColorFull prompt x
  returnFrame frame
displayOverlays prompt pressKeys (x:xs) = do
  frame <- drawOverlay ColorFull (promptAdd prompt pressKeys) (x ++ [moreMsg])
  b <- getConfirm frame
  if b
    then displayOverlays prompt pressKeys xs
    else returnNoFrame ()

-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: Msg -> [Overlay] -> [(K.Key, K.Modifier)]
              -> Action (K.Key, K.Modifier)
displayChoiceUI prompt ovs keys = do
  let (over, rest, spc, more, keysS) = case ovs of
        [] -> ([], [], "", [], keys)
        [x] -> (x, [], "", [], keys)
        x:xs -> (x, xs, ", SPACE", [moreMsg], (K.Space, K.NoModifier) : keys)
      legalKeys =  (K.Esc, K.NoModifier) : keysS
  frame <- drawOverlay ColorFull (prompt <> spc <> ", ESC]") (over ++ more)
  fs <- getFrontendSession
  (key, modifier) <- liftIO $ promptGetKey fs legalKeys frame
  case key of
    K.Esc -> neverMind True
    K.Space | not (null rest) -> displayChoiceUI prompt rest keys
    _ -> return (key, modifier)

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFramePush :: Maybe SingleFrame -> Action ()
displayFramePush mframe = do
  fs <- getFrontendSession
  liftIO $ displayFrame fs False mframe

-- | Draw the current level. The prompt is displayed, but not added
-- to history. The prompt is appended to the current message
-- and only the first screenful of the resulting overlay is displayed.
drawPrompt :: ColorMode -> Msg -> Action SingleFrame
drawPrompt dm prompt = do
  cops <- getCOps
  per <- getPerception
  s <- get
  Diary{sreport} <- getDiary
  let over = splitReport $ addMsg sreport prompt
  return $ draw dm cops per s over

-- | Draw the current level. The prompt and the overlay are displayed,
-- but not added to history. The prompt is appended to the current message
-- and only the first line of the result is displayed.
-- The overlay starts on the second line.
drawOverlay :: ColorMode -> Msg -> Overlay -> Action SingleFrame
drawOverlay dm prompt overlay = do
  cops <- getCOps
  per <- getPerception
  s <- get
  Diary{sreport} <- getDiary
  let xsize = lxsize $ slevel s
      msgPrompt = renderReport $ addMsg sreport prompt
      over = padMsg xsize msgPrompt : overlay
  return $ draw dm cops per s over

-- | Initialize perception, etc., display level and run the action.
startClip :: Action () -> Action ()
startClip action =
  -- Determine perception before running player command, in case monsters
  -- have opened doors, etc.
  withPerception $ do
    remember  -- heroes notice their surroundings, before they get displayed
    displayPush  -- draw the current surroundings
    action  -- let the actor act

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: Action ()
displayPush = do
  fs <- getFrontendSession
  s  <- get
  pl <- gets splayer
  frame <- drawPrompt ColorFull ""
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  let (_, Actor{bdir}, _) = findActorAnyLevel pl s
      isRunning = isJust bdir
  liftIO $ displayFrame fs isRunning $ Just frame

-- | Update heroes memory.
remember :: Action ()
remember = do
  per <- getPerception
  let vis = IS.toList (totalVisible per)
  rememberList vis

-- | Update heroes memory at the given list of locations.
rememberList :: [Point] -> Action ()
rememberList vis = do
  Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getCOps
  lvl <- gets slevel
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
      unknownId = ouniqGroup "unknown space"
      newClear (loc, tk) = lvl `rememberAt` loc == unknownId
                           && Tile.isExplorable cotile tk
      clearN = length $ filter newClear rememberTile
  modify (updateLevel (updateLRMap (Kind.// rememberTile)))
  modify (updateLevel (\ l@Level{lseen} -> l {lseen = lseen + clearN}))
  let alt Nothing      = Nothing
      alt (Just ([], _)) = Nothing
      alt (Just (t, _))  = Just (t, t)
      rememberItem = IM.alter alt
  modify (updateLevel (updateIMap (\ m -> foldr rememberItem m vis)))

-- | Save the diary and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: Action ()
saveGameBkp = do
  state <- get
  diary <- getDiary
  liftIO $ Save.saveGameBkp state diary

-- | Dumps the current configuration to a file.
--
-- See 'Config.dump'.
dumpCfg :: FilePath -> Action ()
dumpCfg fn = do
  cp <- getOrigCP  -- TODO
  liftIO $ ConfigIO.dump fn cp

-- | Handle current score and display it with the high scores.
-- Aborts if display of the scores was interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
handleScores :: Bool -> Status -> Int -> Action ()
handleScores write status total =
  when (total /= 0) $ do
    config  <- gets sconfig
    time    <- gets stime
    curDate <- liftIO getClockTime
    let score = register config write total time curDate status
    (placeMsg, slideshow) <- liftIO score
    displayOverAbort placeMsg slideshow

-- | Continue or restart or exit the game.
endOrLoop :: Action () -> Action ()
endOrLoop handleTurn = do
  squit <- gets squit
  Kind.COps{coitem} <- getCOps
  s <- get
  let (_, total) = calculateTotal coitem s
  -- The first, boolean component of squit determines
  -- if ending screens should be shown, the other argument describes
  -- the cause of the disruption of game flow.
  case squit of
    Nothing -> handleTurn  -- just continue
    Just (_, status@Camping) -> do
      -- Save and display in parallel.
      mv <- liftIO newEmptyMVar
      liftIO $ void $ forkIO (Save.saveGameFile s `finally` putMVar mv ())
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

restartGame :: Action () -> Action ()
restartGame handleTurn = do
  -- Take the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  cp <- getOrigCP
  cops <- getCOps
  state <- gameResetAction cp cops
  modify $ const state
  saveGameBkp
  handleTurn

-- TODO: do this inside Action ()
gameReset :: ConfigIO.CP -> Kind.COps -> IO State
gameReset cp1 cops@Kind.COps{ coitem
                                , cofact=Kind.Ops{opick}} = do
  (g2, cp2) <- ConfigIO.getSetGen cp1 "dungeonRandomGenerator"
  (g3, cp3) <- ConfigIO.getSetGen cp2 "startingRandomGenerator"
  dataDir <- ConfigIO.appDataDir
  let config3 = ConfigIO.parseConfig dataDir cp3
      (DungeonState.FreshDungeon{..}, ag) =
        runState (DungeonState.generate cops config3) g2
      (sflavour, ag2) = runState (dungeonFlavourMap coitem) ag
      factionName = configFaction config3
      sfaction = evalState (opick factionName (const True)) ag2
  let state =
        defaultState
          config3 sfaction sflavour freshDungeon entryLevel entryLoc g3
      hstate = initialHeroes cops entryLoc state
  return hstate
gameResetAction :: ConfigIO.CP -> Kind.COps -> Action State
gameResetAction cp cops = liftIO $ gameReset cp cops

-- | Wire together content, the definitions of game commands,
-- config and a high-level startup function
-- to form the starting game session. Evaluate to check for errors,
-- in particular verify content consistency.
-- Then create the starting game config from the default config file
-- and initialize the engine with the starting session.
startFrontend :: Kind.COps -> (Config -> Binding (ActionFrame ()))
              -> Action () -> IO ()
startFrontend !scops@Kind.COps{corule} stdBinding handleTurn = do
  let cpDefault = rconfigDefault $ Kind.stdRuleset corule
  cp <- ConfigIO.mkConfig cpDefault
  dataDir <- ConfigIO.appDataDir
  let sconfig = ConfigIO.parseConfig dataDir cp
      !sbinding = stdBinding sconfig
      !sorigConfig = cp
      -- The only option taken not from config in savegame,
      -- but from current config file, possibly from user directory.
      font = configFont sconfig
      -- In addition to handling the turn, if the game ends or exits,
      -- handle the diary and backup savefile.
      handleGame = do
        handleTurn
        diary <- getDiary
        -- Save diary often, at each game exit, in case of crashes.
        liftIO $ Save.rmBkpSaveDiary sconfig diary
      loop sfs = start cp sconfig Session{..} handleGame
  startup font loop

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code.
speedupCops :: Session -> Session
speedupCops sess@Session{scops = cops@Kind.COps{cotile=tile}} =
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
      scops = cops {Kind.cotile}
  in sess {scops}

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: ConfigIO.CP -> Config -> Session -> Action () -> IO ()
start cp config slowSess handleGame = do
  let sess@Session{scops = cops@Kind.COps{ corule }} = speedupCops slowSess
      title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  restored <- Save.restoreGame pathsDataFile config title
  case restored of
    Right (diary, msg) -> do  -- Starting a new game.
      state <- gameReset cp cops
      handlerToIO sess state
        diary{sreport = singletonReport msg}
        -- TODO: gameReset >> handleTurn or defaultState {squit=Reset}
        handleGame
    Left (state, diary, msg) ->  -- Running a restored a game.
      handlerToIO sess state
        -- This overwrites the "Really save/quit?" messages.
        diary{sreport = singletonReport msg}
        handleGame

-- | Debugging.
debug :: Text -> Action ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
