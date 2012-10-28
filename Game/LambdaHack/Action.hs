-- | Game action monad and basic building blocks
-- for player and monster actions.
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Game.LambdaHack.Action
  ( -- * Actions and basic operations
    ActionFun, Action, handlerToIO, rndToAction
    -- * Actions returning frames
  , ActionFrame, returnNoFrame, returnFrame, whenFrame, inFrame
    -- * Game session and its accessors
  , Session(..), getCOps, getBinding, getOrigConfig
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryWithFrame, tryRepeatedlyWith, tryIgnore, tryIgnoreFrame
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
    -- * Assorted operations
  , getPerception, updateAnyActor, updatePlayerBody
    -- * Assorted primitives
  , currentDate, saveGameBkp, dumpCfg, endOrLoop, rmBkpSaveDiary
  , gameReset, frontendName, startFrontend
  , debug
  ) where

import Control.Monad
import Control.Monad.State hiding (State, state, liftIO)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.List as L
import System.Time
import Data.Maybe
import Control.Concurrent
import Control.Exception (finally)
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Perception
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Draw
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Save as Save
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.HighScore as H
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Point
import Game.LambdaHack.Time
import qualified Game.LambdaHack.DungeonState as DungeonState
import Game.LambdaHack.Item

-- | The type of the function inside any action.
-- (Separated from the @Action@ type to document each argument with haddock.)
type ActionFun r a =
   Session                           -- ^ session setup data
   -> DungeonPerception              -- ^ cached perception
   -> (State -> Diary -> a -> IO r)  -- ^ continuation
   -> (Msg -> IO r)                  -- ^ failure/reset continuation
   -> State                          -- ^ current state
   -> Diary                          -- ^ current diary
   -> IO r

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action
  { runAction :: forall r . ActionFun r a
  }

instance Show (Action a) where
  show _ = "an action"

-- TODO: check if it's strict enough, if we don't keep old states for too long,
-- Perhaps make state type fields strict for that, too?
instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

instance Functor Action where
  fmap f (Action g) = Action (\ s p k a st ms ->
                               let k' st' ms' = k st' ms' . f
                               in g s p k' a st ms)

instance MonadState State Action where
  get     = Action (\ _s _p k _a  st ms -> k st  ms st)
  put nst = Action (\ _s _p k _a _st ms -> k nst ms ())

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\ _s _p k _a st m -> k st m x)

-- | Distributes the session and shutdown continuation,
-- threads the state and diary.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\ s p k a st ms ->
                          let next nst nm x =
                                runAction (f x) s p k a nst nm
                          in runAction m s p next a st ms)

-- Instance commented out and action hiden, so that outside of this module
-- nobody can subvert Action by invoking arbitrary IO.
--   instance MonadIO Action where
liftIO :: IO a -> Action a
liftIO x = Action (\ _s _p k _a st ms -> x >>= k st ms)

-- | Run an action, with a given session, state and diary, in the @IO@ monad.
handlerToIO :: Session -> State -> Diary -> Action () -> IO ()
handlerToIO sess@Session{scops} state diary h =
  runAction h
    sess
    (dungeonPerception scops state)  -- create and cache perception
    (\ _ _ x -> return x)    -- final continuation returns result
    (\ msg ->
      ioError $ userError $ "unhandled abort  " ++ msg)  -- e.g., in AI code
    state
    diary

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: Rnd a -> Action a
rndToAction r = do
  g <- gets srandom
  let (a, ng) = runState r g
  modify (\ state -> state {srandom = ng})
  return a

-- | Actions and screen frames, including delays, resulting
-- from performing the actions.
type ActionFrame a = Action (a, [Maybe Color.SingleFrame])

-- | Return the value with an empty set of screen frames.
returnNoFrame :: a -> ActionFrame a
returnNoFrame a = return (a, [])

-- | Return the trivial value with a single frame to show.
returnFrame :: Color.SingleFrame -> ActionFrame ()
returnFrame fr = return ((), [Just fr])

-- | As the @when@ monad operation, but on type @ActionFrame ()@.
whenFrame :: Bool -> ActionFrame () -> ActionFrame ()
whenFrame True x  = x
whenFrame False _ = returnNoFrame ()

-- | Inject action into actions with screen frames.
inFrame :: Action () -> ActionFrame ()
inFrame act = act >> returnNoFrame ()

-- | The constant session information, not saved to the game save file.
data Session = Session
  { sfs   :: FrontendSession           -- ^ frontend session information
  , scops :: Kind.COps                 -- ^ game content
  , sbinding    :: Binding (ActionFrame ())
                                       -- ^ binding of keys to commands
  , sorigConfig :: Config.CP           -- ^ config from the config file
  }

-- | Get the frontend session.
getFrontendSession :: Action FrontendSession
getFrontendSession = Action (\ Session{sfs} _p k _a st ms -> k st ms sfs)

-- | Get the content operations.
getCOps :: Action Kind.COps
getCOps = Action (\ Session{scops} _p k _a st ms -> k st ms scops)

-- | Get the key binding.
getBinding :: Action (Binding (ActionFrame ()))
getBinding = Action (\ Session{sbinding} _p k _a st ms -> k st ms sbinding)

-- | Get the config from the config file.
getOrigConfig :: Action (Config.CP)
getOrigConfig =
  Action (\ Session{sorigConfig} _p k _a st ms -> k st ms sorigConfig)

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: Action a
abort = abortWith ""

-- | Abort with the given message.
abortWith :: Msg -> Action a
abortWith msg = Action (\ _s _p _k a _st _ms -> a msg)

-- | Abort and print the given msg if the condition is true.
abortIfWith :: Bool -> Msg -> Action a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: Bool -> Action a
neverMind b = abortIfWith b "never mind"

-- | Set the current exception handler. First argument is the handler,
-- second is the computation the handler scopes over.
tryWith :: (Msg -> Action a) -> Action a -> Action a
tryWith exc h = Action (\ s p k a st ms ->
                         let runA msg = runAction (exc msg) s p k a st ms
                         in runAction h s p k runA st ms)

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

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: (Msg -> Action ()) -> Action () -> Action ()
tryRepeatedlyWith exc h =
  tryWith (\ msg -> exc msg >> tryRepeatedlyWith exc h) h

-- | Try the given computation and silently catch failure.
tryIgnore :: Action () -> Action ()
tryIgnore =
  tryWith (\ msg -> if null msg
                    then return ()
                    else assert `failure` (msg, "in tryIgnore"))

-- | Try the given computation and silently catch failure,
-- returning empty set of screen frames.
tryIgnoreFrame :: ActionFrame () -> ActionFrame ()
tryIgnoreFrame =
  tryWith (\ msg -> if null msg
                    then returnNoFrame ()
                    else assert `failure` (msg, "in tryIgnoreFrame"))

-- | Get the current diary.
getDiary :: Action Diary
getDiary = Action (\ _s _p k _a st diary -> k st diary diary)

-- | Add a message to the current report.
msgAdd :: Msg -> Action ()
msgAdd nm = Action (\ _s _p k _a st ms ->
                     k st ms{sreport = addMsg (sreport ms) nm} ())

-- | Wipe out and set a new value for the history.
historyReset :: History -> Action ()
historyReset shistory = Action (\ _s _p k _a st Diary{sreport} ->
                                 k st Diary{..} ())

-- | Wipe out and set a new value for the current report.
msgReset :: Msg -> Action ()
msgReset nm = Action (\ _s _p k _a st ms ->
                       k st ms{sreport = singletonReport nm} ())

-- | Store current report in the history and reset report.
recordHistory :: Action ()
recordHistory = do
  Diary{sreport, shistory} <- getDiary
  unless (nullReport sreport) $ do
    config <- gets sconfig
    let historyMax = Config.get config "ui" "historyMax"
    msgReset ""
    historyReset $ takeHistory historyMax $ addReport sreport shistory

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
getKeyFrameCommand :: Color.SingleFrame -> Action (K.Key, K.Modifier)
getKeyFrameCommand frame = do
  fs <- getFrontendSession
  keyb <- getBinding
  (nc, modifier) <- liftIO $ promptGetKey fs [] frame
  return $ case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: Color.SingleFrame -> Action Bool
getConfirm frame = do
  fs <- getFrontendSession
  let keys = [ (K.Space, K.NoModifier), (K.Esc, K.NoModifier)]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Space -> return True
    _       -> return False

-- | A series of confirmations for all overlays.
getOverConfirm :: [Color.SingleFrame] -> Action Bool
getOverConfirm []     = return True
getOverConfirm (x:xs) = do
  b <- getConfirm x
  if b
    then getOverConfirm xs
    else return False

-- | A yes-no confirmation.
getYesNo :: Color.SingleFrame -> Action Bool
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

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: ColorMode -> Msg -> Action Bool
displayMore dm prompt = do
  frame <- drawPrompt dm (prompt ++ moreMsg)
  getConfirm frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: Msg -> Action Bool
displayYesNo prompt = do
  frame <- drawPrompt ColorBW (prompt ++ yesnoMsg)
  getYesNo frame

-- | Print a msg and several overlays, one per page.
-- All frames require confirmations. Raise @abort@ if the player presses ESC.
displayOverAbort :: Msg -> [Overlay] -> Action ()
displayOverAbort prompt xs = do
  let f x = drawOverlay ColorFull (prompt ++ " [SPACE, ESC]") (x ++ [moreMsg])
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
  frame <- drawOverlay ColorFull (prompt ++ " " ++ pressKeys) (x ++ [moreMsg])
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
  frame <- drawOverlay ColorFull (prompt ++ spc ++ ", ESC]") (over ++ more)
  fs <- getFrontendSession
  (key, modifier) <- liftIO $ promptGetKey fs legalKeys frame
  case key of
    K.Esc -> neverMind True
    K.Space | not (null rest) -> displayChoiceUI prompt rest keys
    _ -> return (key, modifier)

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFramePush :: Maybe Color.SingleFrame -> Action ()
displayFramePush mframe = do
  fs <- getFrontendSession
  liftIO $ displayFrame fs False mframe

-- | Draw the current level. The prompt is displayed, but not added
-- to history. The prompt is appended to the current message
-- and only the first screenful of the resulting overlay is displayed.
drawPrompt :: ColorMode -> Msg -> Action Color.SingleFrame
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
drawOverlay :: ColorMode -> Msg -> Overlay -> Action Color.SingleFrame
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

-- | Update player memory.
remember :: Action ()
remember = do
  per <- getPerception
  let vis = IS.toList (totalVisible per)
  rememberList vis

-- | Update player at the given list of locations..
rememberList :: [Point] -> Action ()
rememberList vis = do
  lvl <- gets slevel
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
  modify (updateLevel (updateLRMap (Kind.// rememberTile)))
  let alt Nothing      = Nothing
      alt (Just ([], _)) = Nothing
      alt (Just (t, _))  = Just (t, t)
      rememberItem = IM.alter alt
  modify (updateLevel (updateIMap (\ m -> foldr rememberItem m vis)))

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h =
  Action (\ sess@Session{scops} _ k a st ms ->
           runAction h sess (dungeonPerception scops st) k a st ms)

-- | Get the current perception.
getPerception :: Action Perception
getPerception = Action (\ _s per k _a s ms ->
                         k s ms (fromJust $ L.lookup (slid s) per))

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: ActorId -> (Actor -> Actor) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: (Actor -> Actor) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- | Obtains the current date and time.
currentDate :: Action ClockTime
currentDate = liftIO getClockTime

-- | Save the diary and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: State -> Diary -> Action ()
saveGameBkp state diary = liftIO $ Save.saveGameBkp state diary

-- | Dumps the current configuration to a file.
--
-- See 'Config.dump'.
dumpCfg :: FilePath -> Config.CP -> Action ()
dumpCfg fn config = liftIO $ Config.dump fn config

-- | Handle current score and display it with the high scores.
-- False if display of the scores was void or interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
handleScores :: Bool -> H.Status -> Int -> Action ()
handleScores write status total =
  when (total /= 0) $ do
    config  <- gets sconfig
    time    <- gets stime
    curDate <- currentDate
    let points = case status of
                   H.Killed _ -> (total + 1) `div` 2
                   _ -> total
    let score = H.ScoreRecord points (timeNegate time) curDate status
    (placeMsg, slideshow) <- liftIO $ H.register config write score
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
    Just (_, status@H.Camping) -> do
      -- Save and display in parallel.
      mv <- liftIO newEmptyMVar
      liftIO $ void $ forkIO (Save.saveGameFile s `finally` putMVar mv ())
      tryIgnore $ handleScores False status total
      void $ displayMore ColorFull "See you soon, stronger and braver!"
      liftIO $ takeMVar mv  -- wait until saved
      -- Do nothing, that is, quit the game loop.
    Just (showScreens, status@H.Killed{}) | showScreens -> do
      Diary{sreport} <- getDiary
      unless (nullReport sreport) $ do
        -- Sisplay any leftover report. Suggest it could be the cause of death.
        void $ displayMore ColorFull "Who would have thought?"
        recordHistory  -- prevent repeating the report
      tryWith
        (\ finalMsg ->
          let highScoreMsg = "Let's hope another party can save the day!"
              msg = if null finalMsg then highScoreMsg else finalMsg
          in void $ displayMore ColorFull msg
          -- Do nothing, that is, quit the game loop.
        )
        (do handleScores True status total
            go <- displayMore ColorFull "This time will be different."
            when (not go) $ abortWith "You could really win this time."
            restartGame
            handleTurn
        )
    Just (showScreens, status@H.Victor) | showScreens -> do
      Diary{sreport} <- getDiary
      unless (nullReport sreport) $ do
        -- Sisplay any leftover report. Suggest it could be the master move.
        void $ displayMore ColorFull "Brilliant, wasn't it?"
        recordHistory  -- prevent repeating the report
      tryIgnore $ handleScores True status total
      void $ displayMore ColorFull "Can it be done better, though?"
      restartGame
      handleTurn
    Just (_, H.Restart) -> do
      void $ displayMore ColorFull "This time for real."
      restartGame
      handleTurn
    _ -> return ()

restartGame :: Action ()
restartGame = do
  -- Take the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  config <- getOrigConfig
  cops <- getCOps
  diary <- getDiary
  state <- gameResetAction config cops
  modify $ const state
  saveGameBkp state diary

rmBkpSaveDiary :: Action ()
rmBkpSaveDiary  = do
  config <- gets sconfig
  diary <- getDiary
  -- Save diary often in case of crashes.
  liftIO $ Save.rmBkpSaveDiary config diary

-- TODO: do this inside Action ()
gameReset :: Config.CP -> Kind.COps -> IO State
gameReset config1 cops@Kind.COps{ coitem
                                , cofact=Kind.Ops{opick}} = do
  (g2, config2) <- Config.getSetGen config1 "dungeonRandomGenerator"
  let (DungeonState.FreshDungeon{..}, ag) =
        runState (DungeonState.generate cops config2) g2
      (sflavour, ag2) = runState (dungeonFlavourMap coitem) ag
      factionName = Config.getOption config2 "heroes" "faction"
      sfaction =
        evalState
          (opick (fromMaybe "playable" factionName) (const True)) ag2
  (g3, config3) <- Config.getSetGen config2 "startingRandomGenerator"
  let state =
        defaultState
          config3 sfaction sflavour freshDungeon entryLevel entryLoc g3
      hstate = initialHeroes cops entryLoc state
  return hstate
gameResetAction :: Config.CP -> Kind.COps -> Action State
gameResetAction config cops = liftIO $ gameReset config cops

-- | Wire together content, the definitions of game commands,
-- config and a high-level startup function
-- to form the starting game session. Evaluate to check for errors,
-- in particular verify content consistency.
-- Then create the starting game config from the default config file
-- and initialize the engine with the starting session.
startFrontend :: Kind.COps -> Binding (ActionFrame ()) -> Config.CP
              -> (Config.CP -> Session -> IO ()) -> IO ()
startFrontend !scops !sbinding !sconfig start = do
  -- The only option taken not from config in savegame, but from fresh config.
  let sorigConfig = sconfig
      configFont = fromMaybe "" $ Config.getOption sconfig "ui" "font"
      loop sfs = start sconfig Session{..}
  startup configFont loop

-- | Debugging.
debug :: String -> Action ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
