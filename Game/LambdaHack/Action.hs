-- | Game action monad and basic building blocks
-- for player and monster actions.
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Game.LambdaHack.Action
  ( -- * Actions and basic operations
    ActionFun, Action, handlerToIO, end, rndToAction
    -- * Actions returning frames
  , ActionFrame, returnNoFrame, whenFrame, inFrame
    -- * Game session and its accessors
  , Session(..), getCOps, getBinding
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryWithFrame, tryRepeatedlyWith, tryIgnore, tryIgnoreFrame
    -- * Diary and report
  , getDiary, msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyChoice, getOverConfirm
    -- * Display each frame and confirm
  , displayMore, displayYesNo, displayOverAbort
    -- * Assorted frame operations
  , displayOverlays, displayChoiceUI, displayFramePush, drawPrompt
    -- * Start of turn operations
  , startTurn, remember, rememberList
    -- * Assorted operations
  , getPerception, updateAnyActor, updatePlayerBody, advanceTime
    -- * Assorted primitives
  , currentDate, registerHS, saveGameBkp, saveGameFile, dumpCfg, debug
  ) where

import Control.Monad
import Control.Monad.State hiding (State, state, liftIO)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.List as L
import System.Time
import Data.Maybe
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Perception
import Game.LambdaHack.Display
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

-- | The type of the function inside any action.
-- (Separated from the @Action@ type to document each argument with haddock.)
type ActionFun r a =
   Session                           -- ^ session setup data
   -> (State -> Diary -> IO r)       -- ^ shutdown continuation
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
  fmap f (Action g) = Action (\ s e p k a st ms ->
                               let k' st' ms' = k st' ms' . f
                               in g s e p k' a st ms)

instance MonadState State Action where
  get     = Action (\ _s _e _p k _a  st ms -> k st  ms st)
  put nst = Action (\ _s _e _p k _a _st ms -> k nst ms ())

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\ _s _e _p k _a st m -> k st m x)

-- | Distributes the session and shutdown continuation,
-- threads the state and diary.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\ s e p k a st ms ->
                          let next nst nm x =
                                runAction (f x) s e p k a nst nm
                          in runAction m s e p next a st ms)

-- Instance commented out and action hiden, so that outside of this module
-- nobody can subvert Action by invoking arbitrary IO.
--   instance MonadIO Action where
liftIO :: IO a -> Action a
liftIO x = Action (\ _s _e _p k _a st ms -> x >>= k st ms)

-- | Run an action, with a given session, state and diary, in the @IO@ monad.
handlerToIO :: Session -> State -> Diary -> Action () -> IO ()
handlerToIO sess@Session{sfs, scops} state diary h =
  runAction h
    sess
    (\ ns ndiary -> Save.rmBkpSaveDiary ns ndiary
                 >> shutdown sfs)    -- get out of the game
    (dungeonPerception scops state)  -- create and cache perception
    (\ _ _ x -> return x)    -- final continuation returns result
    (\ msg ->
      ioError $ userError $ "unhandled abort  " ++ msg)  -- e.g., in AI code
    state
    diary

-- | End the game, i.e., invoke the shutdown continuation.
end :: Action a
end = Action (\ _s e _p _k _a s diary -> e s diary)

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
  , skeyb :: Binding (ActionFrame ())  -- ^ binding of keys to commands
  }

-- | Get the frontend session.
getFrontendSession :: Action FrontendSession
getFrontendSession = Action (\ Session{sfs} _e _p k _a st ms -> k st ms sfs)

-- | Get the content operations.
getCOps :: Action Kind.COps
getCOps = Action (\ Session{scops} _e _p k _a st ms -> k st ms scops)

-- | Get the key binding.
getBinding :: Action (Binding (ActionFrame ()))
getBinding = Action (\ Session{skeyb} _e _p k _a st ms -> k st ms skeyb)

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: Action a
abort = abortWith ""

-- | Abort with the given message.
abortWith :: Msg -> Action a
abortWith msg = Action (\ _s _e _p _k a _st _ms -> a msg)

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
tryWith exc h = Action (\ s e p k a st ms ->
                         let runA msg = runAction (exc msg) s e p k a st ms
                         in runAction h s e p k runA st ms)

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a frame with the abort message, if any.
tryWithFrame :: Action a -> ActionFrame a -> ActionFrame a
tryWithFrame exc h =
  let msgToFrames ""  = returnNoFrame ()
      msgToFrames msg = do
        msgReset ""
        fr <- drawPrompt ColorFull msg
        return ((), [Just fr])
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
getDiary = Action (\ _s _e _p k _a st diary -> k st diary diary)

-- | Add a message to the current report.
msgAdd :: Msg -> Action ()
msgAdd nm = Action (\ _s _e _p k _a st ms ->
                     k st ms{sreport = addMsg (sreport ms) nm} ())

-- | Wipe out and set a new value for the history.
historyReset :: History -> Action ()
historyReset shistory = Action (\ _s _e _p k _a st Diary{sreport} ->
                                 k st Diary{..} ())

-- | Wipe out and set a new value for the current report.
msgReset :: Msg -> Action ()
msgReset nm = Action (\ _s _e _p k _a st ms ->
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

-- | Wait for a player keypress.
getKeyChoice :: [(K.Key, K.Modifier)] -> Color.SingleFrame
          -> Action (K.Key, K.Modifier)
getKeyChoice keys frame = do
  fs <- getFrontendSession
  liftIO $ promptGetKey fs keys frame

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
-- All frames require confirmations. Raise @abort@ if the players presses ESC.
displayOverAbort :: Msg -> [Overlay] -> Action ()
displayOverAbort prompt xs = do
  let f x = drawOverlay ColorFull prompt (x ++ [moreMsg])
  frames <- mapM f xs
  b <- getOverConfirm frames
  when (not b) abort

-- | Print a msg and several overlays, one per page.
-- The last frame does not expect a confirmation.
displayOverlays :: Msg -> [Overlay] -> ActionFrame ()
displayOverlays _      []     = returnNoFrame ()
displayOverlays prompt [x]    = do
  frame <- drawOverlay ColorFull prompt x
  return $ ((), [Just frame])
displayOverlays prompt (x:xs) = do
  frame <- drawOverlay ColorFull prompt (x ++ [moreMsg])
  b <- getConfirm frame
  if b
    then displayOverlays prompt xs
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
  frame <- drawOverlay ColorFull (prompt ++ spc ++ ", ESC]") (over ++ more)
  (key, modifier) <- getKeyChoice ((K.Esc, K.NoModifier) : keysS) frame
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
startTurn :: Action () -> Action ()
startTurn action =
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
  Action (\ sess@Session{scops} e _ k a st ms ->
           runAction h sess e (dungeonPerception scops st) k a st ms)

-- | Get the current perception.
getPerception :: Action Perception
getPerception = Action (\ _s _e per k _a s ms ->
                         k s ms (fromJust $ L.lookup (slid s) per))

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: ActorId -> (Actor -> Actor) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: (Actor -> Actor) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- | Advance (or rewind) the move time for the given actor.
advanceTime :: Bool -> ActorId -> Action ()
advanceTime forward actor = do
  Kind.COps{coactor} <- getCOps
  let upd m@Actor{btime} =
        let speed = actorSpeed coactor m
            ticks = ticksPerMeter speed
            delta | forward   = ticks
                  | otherwise = timeNegate ticks
        in m { btime = timeAdd btime delta }
  updateAnyActor actor upd
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  when (actor == pl) $ do
    let updH a m = if bparty m == heroParty && a /= pl then upd m else m
    modify (updateLevel (updateActorDict (IM.mapWithKey updH)))

-- | Obtains the current date and time.
currentDate :: Action ClockTime
currentDate = liftIO getClockTime

-- | Take care of saving a new score to the table
-- and return a list of messages to display.
--
-- See 'H.register'.
registerHS :: Config.CP -> Bool -> H.ScoreRecord -> Action (String, [Overlay])
registerHS config write s = liftIO $ H.register config write s

-- | Save the diary and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: State -> Diary -> Action ()
saveGameBkp state diary = liftIO $ Save.saveGameBkp state diary

-- | Save a simple serialized version of the current state and diary.
--
-- See 'Save.saveGameFile'.
saveGameFile :: State -> Diary -> Action ()
saveGameFile state diary = liftIO $ Save.saveGameFile state diary

-- | Dumps the current configuration to a file.
--
-- See 'Config.dump'.
dumpCfg :: FilePath -> Config.CP -> Action ()
dumpCfg fn config = liftIO $ Config.dump fn config

-- | Debugging.
debug :: String -> Action ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
