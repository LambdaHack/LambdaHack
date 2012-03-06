-- TODO: Add an export list, with sections, after the file is rewritten
-- according to #17.
-- | Game action monad and basic building blocks
-- for player and monster actions.
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Game.LambdaHack.Action
  ( ActionFun, Action, ActionFrame
  , returnNoFrame, whenFrame, inFrame
  , handlerToIO, end, rndToAction
  , Session(..), getCOps, getBinding
  , debug, tryWith, tryWithFrame, tryRepeatedlyWith, tryIgnore, tryIgnoreFrame
  , abort, abortWith, abortIfWith, neverMind
  , getDiary, historyReset, msgAdd, msgReset
  , getCommand, getConfirm, getChoice, getOverConfirm
  , displayNothingPush, displayPush, drawPrompt
  , displayMoreConfirm, displayMoreCancel, displayYesNo, displayChoice
  , displayOverlays, displayOverConfirm
  , withPerception, getPerception, updateAnyActor, updatePlayerBody
  , advanceTime, playerAdvanceTime
  , currentDate, registerHS, saveGameBkp, saveGameFile, dump
  ) where

import Control.Monad
import Control.Monad.State hiding (State, state, liftIO)
import qualified Data.IntMap as IM
import qualified Data.Map as M
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
import Game.LambdaHack.Content.ActorKind
import qualified Game.LambdaHack.Save as Save
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.HighScore as H
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Color as Color

-- | The type of the function inside any action.
-- (Separated from the @Action@ type to document each argument with haddock.)
type ActionFun r a =
   Session                           -- ^ session setup data
   -> (State -> Diary -> IO r)       -- ^ shutdown continuation
   -> Perception                     -- ^ cached perception
   -> (State -> Diary -> a -> IO r)  -- ^ continuation
   -> (Msg -> IO r)                  -- ^ failure/reset continuation
   -> State                          -- ^ current state
   -> Diary                          -- ^ current diary
   -> IO r

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action
  { runAction :: forall r . ActionFun r a
  }

type ActionFrame a = Action (a, [Color.SingleFrame])

returnNoFrame :: a -> ActionFrame a
returnNoFrame a = return (a, [])

whenFrame :: Bool -> ActionFrame () -> ActionFrame ()
whenFrame True x  = x
whenFrame False _ = returnNoFrame ()

inFrame :: Action () -> ActionFrame ()
inFrame act = act >> returnNoFrame ()

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

instance MonadState State Action where
  get     = Action (\ _s _e _p k _a  st ms -> k st  ms st)
  put nst = Action (\ _s _e _p k _a _st ms -> k nst ms ())

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
                 >> shutdown sfs)  -- get out of the game
    (perception scops state)  -- create and cache perception
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

-- | Debugging.
debug :: String -> Action ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x

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
        return ((), [fr])
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

-- | Try the given computation and silently catch failure.
tryIgnoreFrame :: ActionFrame () -> ActionFrame ()
tryIgnoreFrame =
  tryWith (\ msg -> if null msg
                    then returnNoFrame ()
                    else assert `failure` (msg, "in tryIgnoreFrame"))

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

-- | Get the current diary.
getDiary :: Action Diary
getDiary = Action (\ _s _e _p k _a st diary -> k st diary diary)

-- | Wipe out and set a new value for the history.
historyReset :: History -> Action ()
historyReset shistory = Action (\ _s _e _p k _a st Diary{sreport} ->
                                 k st Diary{..} ())

-- | Add to the current msg.
msgAdd :: Msg -> Action ()
msgAdd nm = Action (\ _s _e _p k _a st ms ->
                     k st ms{sreport = addMsg (sreport ms) nm} ())

-- | Wipe out and set a new value for the current report.
msgReset :: Msg -> Action ()
msgReset nm = Action (\ _s _e _p k _a st ms ->
                       k st ms{sreport = singletonReport nm} ())

-- | Wait for a player command.
getCommand :: Maybe Bool -> Action (K.Key, K.Modifier)
getCommand doPush = do
  fs <- getFrontendSession
  keyb <- getBinding
  (nc, modifier) <- liftIO $ nextEvent fs doPush
  return $ case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Push a wait for a single frame to the frame queue.
displayNothingPush :: Action ()
displayNothingPush = do
  fs <- getFrontendSession
  liftIO $ displayNothing fs

-- | Push the frame depicting the current level to the frame queue.
-- If there are any animations to play, they are pushed at this point, too,
-- and cleared. Only one screenful of the message is shown,
-- the rest is ignored.
displayPush :: Action ()
displayPush = do
  fs <- getFrontendSession
  cops <- getCOps
  per <- getPerception
  s@State{sanim} <- get
  Diary{sreport} <- getDiary
  let over = splitReport sreport
      sNew = s {sanim=[]}
  modify (const sNew)
  liftIO $ displayAnimation fs cops per sNew sanim
  -- TODO: at least some frames should be shown after anim, e.g., focus
  liftIO $ displayLevel fs ColorFull cops per sNew over

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
drawOver :: ColorMode -> Msg -> Overlay -> Action Color.SingleFrame
drawOver dm prompt overlay = do
  cops <- getCOps
  per <- getPerception
  s <- get
  Diary{sreport} <- getDiary
  let xsize = lxsize $ slevel s
      msgPrompt = renderReport $ addMsg sreport prompt
      over = padMsg xsize msgPrompt : overlay
  return $ draw dm cops per s over

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

-- | Print a yes/no question and return the player's answer.
displayYesNo :: Msg -> Action Bool
displayYesNo prompt = do
  -- Turn player's attention to the choice via BW colours.
  frame <- drawPrompt ColorBW (prompt ++ yesnoMsg)
  getYesNo frame

-- TODO: Find a way to add -more- into the frame, if needed.
-- Perhaps in the bottom left corner.
-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: Color.SingleFrame -> Action Bool
getConfirm frame = do
  fs <- getFrontendSession
  let keys = [ (K.Space, K.NoModifier), (K.Esc, K.NoModifier)]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Space -> return True
    _       -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMoreConfirm :: ColorMode -> Msg -> Action Bool
displayMoreConfirm dm prompt = do
  frame <- drawPrompt dm (prompt ++ moreMsg)
  getConfirm frame

-- | Print a message with a @more@ prompt, await confirmation
-- and ignore confirmation.
displayMoreCancel :: Msg -> Action ()
displayMoreCancel prompt = void $ displayMoreConfirm ColorFull prompt

-- TODO: implement with getOverConfirm, rename things so that drawOver
-- and displayOverConfirm are not confused.
-- TODO: add Abort to the name to indicate try may needed in all 3 below.
-- | Print a msg and several overlays, one per page.
-- The last frame does not expect a confirmation.
displayOverlays :: Msg -> [Overlay] -> ActionFrame ()
displayOverlays _      []     = returnNoFrame ()
displayOverlays prompt [x]    = do
  frame <- drawOver ColorFull prompt x
  return $ ((), [frame])
displayOverlays prompt (x:xs) = do
  frame <- drawOver ColorFull prompt (x ++ [moreMsg])
  b <- getConfirm frame
  if b
    then displayOverlays prompt xs
    else abort

-- | A series of confirmations for all overlays.
getOverConfirm :: [Color.SingleFrame] -> Action ()
getOverConfirm []     = return ()
getOverConfirm (x:xs) = do
  b <- getConfirm x
  if b
    then getOverConfirm xs
    else abort

-- | Print a msg and several overlays, one per page.
-- All frames require confirmations.
displayOverConfirm :: Msg -> [Overlay] -> Action ()
displayOverConfirm prompt xs = do
  let f x = drawOver ColorFull prompt (x ++ [moreMsg])
  frames <- mapM f xs
  getOverConfirm frames

-- | Wait for a player keypress.
getChoice :: [(K.Key, K.Modifier)] -> Color.SingleFrame
          -> Action (K.Key, K.Modifier)
getChoice keys frame = do
  fs <- getFrontendSession
  liftIO $ promptGetKey fs keys frame

-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoice :: Msg -> [Overlay] -> [(K.Key, K.Modifier)]
              -> Action (K.Key, K.Modifier)
displayChoice prompt ovs keys = do
  let (over, rest, spc, more, keysS) = case ovs of
        [] -> ([], [], "", [], keys)
        [x] -> (x, [], "", [], keys)
        x:xs -> (x, xs, ", SPACE", [moreMsg], (K.Space, K.NoModifier) : keys)
  frame <- drawOver ColorFull (prompt ++ spc ++ ", ESC]") (over ++ more)
  (key, modifier) <- getChoice ((K.Esc, K.NoModifier) : keysS) frame
  case key of
    K.Esc -> neverMind True
    K.Space | not (null rest) -> displayChoice prompt rest keys
    _ -> return (key, modifier)

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h = Action (\ sess@Session{scops} e _ k a st ms ->
                            runAction h sess e (perception scops st) k a st ms)

-- | Get the current perception.
getPerception :: Action Perception
getPerception = Action (\ _s _e p k _a st ms -> k st ms p)

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: ActorId -> (Actor -> Actor) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: (Actor -> Actor) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- | Advance the move time for the given actor.
advanceTime :: ActorId -> Action ()
advanceTime actor = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getCOps
  time <- gets stime
  let upd m = m { btime = time + aspeed (okind (bkind m)) }
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  s <- get
  -- If actor dead or not on current level, don't bother.
  when (memActor actor s) $ updateAnyActor actor upd
  when (actor == pl) $ do
    let updH a = if bparty a == heroParty then upd a else a
    modify (updateLevel (updateActor (IM.map updH)))

-- | Add a turn to the player time counter.
playerAdvanceTime :: Action ()
playerAdvanceTime = do
  pl <- gets splayer
  advanceTime pl

currentDate :: Action ClockTime
currentDate = liftIO getClockTime

registerHS :: Config.CP -> Bool -> H.ScoreRecord -> Action (String, [Overlay])
registerHS config write s = liftIO $ H.register config write s

saveGameBkp :: State -> Diary -> Action ()
saveGameBkp state diary = liftIO $ Save.saveGameBkp state diary

saveGameFile :: State -> Diary -> Action ()
saveGameFile state diary = liftIO $ Save.saveGameFile state diary

dump :: FilePath -> Config.CP -> Action ()
dump fn config = liftIO $ Config.dump fn config
