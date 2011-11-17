{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Game.LambdaHack.Action where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.IntMap as IM
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Game.LambdaHack.Perception
import Game.LambdaHack.Display hiding (display)
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import qualified Game.LambdaHack.Save as Save
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random

newtype Action a = Action
  { runAction ::
      forall r .
      Session                         -- ^ session setup data
      -> IO r                         -- ^ shutdown cont
      -> Perceptions                  -- ^ cached perception
      -> (State -> Msg -> a -> IO r)  -- ^ continuation
      -> IO r                         -- ^ failure/reset cont
      -> State                        -- ^ current state
      -> Msg                          -- ^ current message
      -> IO r
  }

-- TODO: check if it's strict enough, if we don't keep old states for too long,
-- Perhaps make state type fields strict for that, too?
instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

-- | Invokes the continuation.
returnAction :: a -> Action a
returnAction x = Action (\ _s _e _p k _a st m -> k st m x)

-- | Distributes the session and shutdown continuation,
-- threads the state and message.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\ s e p k a st ms ->
                           let next nst nm x =
                                 runAction (f x) s e p k a nst nm
                           in  runAction m s e p next a st ms)

instance MonadIO Action where
  liftIO x = Action (\ _s _e _p k _a st ms -> x >>= k st ms)

instance MonadState State Action where
  get     = Action (\ _s _e _p k _a  st ms -> k st  ms st)
  put nst = Action (\ _s _e _p k _a _st ms -> k nst ms ())

-- | Exported function to run the monad.
handlerToIO :: Session -> State -> Msg -> Action () -> IO ()
handlerToIO sess@(_, _, cops) state msg h =
  runAction h
    sess
    (Save.rmBkp (sconfig state) >> shutdown sess)  -- get out of the game
    (perception_ cops state)        -- cached perception
    (\ _ _ x -> return x)      -- final continuation returns result
    (ioError $ userError "unhandled abort")
    state
    msg

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: Rnd a -> Action a
rndToAction r = do
  g <- gets srandom
  let (a, ng) = runState r g
  modify (\ state -> state {srandom = ng})
  return a

-- | Invoking a session command.
session :: (Session -> Action a) -> Action a
session f = Action (\ s e p k a st ms -> runAction (f s) s e p k a st ms)

-- | Invoking a session command.
sessionIO :: (Session -> IO a) -> Action a
sessionIO f = Action (\ s _e _p k _a st ms -> f s >>= k st ms)

-- | Display the current level with modified current message.
displayGeneric :: ColorMode -> (String -> String) -> Action Bool
displayGeneric dm f = Action (\ s _e p k _a st ms -> displayLevel dm s p st (f ms) Nothing >>= k st ms)

-- | Display the current level, with the current message and color. Most common.
display :: Action Bool
display = displayGeneric ColorFull id

-- | Display an overlay on top of the current screen.
overlay :: String -> Action Bool
overlay txt = Action (\ s _e p k _a st ms -> displayLevel ColorFull s p st ms (Just txt) >>= k st ms)

-- | Wipe out and set a new value for the current message.
messageReset :: Msg -> Action ()
messageReset nm = Action (\ _s _e _p k _a st _ms -> k st nm ())

-- | Add to the current message.
messageAdd :: Msg -> Action ()
messageAdd nm = Action (\ _s _e _p k _a st ms -> k st (addMsg ms nm) ())

-- | Clear the current message.
messageClear :: Action ()
messageClear = Action (\ _s _e _p k _a st _ms -> k st "" ())

-- | Get the current message.
currentMsg :: Action Msg
currentMsg = Action (\ _s _e _p k _a st ms -> k st ms ms)

-- | Get the content ops.
contentOps :: Action Kind.COps
contentOps = Action (\ (_, _, cops) _e _p k _a st ms -> k st ms cops)

-- | Get the content ops modified by a function.
contentf :: (Kind.COps -> a) -> Action a
contentf f = Action (\ (_, _, cops) _e _p k _a st ms -> k st ms (f cops))

-- | End the game, i.e., invoke the shutdown continuation.
end :: Action ()
end = Action (\ _s e _p _k _a _st _ms -> e)

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: Action a
abort = Action (\ _s _e _p _k a _st _ms -> a)

-- | Set the current exception handler. First argument is the handler,
-- second is the computation the handler scopes over.
tryWith :: Action () -> Action () -> Action ()
tryWith exc h = Action (\ s e p k a st ms -> runAction h s e p k (runAction exc s e p k a st ms) st ms)

-- | Takes a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: Action () -> Action () -> Action ()
tryRepeatedlyWith exc h = tryWith (exc >> tryRepeatedlyWith exc h) h

-- | Try the given computation and silently catch failure.
try :: Action () -> Action ()
try = tryWith (return ())

-- | Try the given computation until it succeeds without failure.
tryRepeatedly :: Action () -> Action ()
tryRepeatedly = tryRepeatedlyWith (return ())

-- | Print a debug message or ignore.
debug :: String -> Action ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x

-- | Print the given message, then abort.
abortWith :: Msg -> Action a
abortWith msg = do
  messageReset msg
  display
  abort

neverMind :: Bool -> Action a
neverMind b = abortIfWith b "never mind"

-- | Abort, and print the given message if the condition is true.
abortIfWith :: Bool -> Msg -> Action a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Print message, await confirmation. Return value indicates
-- if the player tried to abort/escape.
messageMoreConfirm :: ColorMode -> Msg -> Action Bool
messageMoreConfirm dm msg = do
  messageAdd (msg ++ more)
  displayGeneric dm id
  session getConfirm

-- | Print message, await confirmation, ignore confirmation.
messageMore :: Msg -> Action ()
messageMore msg = messageClear >> messageMoreConfirm ColorFull msg >> return ()

-- | Print a yes/no question and return the player's answer.
messageYesNo :: Msg -> Action Bool
messageYesNo msg = do
  messageReset (msg ++ yesno)
  displayGeneric ColorBW id  -- turn player's attention to the choice
  session getYesNo

-- | Print a message and an overlay, await confirmation. Return value
-- indicates if the player tried to abort/escape.
messageOverlayConfirm :: Msg -> String -> Action Bool
messageOverlayConfirm msg txt = messageOverlaysConfirm msg [txt]

-- | Prints several overlays, one per page, and awaits confirmation.
-- Return value indicates if the player tried to abort/escape.
messageOverlaysConfirm :: Msg -> [String] -> Action Bool
messageOverlaysConfirm _msg [] =
  do
    messageClear
    display
    return True
messageOverlaysConfirm msg (x:xs) =
  do
    messageReset msg
    b0 <- overlay (x ++ more)
    if b0
      then do
        b <- session getConfirm
        if b
          then messageOverlaysConfirm msg xs
          else stop
      else stop
  where
    stop = do
      messageClear
      display
      return False

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h = Action (\ s@(_, _, cops) e _ k a st ms ->
                            runAction h s e (perception_ cops st) k a st ms)

-- | Get the current perception.
currentPerception :: Action Perceptions
currentPerception = Action (\ _s _e p k _a st ms -> k st ms p)

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: Action () -> Action ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "this command does not work on remote levels"

updateAnyActor :: ActorId -> (Actor -> Actor) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

updatePlayerBody :: (Actor -> Actor) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- | Advance the move time for the given actor.
advanceTime :: ActorId -> Action ()
advanceTime actor = do
  Kind.Ops{okind} <- contentf Kind.coactor
  time <- gets stime
  let upd m = m { btime = time + aspeed (okind (bkind m)) }
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  if actor == pl || isAHero actor
    then do
           modify (updateLevel (updateHeroes (IM.map upd)))
           unless (isAHero pl) $ updatePlayerBody upd
    else updateAnyActor actor upd

playerAdvanceTime :: Action ()
playerAdvanceTime = do
  pl <- gets splayer
  advanceTime pl
