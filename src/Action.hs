{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Action where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.List as L
import qualified Data.IntMap as IM
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Perception
import Display hiding (display)
import Message
import State
import Level
import Movable
import MovableState
import MovableKind
import qualified Save

newtype Action a = Action
  { runAction ::
      forall r .
      Session ->
      IO r ->                             -- shutdown cont
      Perceptions ->                      -- cached perception
      (State -> Message -> a -> IO r) ->  -- continuation
      IO r ->                             -- failure/reset cont
      State ->                            -- current state
      Message ->                          -- current message
      IO r
  }

instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

-- | Invokes the continuation.
returnAction :: a -> Action a
returnAction x = Action (\ s e p k a st m -> k st m x)

-- | Distributes the session and shutdown continuation,
-- threads the state and message.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\ s e p k a st ms ->
                           let next nst nm x =
                                 runAction (f x) s e p k a nst nm
                           in  runAction m s e p next a st ms)

instance MonadIO Action where
  liftIO x = Action (\ s e p k a st ms -> x >>= k st ms)

instance MonadState State Action where
  get     = Action (\ s e p k a st ms -> k  st ms st)
  put nst = Action (\ s e p k a st ms -> k nst ms ())

-- | Exported function to run the monad.
handlerToIO :: Session -> State -> Message -> Action () -> IO ()
handlerToIO session state msg h =
  runAction h
    session
    (Save.rmBkp (sconfig state) >> shutdown session)  -- get out of the game
    (perception_ state)        -- cached perception
    (\ _ _ x -> return x)      -- final continuation returns result
    (ioError $ userError "unhandled abort")
    state
    msg

-- | Invoking a session command.
session :: (Session -> Action a) -> Action a
session f = Action (\ s e p k a st ms -> runAction (f s) s e p k a st ms)

-- | Invoking a session command.
sessionIO :: (Session -> IO a) -> Action a
sessionIO f = Action (\ s e p k a st ms -> f s >>= k st ms)

-- | Display the current level, without any message.
displayWithoutMessage :: Action Bool
displayWithoutMessage = Action (\ s e p k a st ms -> displayLevel False s p st "" Nothing >>= k st ms)

-- | Display the current level, with the current message.
display :: Action Bool
display = Action (\ s e p k a st ms -> displayLevel False s p st ms Nothing >>= k st ms)

-- | Display the current level in black and white, and the current message,
displayBW :: Action Bool
displayBW = Action (\ s e p k a st ms -> displayLevel True s p st ms Nothing >>= k st ms)

-- | Display an overlay on top of the current screen.
overlay :: String -> Action Bool
overlay txt = Action (\ s e p k a st ms -> displayLevel False s p st ms (Just txt) >>= k st ms)

-- | Set the current message.
messageWipeAndSet :: Message -> Action ()
messageWipeAndSet nm = Action (\ s e p k a st ms -> k st nm ())

-- | Add to the current message.
messageAdd :: Message -> Action ()
messageAdd nm = Action (\ s e p k a st ms -> k st (addMsg ms nm) ())

-- | Clear the current message.
resetMessage :: Action Message
resetMessage = Action (\ s e p k a st ms -> k st "" ms)

-- | Get the current message.
currentMessage :: Action Message
currentMessage = Action (\ s e p k a st ms -> k st ms ms)

-- | End the game, i.e., invoke the shutdown continuation.
end :: Action ()
end = Action (\ s e p k a st ms -> e)

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: Action a
abort = Action (\ s e p k a st ms -> a)

-- | Perform an action and signal an error if the result is False.
assertTrue :: Action Bool -> Action ()
assertTrue h = do
  b <- h
  when (not b) $ error "assertTrue: failure"

-- | Perform an action and signal an error if the result is True.
assertFalse :: Action Bool -> Action ()
assertFalse h = do
  b <- h
  when b $ error "assertFalse: failure"

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
debug x = return () -- liftIO $ hPutStrLn stderr x

-- | Print the given message, then abort.
abortWith :: Message -> Action a
abortWith msg = do
  messageWipeAndSet msg
  display
  abort

neverMind :: Bool -> Action a
neverMind b = abortIfWith b "never mind"

-- | Abort, and print the given message if the condition is true.
abortIfWith :: Bool -> Message -> Action a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Print message, await confirmation. Return value indicates
-- if the player tried to abort/escape.
messageMoreConfirm :: Bool -> Message -> Action Bool
messageMoreConfirm blackAndWhite msg = do
  messageAdd (msg ++ more)
  if blackAndWhite then displayBW else display
  session getConfirm

-- | Print message, await confirmation, ignore confirmation.
messageMore :: Message -> Action ()
messageMore msg = resetMessage >> messageMoreConfirm False msg >> return ()

-- | Print a yes/no question and return the player's answer.
messageYesNo :: Message -> Action Bool
messageYesNo msg = do
  messageWipeAndSet (msg ++ yesno)
  displayBW  -- turn player's attention to the choice
  session getYesNo

-- | Print a message and an overlay, await confirmation. Return value
-- indicates if the player tried to abort/escape.
messageOverlayConfirm :: Message -> String -> Action Bool
messageOverlayConfirm msg txt = messageOverlaysConfirm msg [txt]

-- | Prints several overlays, one per page, and awaits confirmation.
-- Return value indicates if the player tried to abort/escape.
messageOverlaysConfirm :: Message -> [String] -> Action Bool
messageOverlaysConfirm msg [] =
  do
    resetMessage
    display
    return True
messageOverlaysConfirm msg (x:xs) =
  do
    messageWipeAndSet msg
    b <- overlay (x ++ more)
    if b
      then do
        b <- session getConfirm
        if b
          then do
            messageOverlaysConfirm msg xs
          else stop
      else stop
  where
    stop = do
      resetMessage
      display
      return False

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h = Action (\ s e _ k a st ms ->
                            runAction h s e (perception_ st) k a st ms)

-- | Get the current perception.
currentPerception :: Action Perceptions
currentPerception = Action (\ s e p k a st ms -> k st ms p)

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: Action () -> Action ()
checkCursor h = do
  cursor <- gets scursor
  level  <- gets slevel
  if creturnLn cursor == lname level
    then h
    else abortWith "this command does not work on remote levels"

updateAnyActor :: Actor -> (Movable -> Movable) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

updatePlayerBody :: (Movable -> Movable) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- | Advance the move time for the given actor.
advanceTime :: Actor -> Action ()
advanceTime actor = do
  time <- gets stime
  let upd m = m { mtime = time + (nspeed (mkind m)) }
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  if (actor == pl || isAHero actor)
    then do
           modify (updateLevel (updateHeroes (IM.map upd)))
           when (not $ isAHero pl) $ updatePlayerBody upd
    else updateAnyActor actor upd

playerAdvanceTime :: Action ()
playerAdvanceTime = do
  pl <- gets splayer
  advanceTime pl
