{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Action where

import Control.Monad
import Control.Monad.State hiding (State)
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Perception
import Display2 hiding (display)
import Message
import State

newtype Action a = Action
  { runAction ::
      forall r .
      Session ->
      IO r ->                                          -- shutdown cont
      Perception ->                                    -- cached perception
      (State -> Message -> a -> IO r) ->               -- continuation
      IO r ->                                          -- failure/reset cont
      State ->                                         -- current state
      Message ->                                       -- current message
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
    (shutdown session)         -- get out of the game
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
displayWithoutMessage = Action (\ s e p k a st ms -> displayLevel s p st "" Nothing >>= k st ms)

-- | Display the current level, with the current message.
display :: Action Bool
display = Action (\ s e p k a st ms -> displayLevel s p st ms Nothing >>= k st ms)

-- | Display an overlay on top of the current screen.
overlay :: String -> Action Bool
overlay txt = Action (\ s e p k a st ms -> displayLevel s p st ms (Just txt) >>= k st ms)

-- | Set the current message.
message :: Message -> Action ()
message nm = Action (\ s e p k a st ms -> k st nm ())

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
abortWith msg =
  do
    message msg
    display
    abort

-- | Abort, and print the given message if the condition is true.
abortIfWith :: Bool -> Message -> Action a
abortIfWith True  = abortWith
abortIfWith False = const abort

-- | Print message, await confirmation. Return value indicates if the
-- player tried to abort/escape.
messageMoreConfirm :: Message -> Action Bool
messageMoreConfirm msg =
  do
    message (msg ++ more)
    display
    session getConfirm

-- | Print a yes/no question and return the player's answer.
messageYesNo :: Message -> Action Bool
messageYesNo msg =
  do
    message (msg ++ yesno)
    display
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
    message msg
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
    stop =
      do
        resetMessage
        display
        return False

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h = Action (\ s e _ k a st ms ->
                            runAction h s e (perception_ st) k a st ms)

-- | Get the current perception.
currentPerception :: Action Perception
currentPerception = Action (\ s e p k a st ms -> k st ms p)

