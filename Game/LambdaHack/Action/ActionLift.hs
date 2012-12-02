{-# LANGUAGE OverloadedStrings #-}
-- | Game action monad and basic building blocks for player and monster
-- actions. Exports 'liftIO' for injecting 'IO' into the 'Action' monad,
-- but does not export the implementation of the Action monad.
-- The 'liftIO' and 'handlerToIO' operations are used only in Action.hs.
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Game.LambdaHack.Action.ActionLift
  ( -- * Actions and basic operations
    ActionFun, Action, liftIO, handlerToIO, withPerception, getPerception
    -- * Actions returning frames
  , ActionFrame, returnNoFrame, returnFrame, whenFrame, inFrame
    -- * Game session and assessors to its components
  , Session(..), getFrontendSession, getCOps, getBinding, getOrigCP
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * Diary and report
  , getDiary, msgAdd, historyReset, msgReset
  ) where

import Control.Monad.State hiding (State, state, liftIO)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Perception
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Animation (SingleFrame(..))

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

-- Instance commented out, so that outside of Action/ nobody can
-- subvert the Action monad by invoking arbitrary IO.
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
    (\ msg ->  -- e.g., in AI code
      ioError $ userError $ T.unpack $ "unhandled abort:" <+> msg)
    state
    diary

-- | Update the cached perception for the given computation.
withPerception :: Action () -> Action ()
withPerception h =
  Action (\ sess@Session{scops} _ k a st ms ->
           runAction h sess (dungeonPerception scops st) k a st ms)

-- | Get the current perception.
getPerception :: Action Perception
getPerception = Action (\ _s per k _a s ms ->
                         k s ms (fromJust $ L.lookup (slid s) per))

-- | Actions and screen frames, including delays, resulting
-- from performing the actions.
type ActionFrame a = Action (a, [Maybe SingleFrame])

-- | Return the value with an empty set of screen frames.
returnNoFrame :: a -> ActionFrame a
returnNoFrame a = return (a, [])

-- | Return the trivial value with a single frame to show.
returnFrame :: SingleFrame -> ActionFrame ()
returnFrame fr = return ((), [Just fr])

-- | As the @when@ monad operation, but on type @ActionFrame ()@.
whenFrame :: Bool -> ActionFrame () -> ActionFrame ()
whenFrame True x  = x
whenFrame False _ = returnNoFrame ()

-- | Inject action into actions with screen frames.
inFrame :: Action () -> ActionFrame ()
inFrame act = act >> returnNoFrame ()

-- | The information that is constant across a playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs   :: FrontendSession           -- ^ frontend session information
  , scops :: Kind.COps                 -- ^ game content
  , sbinding    :: Binding (ActionFrame ())
                                       -- ^ binding of keys to commands
  , sorigConfig :: ConfigIO.CP         -- ^ config from the config file
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
getOrigCP :: Action (ConfigIO.CP)
getOrigCP =
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

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: (Msg -> Action ()) -> Action () -> Action ()
tryRepeatedlyWith exc h =
  tryWith (\ msg -> exc msg >> tryRepeatedlyWith exc h) h

-- | Try the given computation and silently catch failure.
tryIgnore :: Action () -> Action ()
tryIgnore =
  tryWith (\ msg -> if T.null msg
                    then return ()
                    else assert `failure` msg <+> "in tryIgnore")

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
