{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings #-}
-- | Game action monad and basic building blocks for player and monster
-- actions. Exports 'liftIO' for injecting 'IO' into the 'Action' monad,
-- but does not export the implementation of the Action monad.
-- The 'liftIO' and 'handlerToIO' operations are used only in Action.hs.
module Game.LambdaHack.Action.ActionLift
  ( -- * Actions and basic operations
    ActionFun, MonadAction, MonadActionRO(get, gets), Action, Frames
  , handlerToIO, withPerception, getPerception
    -- * Game session and assessors to its components
  , Session(..), askFrontendSession, askCOps, askBinding, askConfigUI
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * Diary and report
  , getDiary, msgAdd, historyReset, msgReset
  ) where

import Control.Monad.IO.Class
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Animation (SingleFrame (..))
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

-- | The type of the function inside any action.
type ActionFun a =
   Session                            -- ^ client session setup data
   -> DungeonPerception               -- ^ cached perception
   -> (State -> Diary -> a -> IO ())  -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

-- | The type of the function inside any read-only action.
type ActionFunRO a =
   Session                            -- ^ client session setup data
   -> DungeonPerception               -- ^ cached perception
   -> (a -> IO ())                    -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current state
   -> Diary                           -- ^ current diary
   -> IO ()

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action {runAction :: ActionFun a}

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\_c _p k _a s d -> k s d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and diary.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\c p k a s d ->
                          let next ns nd x =
                                runAction (f x) c p k a ns nd
                          in runAction m c p next a s d)

-- TODO: check if it's strict enough, if we don't keep old states for too long,
-- Perhaps make state type fields strict for that, too?
instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

instance Show (Action a) where
  show _ = "an action"

class (Monad m, Functor m, Show (m ())) => MonadActionRO m where
  fun2actionRO :: ActionFunRO a -> m a
  -- | Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith      :: (Msg -> m a) -> m a -> m a
  get :: m State
  get = fun2actionRO (\_c _p k _a s _d -> k s)
  gets :: (State -> a) -> m a
  gets = (`fmap` get)

-- The following seems to trigger a GHC bug (Overlapping instances for Show):
-- instance MonadActionRO m => Show (m a) where
--   show _ = "an action"

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

instance MonadActionRO m => Show (WriterT Frames m a) where
  show _ = "an action"

instance MonadActionRO m => MonadActionRO (WriterT Frames m) where
  fun2actionRO = lift . fun2actionRO
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)

class (MonadActionRO m, St.MonadState State m) => MonadAction m where
  fun2action :: ActionFun a -> m a

instance MonadAction m => MonadAction (WriterT Frames m) where
  fun2action = lift . fun2action

instance MonadActionRO Action where
  fun2actionRO f = Action (\c p k a s d -> f c p (k s d) a s d)
  tryWith exc m =
    Action (\c p k a s d ->
             let runA msg = runAction (exc msg) c p k a s d
             in runAction m c p k runA s d)

instance MonadAction Action where
  fun2action = Action

instance Functor Action where
  fmap f m = fun2actionRO (\c p k a s d ->
                            runAction m c p (\_ _ -> k . f) a s d)

--instance MonadAction m => MonadState State m where
instance St.MonadState State Action where
  get    = get
  put ns = fun2action (\_c _p k _a _s d -> k ns d ())

instance MonadIO Action where
  liftIO x = fun2actionRO (\_c _p k _a _s _d -> x >>= k)

-- | Run an action, with a given session, state and diary, in the @IO@ monad.
handlerToIO :: Session -> State -> Diary -> Action () -> IO ()
handlerToIO sess@Session{scops} state diary m =
  runAction m
    sess
    (dungeonPerception scops state)  -- create and cache perception
    (\_ _ _ -> return ())  -- final continuation returns result
    (\msg -> fail $ T.unpack $ "unhandled abort:" <+> msg)  -- e.g., in AI code
    state
    diary

-- TODO: RO
-- | Update the cached perception for the given computation.
withPerception :: MonadAction m => m () -> m ()
withPerception m = do
  cops <- askCOps
  s <- get
  let per = dungeonPerception cops s
  return undefined
--  fun2action (\c@Session{scops} _ k a s d ->
--           action2fun m c per k a s d)

-- | Get the current perception.
getPerception :: MonadActionRO m => m Perception
getPerception = fun2actionRO (\_c per k _a s _d ->
                         k (fromJust $ L.lookup (slid s) per))

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
data Session = Session
  { sfs       :: FrontendSession           -- ^ frontend session information
  , scops     :: Kind.COps                 -- ^ game content
  , sbinding  :: Binding (WriterT Frames Action ())  -- ^ binding of keys to commands
  , sconfigUI :: ConfigUI                  -- ^ the UI config for this session
  }

-- | Get the frontend session.
askFrontendSession :: MonadActionRO m => m FrontendSession
askFrontendSession = fun2actionRO (\Session{sfs} _p k _a _s _d -> k sfs)

-- | Get the content operations.
askCOps :: MonadActionRO m => m Kind.COps
askCOps = fun2actionRO (\Session{scops} _p k _a _s _d -> k scops)

-- | Get the key binding.
askBinding :: MonadActionRO m => m (Binding (WriterT Frames Action ()))
askBinding = fun2actionRO (\Session{sbinding} _p k _a _s _d -> k sbinding)

-- | Get the config from the config file.
askConfigUI :: MonadActionRO m => m ConfigUI
askConfigUI = fun2actionRO (\Session{sconfigUI} _p k _a _s _d -> k sconfigUI)

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadActionRO m => m a
abort = abortWith ""

-- | Abort with the given message.
abortWith :: MonadActionRO m => Msg -> m a
abortWith msg = fun2actionRO (\_c _p _k a _s _d -> a msg)

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadActionRO m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadActionRO m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadActionRO m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadActionRO m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Get the current diary.
getDiary :: MonadActionRO m => m Diary
getDiary = fun2actionRO (\_c _p k _a _s d -> k d)

-- | Add a message to the current report.
msgAdd :: MonadAction m => Msg -> m ()
msgAdd msg = fun2action (\_c _p k _a s d ->
                     k s d{sreport = addMsg (sreport d) msg} ())

-- | Wipe out and set a new value for the history.
historyReset :: MonadAction m => History -> m ()
historyReset shistory = fun2action (\_c _p k _a s Diary{sreport} ->
                                 k s Diary{..} ())

-- | Wipe out and set a new value for the current report.
msgReset :: MonadAction m => Msg -> m ()
msgReset msg = fun2action (\_c _p k _a s d ->
                            k s d{sreport = singletonReport msg} ())
