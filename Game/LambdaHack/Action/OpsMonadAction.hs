-- | Game action monads and their basic operations.
-- This is a restricted view, sufficient for all the code that uses the monads
-- (as opposed to implementing them).
-- Exports 'liftIO' for injecting @IO@ into the action monads,
-- but does not export the implementation of the monad types.
-- The 'liftIO' operation is used only in Action.hs and not re-exported
-- further.
module Game.LambdaHack.Action.OpsMonadAction
  ( -- * Types and type classes to do with actions
    MonadStateGet(..), MonadActionRO(liftIO), MonadAction
    -- * Accessors to the game session Reader
  , askFrontendSession, askCOps, askBinding, askConfigUI
    -- * Abort exception and its handler
  , abortWith, tryWith
    -- * The diary state
  , getDiary, msgAdd, historyReset, msgReset
  ) where

import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import Game.LambdaHack.FunMonadAction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- | Get the frontend session.
askFrontendSession :: MonadActionRO m => m FrontendSession
askFrontendSession = fun2actionRO (\Session{sfs} _p k _a _s _d -> k sfs)

-- | Get the content operations.
askCOps :: MonadActionRO m => m Kind.COps
askCOps = fun2actionRO (\Session{scops} _p k _a _s _d -> k scops)

-- | Get the key binding.
askBinding :: MonadActionRO m => m Binding
askBinding = fun2actionRO (\Session{sbinding} _p k _a _s _d -> k sbinding)

-- | Get the config from the config file.
askConfigUI :: MonadActionRO m => m ConfigUI
askConfigUI = fun2actionRO (\Session{sconfigUI} _p k _a _s _d -> k sconfigUI)

-- | Abort with the given message.
abortWith :: MonadActionRO m => Msg -> m a
abortWith msg = fun2actionRO (\_c _p _k a _s _d -> a msg)

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
