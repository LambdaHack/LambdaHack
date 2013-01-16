{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Game action monads and basic building blocks for player and monster
-- actions. Has no access to the the main action type @Action@.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Action
  ( -- * Action monads
    MonadActionRoot
  , MonadActionRO( getState, getsState )
  , MonadAction( putState, modifyState )
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore
    -- * Assorted primitives
  , rndToAction
  , debug
  ) where

import qualified Control.Monad.State as St
import Data.Text (Text)
import qualified Data.Text as T
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import Game.LambdaHack.ActionClass (MonadAction (..), MonadActionRO (..),
                                    MonadActionRoot (..))
import Game.LambdaHack.Msg
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadActionRoot m => m a
abort = abortWith ""

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadActionRoot m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadActionRoot m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadActionRoot m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadActionRoot m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadAction m => Rnd a -> m a
rndToAction r = do
  g <- getsState srandom
  let (a, ng) = St.runState r g
  modifyState $ updateRandom $ const ng
  return a

-- | Debugging.
debug :: MonadActionRoot m => Text -> m ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x
