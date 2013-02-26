{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Action
  ( -- * Action monads
    MonadActionAbort(..), MonadActionRO(..), MonadAction(..)
  , ConnCli(..), ConnFaction, ConnDict
    -- * Various ways to abort action
  , abort, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryRepeatedlyWith, tryIgnore
    -- * Shorthands
  , updateLevel, getsLevel
  ) where

import Control.Concurrent.STM.TQueue
import Control.Monad.Writer.Strict (WriterT (WriterT), lift, runWriterT)
import qualified Data.EnumMap.Strict as EM
import Data.Monoid
import qualified Data.Text as T

import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

-- | Connection channels between server and a single client.
data ConnCli c = ConnCli
  { toClient :: TQueue c
  , toServer :: TQueue [CmdSer]
  }

instance Show (ConnCli c) where
  show _ = "client channels"

-- | Human-controlled client, UI of the client, AI of the client.
type ConnFaction = (Maybe (ConnCli CmdUI), Maybe (ConnCli CmdCli))

-- | Connection information for each client and an optional AI client
-- for the same faction, indexed by faction identifier.
type ConnDict = EM.EnumMap FactionId ConnFaction

-- | The bottom of the action monads class semilattice.
class (Monad m, Functor m) => MonadActionAbort m where
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith      :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith    :: Msg -> m a

instance (Monoid a, MonadActionAbort m) => MonadActionAbort (WriterT a m) where
  tryWith exc m =
    WriterT $ tryWith (\msg -> runWriterT (exc msg)) (runWriterT m)
  abortWith   = lift . abortWith

class (Monad m, Functor m) => MonadActionRO m where
  getState    :: m State
  getsState   :: (State -> a) -> m a

instance (Monoid a, MonadActionRO m) => MonadActionRO (WriterT a m) where
  getState    = lift getState
  getsState   = lift . getsState

instance MonadActionRO m => Show (WriterT a m b) where
  show _ = "an action"

class MonadActionRO m => MonadAction m where
  modifyState :: (State -> State) -> m ()
  putState    :: State -> m ()

instance (Monoid a, MonadAction m) => MonadAction (WriterT a m) where
  modifyState = lift . modifyState
  putState    = lift . putState

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadActionAbort m => m a
abort = abortWith ""

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadActionAbort m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadActionAbort m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadActionAbort m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadActionAbort m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Update a given level data within state.
updateLevel :: MonadAction m => LevelId -> (Level -> Level) -> m ()
updateLevel lid f = modifyState $ updateDungeon $ EM.adjust f lid

getsLevel :: MonadActionRO m => LevelId -> (Level -> a) -> m a
getsLevel lid f = getsState $ f . (EM.! lid) . sdungeon
