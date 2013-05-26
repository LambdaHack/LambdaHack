{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.Action
  ( -- * Action monads
    MonadActionRO(..), MonadAction(..), MonadAtomic(..)
    -- * Shorthands
  , getsLevel, nHumans
  ) where

import Control.Monad.Writer.Strict (WriterT, lift)
import qualified Data.EnumMap.Strict as EM
import Data.Monoid

import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State

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

class MonadActionRO m => MonadAtomic m where
  execAtomic    :: Atomic -> m ()
  execCmdAtomic :: CmdAtomic -> m ()
  execCmdAtomic = execAtomic . CmdAtomic
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic

getsLevel :: MonadActionRO m => LevelId -> (Level -> a) -> m a
getsLevel lid f = getsState $ f . (EM.! lid) . sdungeon

nHumans :: MonadActionRO m => m Int
nHumans = do
  factionD <- getsState sfactionD
  return $ length $ filter isHumanFact $ EM.elems factionD
