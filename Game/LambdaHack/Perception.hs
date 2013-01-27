{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Perception(..), totalVisible, actorSeesLoc, FactionPers, Pers
  , PerceptionVisible(..)
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Actor
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Point

-- TOOD: if really needed, optimize by representing as a set of intervals.
newtype PerceptionVisible = PerceptionVisible
  { pvisible :: ES.EnumSet Point}
  deriving Show

-- | The type representing the perception of a faction on a level.
-- The total visibility holds the sum of FOVs of all actors
-- of a given faction on the level and servers only as a speedup.
data Perception = Perception
  { pactors :: EM.EnumMap ActorId PerceptionVisible  -- ^ per actor
  , ptotal  :: PerceptionVisible                     -- ^ sum for all actors
  }
  deriving Show

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = EM.EnumMap LevelId Perception

-- | Perception indexed by faction identifier.
type Pers = EM.EnumMap FactionId FactionPers

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> ES.EnumSet Point
totalVisible = pvisible . ptotal

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per aid pos = pos `ES.member` pvisible (pactors per EM.! aid)
