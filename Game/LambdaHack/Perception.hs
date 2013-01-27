{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Perception(..), totalVisible, actorSeesLoc, FactionPers, Pers
  , PerceptionVisible(..)
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Actor
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Point

-- TOOD: if really needed, optimize by representing as a set of intervals.
newtype PerceptionVisible = PerceptionVisible
  { pvisible :: IS.IntSet }
  deriving Show

-- | The type representing the perception of a faction on a level.
-- The total visibility holds the sum of FOVs of all actors
-- of a given faction on the level and servers only as a speedup.
data Perception = Perception
  { pactors :: IM.IntMap PerceptionVisible  -- ^ per actor
  , ptotal  :: PerceptionVisible            -- ^ sum for all actors
  }
  deriving Show

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = M.Map LevelId Perception

-- | Perception indexed by faction identifier.
type Pers = EM.EnumMap FactionId FactionPers

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> IS.IntSet
totalVisible = pvisible . ptotal

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per aid pos = pos `IS.member` pvisible (pactors per IM.! aid)
