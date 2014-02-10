{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Actors perceiving other actors and the dungeon level.
--
-- Visibility works according to KISS. Everything that player sees is real.
-- There are no unmarked hidden tiles and only solid tiles can be marked,
-- so there are no invisible walls and to pass through an illusory wall,
-- you have use a turn bumping into it first. Only tiles marked with Suspect
-- can turn out to be another tile. (So, if all tiles are marked with
-- Suspect, the player knows nothing for sure, but this should be avoided,
-- because searching becomes too time-consuming.)
-- Each actor sees adjacent tiles, even when blind, so adjacent tiles are
-- known, so the actor can decide accurately whether to pass thorugh
-- or alter, etc.
--
-- Items are always real and visible. Actors are real, but can be invisible.
-- Invisible actors in walls can't be hit, but are hinted at when altering
-- the tile, so the player can flee or block. Invisible actors in open
-- space can be hit.
module Game.LambdaHack.Common.Perception
  ( Perception(Perception), PerceptionVisible(PerceptionVisible)
  , totalVisible, smellVisible
  , nullPer, addPer, diffPer
  , FactionPers, Pers
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point

newtype PerceptionVisible = PerceptionVisible
    {pvisible :: ES.EnumSet Point}
  deriving (Show, Eq, Binary)

-- TOOD: if really needed, optimize by representing as a set of intervals
-- or a set of bitmaps, like the internal representation of IntSet.
-- | The type representing the perception of a faction on a level.
data Perception = Perception
  { ptotal :: !PerceptionVisible  -- ^ sum over all actors
  , psmell :: !PerceptionVisible  -- ^ sum over actors that can smell
  }
  deriving (Show, Eq, Generic)

instance Binary Perception

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = EM.EnumMap LevelId Perception

-- | Perception indexed by faction identifier.
-- This can't be added to @FactionDict@, because clients can't see it.
type Pers = EM.EnumMap FactionId FactionPers

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> ES.EnumSet Point
totalVisible = pvisible . ptotal

-- | The set of tiles smelled by at least one hero.
smellVisible :: Perception -> ES.EnumSet Point
smellVisible = pvisible . psmell

nullPer :: Perception -> Bool
nullPer per = ES.null (totalVisible per)

addPer :: Perception -> Perception -> Perception
addPer per1 per2 =
  Perception
    { ptotal = PerceptionVisible
               $ totalVisible per1 `ES.union` totalVisible per2
    , psmell = PerceptionVisible
               $ smellVisible per1 `ES.union` smellVisible per2
    }

diffPer :: Perception -> Perception -> Perception
diffPer per1 per2 =
  Perception
    { ptotal = PerceptionVisible
               $ totalVisible per1 ES.\\ totalVisible per2
    , psmell = PerceptionVisible
               $ smellVisible per1 ES.\\ smellVisible per2
    }
