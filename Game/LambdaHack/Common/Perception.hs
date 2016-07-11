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
  ( -- * Public perception
    PerVisible(..)
  , PerSmelled(..)
  , Perception(..)
  , totalVisible, totalSmelled
  , nullPer, addPer, diffPer
    -- * Server perception
  , PerReachable(..)
  , PerCache(..)
  , PerActor
  , PerceptionServer(..)
    -- * Assorted
  , PublicPers
  , ServerPers
  , Pers(..)
  , FovCache3(..), emptyFovCache3
  , PersLit, PersLitA, PersFovCache, PersFovCacheA, PersLight, PersClear
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- * Public perception

-- | Visible positions.
newtype PerVisible = PerVisible
    {pvisible :: ES.EnumSet Point}
  deriving (Show, Eq, Binary)

-- | Smelled positions.
newtype PerSmelled = PerSmelled
    {psmelled :: ES.EnumSet Point}
  deriving (Show, Eq, Binary)

-- | The type representing the perception of a faction on a level.
data Perception = Perception
  { psight :: !PerVisible
  , psmell :: !PerSmelled
  }
  deriving (Show, Eq, Generic)

instance Binary Perception

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> ES.EnumSet Point
totalVisible = pvisible . psight

-- | The set of tiles smelt by at least one hero.
totalSmelled :: Perception -> ES.EnumSet Point
totalSmelled = psmelled . psmell

nullPer :: Perception -> Bool
nullPer per = ES.null (totalVisible per) && ES.null (totalSmelled per)

addPer :: Perception -> Perception -> Perception
addPer per1 per2 =
  Perception
    { psight = PerVisible
               $ totalVisible per1 `ES.union` totalVisible per2
    , psmell = PerSmelled
               $ totalSmelled per1 `ES.union` totalSmelled per2
    }

diffPer :: Perception -> Perception -> Perception
diffPer per1 per2 =
  Perception
    { psight = PerVisible
               $ totalVisible per1 ES.\\ totalVisible per2
    , psmell = PerSmelled
               $ totalSmelled per1 ES.\\ totalSmelled per2
    }

-- * Server perception

-- | Visually reachable positions (light passes through them to the actor).
-- They need to be intersected with lit positions to obtain visible positions.
newtype PerReachable = PerReachable
    {preachable :: ES.EnumSet Point}
  deriving (Show, Eq)

data PerCache = PerCache
  { creachable :: !PerReachable
  , cnocto     :: !PerVisible
  , csmell     :: !PerSmelled
  }
  deriving (Show, Eq)

type PerActor = EM.EnumMap ActorId PerCache

data PerceptionServer = PerceptionServer
  { ptotal   :: !PerCache
  , perActor :: !PerActor
  }
  deriving (Show, Eq)

-- * Assorted

-- | Perception of a single faction, indexed by level identifier.
type PublicPers = EM.EnumMap LevelId Perception

-- | Server cache of perceptions of a single faction,
-- indexed by level identifier.
type ServerPers = EM.EnumMap LevelId PerceptionServer

-- | Perception indexed by faction identifier.
-- This can't be added to @FactionDict@, because clients can't see it.
data Pers = Pers
  { ppublic :: !(EM.EnumMap FactionId PublicPers)

  , pserver :: !(EM.EnumMap FactionId ServerPers)
  }
  deriving (Show, Eq)

data FovCache3 = FovCache3
  { fovSight :: !Int
  , fovSmell :: !Int
  , fovLight :: !Int
  }
  deriving (Show, Eq)

instance Binary FovCache3 where
  put FovCache3{..} = do
    put fovSight
    put fovSmell
    put fovLight
  get = do
    fovSight <- get
    fovSmell <- get
    fovLight <- get
    return $! FovCache3{..}

emptyFovCache3 :: FovCache3
emptyFovCache3 = FovCache3 0 0 0

type PersLit = (PersFovCache, PersLight, PersClear, PersLight)

type PersLitA = (PersFovCacheA, PersLight, PersClear, PersLight)

-- | The cache of FOV information for a level, such as sight, smell
-- and light radiuses for each actor.
type PersFovCacheA = EM.EnumMap ActorId (Actor, FovCache3)

type PersFovCache = EM.EnumMap ActorId FovCache3

type PersLight = EM.EnumMap LevelId (ES.EnumSet Point)

type PersClear = EM.EnumMap LevelId (PointArray.Array Bool)
