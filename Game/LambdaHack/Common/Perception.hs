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
  , PerLid
  , PerFid
  , totalVisible, totalSmelled
  , nullPer, addPer, diffPer
    -- * Perception cache
  , PerReachable(..)
  , CacheBeforeLit(..)
  , PerActor
  , PerceptionCache(..)
  , PerCacheLid
  , PerCacheFid
    -- * Assorted
  , FovCache3(..), emptyFovCache3, actorFovCache3
  , ItemFovCache, LightSources(..), ClearPoints, LitTerrain (..)
  , PersLit, PersLitA, PersFovCache, PersFovCacheA
  , PersLight, PersClear, PersLitTerrain
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
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

-- | Perception of a single faction, indexed by level identifier.
type PerLid = EM.EnumMap LevelId Perception

-- | Perception indexed by faction identifier.
-- This can't be added to @FactionDict@, because clients can't see it
-- for other factions.
type PerFid = EM.EnumMap FactionId PerLid

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

-- * Perception cache

-- | Visually reachable positions (light passes through them to the actor).
-- They need to be intersected with lit positions to obtain visible positions.
newtype PerReachable = PerReachable
    {preachable :: ES.EnumSet Point}
  deriving (Show, Eq)

data CacheBeforeLit = CacheBeforeLit
  { creachable :: !PerReachable
  , cnocto     :: !PerVisible
  , csmell     :: !PerSmelled
  }
  deriving (Show, Eq)

type PerActor = EM.EnumMap ActorId CacheBeforeLit

data PerceptionCache = PerceptionCache
  { ptotal   :: !CacheBeforeLit
  , perActor :: !PerActor
  }
  deriving (Show, Eq)

-- | Server cache of perceptions of a single faction,
-- indexed by level identifier.
type PerCacheLid = EM.EnumMap LevelId PerceptionCache

-- | Server cache of perceptions, indexed by faction identifier.
type PerCacheFid = EM.EnumMap FactionId PerCacheLid

-- * Assorted

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

actorFovCache3 :: ItemFovCache -> Actor -> FovCache3
actorFovCache3 sitemFovCache b =
  let processIid3 (FovCache3 sightAcc smellAcc lightAcc) (iid, (k, _)) =
        let FovCache3{..} =
              EM.findWithDefault emptyFovCache3 iid sitemFovCache
        in FovCache3 (k * fovSight + sightAcc)
                     (k * fovSmell + smellAcc)
                     (k * fovLight + lightAcc)
      processBag3 bag acc = foldl' processIid3 acc $ EM.assocs bag
      sslOrgan = processBag3 (borgan b) emptyFovCache3
  in processBag3 (beqp b) sslOrgan

type ItemFovCache = EM.EnumMap ItemId FovCache3

newtype LightSources = LightSources
    {lightSources :: ES.EnumSet Point}
  deriving (Show, Eq)

type ClearPoints = PointArray.Array Bool

newtype LitTerrain = LitTerrain
    {litTerrain :: ES.EnumSet Point}
  deriving (Show, Eq)

type PersLit = (PersFovCache, PersLight, PersClear, PersLitTerrain)

type PersLitA = (PersFovCacheA, PersLight, PersClear, PersLitTerrain)

-- | The cache of FOV information for a level, such as sight, smell
-- and light radiuses for each actor.
type PersFovCacheA = EM.EnumMap ActorId (Actor, FovCache3)

type PersFovCache = EM.EnumMap ActorId FovCache3

type PersLight = EM.EnumMap LevelId LightSources

type PersClear = EM.EnumMap LevelId ClearPoints

type PersLitTerrain = EM.EnumMap LevelId LitTerrain
