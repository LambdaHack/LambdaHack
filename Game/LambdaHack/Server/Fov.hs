-- | Field Of View scanning.
--
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( -- * Perception cache
    FovValid(..), PerValidFid
  , PerReachable(..), CacheBeforeLucid(..), PerActor
  , PerceptionCache(..), PerCacheLid, PerCacheFid
    -- * Data used in FOV computation and cached to speed it up
  , FovShine(..), FovLucid(..), FovLucidLid
  , FovClear(..), FovClearLid, FovLit (..), FovLitLid
    -- * Operations
  , perceptionFromPTotal, perActorFromLevel, boundSightByCalm
  , totalFromPerActor, lucidFromLevel, perFidInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , cacheBeforeLucidFromActor, shineFromLevel, floorLightSources, lucidFromItems
  , litFromLevel, litInDungeon, clearFromLevel, clearInDungeon, lucidInDungeon
  , perLidFromFaction, perceptionCacheFromLevel
  , Matrix, fullscan
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import           GHC.Exts (inline)

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Server.FovDigital

-- * Perception cache types

data FovValid a =
    FovValid a
  | FovInvalid
  deriving (Show, Eq)

-- | Main perception validity map, for all factions.
type PerValidFid = EM.EnumMap FactionId (EM.EnumMap LevelId Bool)

-- | Visually reachable positions (light passes through them to the actor).
-- They need to be intersected with lucid positions to obtain visible positions.
newtype PerReachable = PerReachable {preachable :: ES.EnumSet Point}
  deriving (Show, Eq)

data CacheBeforeLucid = CacheBeforeLucid
  { creachable :: PerReachable
  , cnocto     :: PerVisible
  , csmell     :: PerSmelled
  }
  deriving (Show, Eq)

type PerActor = EM.EnumMap ActorId (FovValid CacheBeforeLucid)

-- We might cache even more effectively in terms of Enum{Set,Map} unions
-- if we recorded for each field how many actors see it (and how many
-- lights lit it). But this is complex and unions of EnumSets are cheaper
-- than the EnumMaps that would be required.
data PerceptionCache = PerceptionCache
  { ptotal   :: FovValid CacheBeforeLucid
  , perActor :: PerActor
  }
  deriving (Show, Eq)

-- | Server cache of perceptions of a single faction,
-- indexed by level identifier.
type PerCacheLid = EM.EnumMap LevelId PerceptionCache

-- | Server cache of perceptions, indexed by faction identifier.
type PerCacheFid = EM.EnumMap FactionId PerCacheLid

-- * Data used in FOV computation

-- | Map from level positions that currently hold item or actor(s) with shine
-- to the maximum of radiuses of the shining lights.
--
-- Note that floor and (many projectile) actors light on a single tile
-- should be additive for @FovShine@ to be incrementally updated.
--
-- @FovShine@ should not even be kept in @StateServer@, because it's cheap
-- to compute, compared to @FovLucid@ and invalidated almost as often
-- (not invalidated only by @UpdAlterTile@).
newtype FovShine = FovShine {fovShine :: EM.EnumMap Point Int}
  deriving (Show, Eq)

-- | Level positions with either ambient light or shining items or actors.
newtype FovLucid = FovLucid {fovLucid :: ES.EnumSet Point}
  deriving (Show, Eq)

type FovLucidLid = EM.EnumMap LevelId (FovValid FovLucid)

-- | Level positions that pass through light and vision.
newtype FovClear = FovClear {fovClear :: PointArray.Array Bool}
  deriving (Show, Eq)

type FovClearLid = EM.EnumMap LevelId FovClear

-- | Level positions with tiles that have ambient light.
newtype FovLit = FovLit {fovLit :: ES.EnumSet Point}
  deriving (Show, Eq)

type FovLitLid = EM.EnumMap LevelId FovLit

-- * Update of invalidated Fov data

-- | Compute positions visible (reachable and seen) by the party.
-- A position is lucid, if it's lit by an ambient light or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lucid position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
perceptionFromPTotal :: FovLucid -> CacheBeforeLucid -> Perception
perceptionFromPTotal FovLucid{fovLucid} ptotal =
  let nocto = pvisible $ cnocto ptotal
      reach = preachable $ creachable ptotal
      psight = PerVisible $ nocto `ES.union` (reach `ES.intersection` fovLucid)
      psmell = csmell ptotal
  in Perception{..}

perActorFromLevel :: PerActor -> (ActorId -> Actor) -> ActorAspect -> FovClear
                  -> PerActor
perActorFromLevel perActorOld getActorB actorAspect fovClear =
  -- Dying actors included, to let them see their own demise.
  let f _ fv@FovValid{} = fv
      f aid FovInvalid =
        let ar = actorAspect EM.! aid
            b = getActorB aid
        in FovValid $ cacheBeforeLucidFromActor fovClear b ar
  in EM.mapWithKey f perActorOld

boundSightByCalm :: Int -> Int64 -> Int
boundSightByCalm sight calm =
  min (fromEnum $ calm `div` (5 * oneM)) sight

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
-- Also compute positions seen by noctovision and perceived by smell.
cacheBeforeLucidFromActor :: FovClear -> Actor -> AspectRecord
                          -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs body AspectRecord{..} =
  let radius = boundSightByCalm aSight (bcalm body)
      creachable = PerReachable $ fullscan clearPs radius (bpos body)
      cnocto = PerVisible $ fullscan clearPs aNocto (bpos body)
      smellRadius = if aSmell >= 2 then 2 else 0
      csmell = PerSmelled $ fullscan clearPs smellRadius (bpos body)
  in CacheBeforeLucid{..}

totalFromPerActor :: PerActor -> CacheBeforeLucid
totalFromPerActor perActor =
  let as = map (\a -> case a of
                   FovValid x -> x
                   FovInvalid -> error $ "" `showFailure` perActor)
           $ EM.elems perActor
  in CacheBeforeLucid
       { creachable = PerReachable
                      $ ES.unions $ map (preachable . creachable) as
       , cnocto = PerVisible
                  $ ES.unions $ map (pvisible . cnocto) as
       , csmell = PerSmelled
                  $ ES.unions $ map (psmelled . csmell) as }

-- | Update lights on the level. This is needed every (even enemy)
-- actor move to show thrown torches.
-- We need to update lights even if cmd doesn't change any perception,
-- so that for next cmd that does, but doesn't change lights,
-- and operates on the same level, the lights are up to date.
-- We could make lights lazy to ensure no computation is wasted,
-- but it's rare that cmd changed them, but not the perception
-- (e.g., earthquake in an uninhabited corner of the active arena,
-- but the we'd probably want some feedback, at least sound).
lucidFromLevel :: FovClearLid -> FovLitLid -> State -> LevelId -> Level
               -> FovLucid
lucidFromLevel fovClearLid fovLitLid s lid lvl =
  let shine = shineFromLevel s lid lvl
      lucids = lucidFromItems (fovClearLid EM.! lid)
               $ EM.assocs $ fovShine shine
      litTiles = fovLitLid EM.! lid
  in FovLucid $ ES.unions $ fovLit litTiles : map fovLucid lucids

shineFromLevel :: State -> LevelId -> Level -> FovShine
shineFromLevel s lid lvl =
  let actorLights =
        [ (bpos b, radius)
        | (aid, b) <- inline actorAssocs (const True) lid s
        , let radius = aShine $ sactorAspect s EM.! aid
        , radius > 0 ]
      floorLights = floorLightSources (sdiscoAspect s) lvl
      allLights = floorLights ++ actorLights
      -- If there is light both on the floor and carried by actor
      -- (or several projectile actors), its radius is the maximum.
  in FovShine $ EM.fromListWith max allLights

floorLightSources :: DiscoveryAspect -> Level -> [(Point, Int)]
floorLightSources discoAspect lvl =
  -- Not enough oxygen to have more than one light lit on a given tile.
  -- Items obscuring or dousing off fire are not cumulative as well.
  let processIid (accLight, accDouse) (iid, _) =
        let AspectRecord{aShine} = discoAspect EM.! iid
        in case compare aShine 0 of
          EQ -> (accLight, accDouse)
          GT -> (max aShine accLight, accDouse)
          LT -> (accLight, min aShine accDouse)
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
  in [ (p, radius)
     | (p, bag) <- EM.assocs $ lfloor lvl  -- lembed are hidden
     , let (maxLight, maxDouse) = processBag bag (0, 0)
           radius = maxLight + maxDouse
     , radius > 0 ]

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or shining floor items. Note that an actor can be blind,
-- in which case he doesn't see his own light (but others,
-- from his or other factions, possibly do).
lucidFromItems :: FovClear -> [(Point, Int)] -> [FovLucid]
lucidFromItems clearPs allItems =
  let lucidPos (p, shine) = FovLucid $ fullscan clearPs shine p
  in map lucidPos allItems

-- * Computation of initial perception and caches

-- | Calculate the perception and its caches for the whole dungeon.
perFidInDungeon :: State -> ( FovLitLid, FovClearLid, FovLucidLid
                            , PerValidFid, PerCacheFid, PerFid)
perFidInDungeon s =
  let fovLitLid = litInDungeon s
      fovClearLid = clearInDungeon s
      fovLucidLid = lucidInDungeon  fovClearLid fovLitLid s
      perValidLid = EM.map (const True) (sdungeon s)
      perValidFid = EM.map (const perValidLid) (sfactionD s)
      f fid _ = perLidFromFaction fovLucidLid fovClearLid fid s
      em = EM.mapWithKey f $ sfactionD s
  in ( fovLitLid, fovClearLid, fovLucidLid
     , perValidFid, EM.map snd em, EM.map fst em)

litFromLevel :: Kind.COps -> Level -> FovLit
litFromLevel Kind.COps{coTileSpeedup} Level{ltile} =
  let litSet p t set = if Tile.isLit coTileSpeedup t then p : set else set
  in FovLit $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

litInDungeon :: State -> FovLitLid
litInDungeon s = EM.map (litFromLevel (scops s)) $ sdungeon s

clearFromLevel :: Kind.COps -> Level -> FovClear
clearFromLevel Kind.COps{coTileSpeedup} Level{ltile} =
  FovClear $ PointArray.mapA (Tile.isClear coTileSpeedup) ltile

clearInDungeon :: State -> FovClearLid
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

lucidInDungeon :: FovClearLid -> FovLitLid -> State-> FovLucidLid
lucidInDungeon fovClearLid fovLitLid s =
  EM.mapWithKey
    (\lid lvl -> FovValid $
       lucidFromLevel fovClearLid fovLitLid s lid lvl)
    $ sdungeon s

-- | Calculate perception of a faction.
perLidFromFaction :: FovLucidLid -> FovClearLid -> FactionId -> State
                  -> (PerLid, PerCacheLid)
perLidFromFaction fovLucidLid fovClearLid fid s =
  let em = EM.mapWithKey (\lid _ ->
                            perceptionCacheFromLevel fovClearLid fid lid s)
                         (sdungeon s)
      fovLucid lid = case EM.lookup lid fovLucidLid of
        Just (FovValid fl) -> fl
        _ -> error $ "" `showFailure` (lid, fovLucidLid)
      getValid (FovValid pc) = pc
      getValid FovInvalid = error $ "" `showFailure` fid
  in ( EM.mapWithKey (\lid pc ->
         perceptionFromPTotal (fovLucid lid) (getValid (ptotal pc))) em
     , em )

perceptionCacheFromLevel :: FovClearLid -> FactionId -> LevelId -> State
                         -> PerceptionCache
perceptionCacheFromLevel fovClearLid fid lid s =
  let fovClear = fovClearLid EM.! lid
      lvlBodies = inline actorAssocs (== fid) lid s
      f (aid, b) =
        let ar@AspectRecord{..} = sactorAspect s EM.! aid
        in if aSight <= 0 && aNocto <= 0 && aSmell <= 0  -- dumb missiles
           then Nothing
           else Just (aid, FovValid $ cacheBeforeLucidFromActor fovClear b ar)
      lvlCaches = mapMaybe f lvlBodies
      perActor = EM.fromDistinctAscList lvlCaches
      total = totalFromPerActor perActor
  in PerceptionCache{ptotal = FovValid total, perActor}

-- * The actual Fov algorithm

type Matrix = (Int, Int, Int, Int)

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: FovClear  -- ^ the array with clear points
         -> Int       -- ^ scanning radius
         -> Point     -- ^ position of the spectator
         -> ES.EnumSet Point
fullscan FovClear{fovClear} radius spectatorPos =
  if | radius <= 0 -> ES.empty
     | radius == 1 -> ES.singleton spectatorPos
     | radius == 2 -> inline squareUnsafeSet spectatorPos
     | otherwise ->
         mapTr (1, 0, 0, -1)   -- quadrant I
       $ mapTr (0, 1, 1, 0)    -- II (counter-clockwise)
       $ mapTr (-1, 0, 0, 1)   -- III
       $ mapTr (0, -1, -1, 0)  -- IV
       $ ES.singleton spectatorPos
 where
  mapTr :: Matrix -> ES.EnumSet Point -> ES.EnumSet Point
  mapTr m@(!_, !_, !_, !_) es = scan es (radius - 1) fovClear (trV m)

  -- This function is cheap, so no problem it's called twice
  -- for some points: once for @isClear@, once in @outside@.
  trV :: Matrix -> Bump -> Point
  {-# INLINE trV #-}
  trV (x1, y1, x2, y2) B{..} =
    shift spectatorPos $ Vector (x1 * bx + y1 * by) (x2 * bx + y2 * by)
