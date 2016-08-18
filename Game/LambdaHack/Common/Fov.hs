{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( -- * Perception cache
    FovValid(..)
  , PerValidFid
  , PerReachable(..)
  , CacheBeforeLucid(..)
  , PerActor
  , PerceptionCache(..)
  , PerCacheLid
  , PerCacheFid
    -- * Data used in FOV computation and cached to speed it up
  , FovShine(..), FovLucid(..), FovLucidLid
  , FovClear(..), FovClearLid, FovLit (..), FovLitLid
    -- * Update of invalidated Fov data
  , perceptionFromPTotal, perActorFromLevel, totalFromPerActor, lucidFromLevel
    -- * Computation of initial perception and caches
  , perFidInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , cacheBeforeLucidFromActor
  , perceptionCacheFromLevel, perLidFromFaction
  , clearFromLevel, clearInDungeon
  , litFromLevel, litInDungeon, shineFromLevel
  , floorLightSources, lucidFromItems, lucidInDungeon
    -- * The actual Fov algorithm
  , fullscan
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.FovDigital
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector

-- * Perception cache types

data FovValid a =
    FovValid !a
  | FovInvalid
  deriving (Show, Eq)

-- | Main perception validity map, for all factions.
type PerValidFid = EM.EnumMap FactionId (EM.EnumMap LevelId Bool)

-- | Visually reachable positions (light passes through them to the actor).
-- They need to be intersected with lucid positions to obtain visible positions.
newtype PerReachable = PerReachable {preachable :: ES.EnumSet Point}
  deriving (Show, Eq)

data CacheBeforeLucid = CacheBeforeLucid
  { creachable :: !PerReachable
  , cnocto     :: !PerVisible
  , csmell     :: !PerSmelled
  }
  deriving (Show, Eq)

type PerActor = EM.EnumMap ActorId (FovValid CacheBeforeLucid)

-- We might cache even more effectively in terms of Enum{Set,Map} unions
-- if we recorded for each field how many actors see it (and how many
-- lights lit it). But this is complex and unions of EnumSets are cheaper
-- than the EnumMaps that would be required.
data PerceptionCache = PerceptionCache
  { ptotal   :: !(FovValid CacheBeforeLucid)
  , perActor :: !PerActor
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
-- Note: @ActorAspect@ and @FovShine@ shoudn't be in @State@,
-- because on client they need to be updated every time an item discovery
-- is made, unlike on the server, where it's much simpler and cheaper.
-- BTW, floor and (many projectile) actors light on a single tile
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

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
-- Also compute positions seen by noctovision and perceived by smell.
cacheBeforeLucidFromActor :: FovClear -> Actor -> AspectRecord
                          -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs body AspectRecord{..} =
  let radius = min (fromIntegral $ bcalm body `div` (5 * oneM)) aSight
      creachable = PerReachable $ fullscan clearPs radius (bpos body)
      cnocto = PerVisible $ fullscan clearPs aNocto (bpos body)
      -- TODO: until AI can handle/ignore it, only radius 2 used
      smellRadius = if aSmell >= 2 then 2 else 0
      csmell = PerSmelled $ fullscan clearPs smellRadius (bpos body)
  in CacheBeforeLucid{..}

totalFromPerActor :: PerActor -> CacheBeforeLucid
totalFromPerActor perActor =
  let as = map (\a -> case a of
                   FovValid x -> x
                   FovInvalid -> assert `failure` perActor)
           $ EM.elems perActor
  in CacheBeforeLucid
       { creachable = PerReachable
                      $ ES.unions $ map (preachable . creachable) as
       , cnocto = PerVisible
                  $ ES.unions $ map (pvisible . cnocto) as
       , csmell = PerSmelled
                  $ ES.unions $ map (psmelled . csmell) as }

-- Update lights on the level. This is needed every (even enemy)
-- actor move to show thrown torches.
-- We need to update lights even if cmd doesn't change any perception,
-- so that for next cmd that does, but doesn't change lights,
-- and operates on the same level, the lights are up to date.
-- We could make lights lazy to ensure no computation is wasted,
-- but it's rare that cmd changed them, but not the perception
-- (e.g., earthquake in an uninhabited corner of the active arena,
-- but the we'd probably want some feedback, at least sound).
-- TODO: if more speed needed, cache independently all actors
-- (not in CacheBeforeLucid, so that dark actors moving don't reset lucid)
-- and all floor positions, invalidate those that are changed,
-- recompute and union all afterwards.
lucidFromLevel :: DiscoveryAspect -> ActorAspect -> FovClearLid -> FovLitLid
               -> State -> LevelId -> Level
               -> FovLucid
lucidFromLevel discoAspect actorAspect fovClearLid fovLitLid s lid lvl =
  let shine = shineFromLevel discoAspect actorAspect s lid lvl
      lucids = lucidFromItems (fovClearLid EM.! lid)
               $ EM.assocs $ fovShine shine
      litTiles = fovLitLid EM.! lid
  in FovLucid $ ES.unions $ fovLit litTiles : map fovLucid lucids

shineFromLevel :: DiscoveryAspect -> ActorAspect -> State -> LevelId -> Level
               -> FovShine
shineFromLevel discoAspect actorAspect s lid lvl =
  let actorLights =
        [ (bpos b, radius)
        | (aid, b) <- actorAssocs (const True) lid s
        , let radius = aShine $ actorAspect EM.! aid
        , radius > 0 ]
      floorLights = floorLightSources discoAspect lvl
      allLights = floorLights ++ actorLights
      -- If there is light both on the floor and carried by actor
      -- (or several projectile actors), its radius is the maximum.
  in FovShine $ EM.fromListWith max allLights

floorLightSources :: DiscoveryAspect -> Level -> [(Point, Int)]
floorLightSources discoAspect lvl =
  let processIid shineAcc (iid, (k, _)) =
        let AspectRecord{aShine} = discoAspect EM.! iid
        in k * aShine + shineAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
  in [ (p, radius)
     | (p, bag) <- EM.assocs $ lfloor lvl  -- lembed are hidden
     , let radius = processBag bag 0
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
perFidInDungeon :: DiscoveryAspect -> State
                -> ( ActorAspect, FovLitLid, FovClearLid, FovLucidLid
                   , PerValidFid, PerCacheFid, PerFid)
perFidInDungeon discoAspect s =
  let actorAspect = actorAspectInDungeon discoAspect s
      fovLitLid = litInDungeon s
      fovClearLid = clearInDungeon s
      fovLucidLid =
        lucidInDungeon discoAspect actorAspect fovClearLid fovLitLid s
      perValidLid = EM.map (const True) (sdungeon s)
      perValidFid = EM.map (const perValidLid) (sfactionD s)
      f fid _ = perLidFromFaction actorAspect fovLucidLid fovClearLid fid s
      em = EM.mapWithKey f $ sfactionD s
  in ( actorAspect, fovLitLid, fovClearLid, fovLucidLid
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

lucidInDungeon :: DiscoveryAspect -> ActorAspect -> FovClearLid -> FovLitLid
               -> State
               -> FovLucidLid
lucidInDungeon discoAspect actorAspect fovClearLid fovLitLid s =
  EM.mapWithKey
    (\lid lvl -> FovValid $
       lucidFromLevel discoAspect actorAspect fovClearLid fovLitLid s lid lvl)
    $ sdungeon s

-- | Calculate perception of a faction.
perLidFromFaction :: ActorAspect -> FovLucidLid -> FovClearLid
                  -> FactionId -> State
                  -> (PerLid, PerCacheLid)
perLidFromFaction actorAspect fovLucidLid fovClearLid fid s =
  let em = EM.mapWithKey (\lid _ ->
             perceptionCacheFromLevel actorAspect fovClearLid fid lid s)
             (sdungeon s)
      fovLucid lid = case EM.lookup lid fovLucidLid of
        Just (FovValid fl) -> fl
        _ -> assert `failure` (lid, fovLucidLid)
      getValid (FovValid pc) = pc
      getValid FovInvalid = assert `failure` fid
  in ( EM.mapWithKey (\lid pc ->
         perceptionFromPTotal (fovLucid lid) (getValid (ptotal pc))) em
     , em )

perceptionCacheFromLevel :: ActorAspect -> FovClearLid
                         -> FactionId -> LevelId -> State
                         -> PerceptionCache
perceptionCacheFromLevel actorAspect fovClearLid fid lid s =
  let lvlBodies = EM.fromList $ actorAssocs (== fid) lid s
      bodyMap = EM.mapWithKey (\aid b -> (b, actorAspect EM.! aid)) lvlBodies
      fovClear = fovClearLid EM.! lid
      perActor =
        EM.map (FovValid . uncurry (cacheBeforeLucidFromActor fovClear)) bodyMap
      total = totalFromPerActor perActor
  in PerceptionCache{ptotal = FovValid total, perActor}

-- * The actual Fov algorithm

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
     | radius == 2 -> squareUnsafeSet spectatorPos
     | otherwise ->
         mapTr (\B{..} -> trV   bx  (-by))  -- quadrant I
       $ mapTr (\B{..} -> trV   by    bx)   -- II (we rotate counter-clockwise)
       $ mapTr (\B{..} -> trV (-bx)   by)   -- III
       $ mapTr (\B{..} -> trV (-by) (-bx))  -- IV
       $ ES.singleton spectatorPos
 where
  mapTr :: (Bump -> Point) -> ES.EnumSet Point -> ES.EnumSet Point
  {-# INLINE mapTr #-}
  mapTr tr es1 =
    foldl' (flip $ ES.insert . tr) es1 $ scan (radius - 1) (isCl . tr)

  isCl :: Point -> Bool
  {-# INLINE isCl #-}
  isCl = (fovClear PointArray.!)

  -- This function is cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ Vector x y
