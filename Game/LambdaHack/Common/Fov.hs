{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( perFidInDungeon, perceptionFromResets, perceptionFromPTotal
  , clearInDungeon, litTerrainInDungeon, lightInDungeon, fovCacheInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
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
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector

perceptionFromResets :: PerActor -> Either Bool [ActorId]
                     -> PersLitA -> FactionId -> LevelId
                     -> (Perception, PerceptionCache)
perceptionFromResets perActor0 resetsActor
                     (persFovCache, persLight, persClear, _)
                     fid lid =
  -- Dying actors included, to let them see their own demise.
  let bodyMap = EM.filter (\(b, _) -> bfid b == fid && blid b == lid)
                          persFovCache
      clearPs = persClear EM.! lid
      combine aid bcache per = Just $
        if either id (aid `elem`) resetsActor
        then cacheBeforeLitFromActor clearPs bcache
        else per
      only1 tbcache = EM.map (cacheBeforeLitFromActor clearPs) tbcache
      only2 = const EM.empty  -- dead or stair-using actors removed
      perActor = EM.mergeWithKey combine only1 only2 bodyMap perActor0
  in perceptionFromPerActor perActor persLight lid

perceptionFromPerActor :: PerActor -> PersLight -> LevelId
                       -> (Perception, PerceptionCache)
perceptionFromPerActor perActor persLight lid =
  -- We don't check if any actor changed, because almost surely one is.
  -- Exception: when an actor is destroyed, but then union differs.
  let ptotal = CacheBeforeLit
        { creachable = PerReachable
                       $ ES.unions $ map (preachable . creachable)
                       $ EM.elems perActor
        , cnocto = PerVisible
                   $ ES.unions $ map (pvisible . cnocto)
                   $ EM.elems perActor
        , csmell = PerSmelled
                   $ ES.unions $ map (psmelled . csmell)
                   $ EM.elems perActor }
  in (perceptionFromPTotal ptotal persLight lid, PerceptionCache{..})

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
-- Also compute positions seen by noctovision and perceived by smell.
cacheBeforeLitFromActor :: ClearPoints -> (Actor, FovCache3) -> CacheBeforeLit
cacheBeforeLitFromActor clearPs (body, FovCache3{fovSight, fovSmell}) =
  let radius = min (fromIntegral $ bcalm body `div` (5 * oneM)) fovSight
      creachable = PerReachable $ fullscan clearPs radius (bpos body)
      -- All non-projectile actors feel adjacent positions,
      -- even dark (for easy exploration). Projectiles rely on cameras.
      noctoRadius = if bproj body then 0 else 2
      cnocto = PerVisible $ fullscan clearPs noctoRadius (bpos body)
      -- Projectiles can potentially smell, too.
      -- TODO: until AI can handle/ignore it, only radius 2 used
      smellRadius = if fovSmell >= 2 then 2 else 0
      csmell = PerSmelled $ fullscan clearPs smellRadius (bpos body)
  in CacheBeforeLit{..}

perceptionFromPTotal :: CacheBeforeLit -> PersLight -> LevelId -> Perception
perceptionFromPTotal ptotal persLight lid =
  let litPs = persLight EM.! lid
      psight = visibleOnLevel (creachable ptotal) litPs (cnocto ptotal)
      psmell = csmell ptotal
  in Perception{..}

-- | Compute positions visible (reachable and seen) by the party.
-- A position can be directly lit by an ambient shine or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lit position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: PerReachable -> LightSources -> PerVisible -> PerVisible
visibleOnLevel PerReachable{preachable}
               LightSources{lightSources}
               (PerVisible nocto) =
  PerVisible $ nocto `ES.union` (preachable `ES.intersection` lightSources)

perceptionFromVoid :: PersLitA -> FactionId -> LevelId
                   -> (Perception, PerceptionCache)
perceptionFromVoid (persFovCache, persLight, persClear, _) fid lid =
  -- Dying actors included, to let them see their own demise.
  let bodyMap = EM.filter (\(b, _) -> bfid b == fid && blid b == lid)
                          persFovCache
      clearPs = persClear EM.! lid
      perActor = EM.map (cacheBeforeLitFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor persLight lid

-- | Calculate perception of a faction.
perLidFromFaction :: PersLitA -> FactionId -> State -> (PerLid, PerCacheLid)
perLidFromFaction persLit fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid persLit fid lid)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
perFidInDungeon :: ItemFovCache -> State -> (PersLit, PerFid, PerCacheFid)
perFidInDungeon sItemFovCache s =
  let persClear = clearInDungeon s
      persFovCache = fovCacheInDungeon sItemFovCache (sactorD s)
      addBodyToCache aid cache = (getActorBody aid s, cache)
      persFovCacheA = EM.mapWithKey addBodyToCache persFovCache
      perLitTerrain = litTerrainInDungeon s
      persLight =
        lightInDungeon perLitTerrain persFovCacheA persClear sItemFovCache s
      persLit = (persFovCache, persLight, persClear, perLitTerrain)
      persLitA = (persFovCacheA, persLight, persClear, perLitTerrain)
      f fid _ = perLidFromFaction persLitA fid s
      em = EM.mapWithKey f $ sfactionD s
  in (persLit, EM.map fst em, EM.map snd em)

clearFromLevel :: Kind.COps -> Level -> ClearPoints
clearFromLevel Kind.COps{cotile} Level{ltile} =
  PointArray.mapA (Tile.isClear cotile) ltile

clearInDungeon :: State -> PersClear
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

fovCacheInDungeon :: ItemFovCache -> ActorDict -> PersFovCache
fovCacheInDungeon sitemFovCache actorD =
  EM.map (actorFovCache3 sitemFovCache) actorD

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or floor items. Note that an actor can be blind, in which case he doesn't see
-- his own light (but others, from his or other factions, possibly do).
lightSourcesFromItems :: ClearPoints -> [(Point, Int)] -> [LightSources]
lightSourcesFromItems clearPs allItems =
  let litPos :: (Point, Int) -> LightSources
      litPos (p, light) = LightSources $ fullscan clearPs light p
  in map litPos allItems

lightOnFloor :: ItemFovCache -> Level -> [(Point, Int)]
lightOnFloor sitemFovCache lvl =
  let processIid lightAcc (iid, (k, _)) =
        let FovCache3{fovLight} =
              EM.findWithDefault emptyFovCache3 iid sitemFovCache
        in k * fovLight + lightAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
      processPos (p, bag) = (p, processBag bag 0)
  in map processPos $ EM.assocs $ lfloor lvl  -- lembed are hidden

litTerrainFromLevel :: Kind.COps -> Level -> LitTerrain
litTerrainFromLevel Kind.COps{cotile} Level{ltile} =
  let litSet p t set = if Tile.isLit cotile t then p : set else set
  in LitTerrain $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

litTerrainInDungeon :: State -> PersLitTerrain
litTerrainInDungeon s = EM.map (litTerrainFromLevel (scops s)) $ sdungeon s

-- Note that an actor can be blind,
-- in which case he doesn't see his own light
-- (but others, from his or other factions, possibly do).
litOnLevel :: ItemFovCache -> PersLitTerrain -> PersFovCacheA
           -> PersClear -> LevelId -> Level
           -> LightSources
litOnLevel sitemFovCache oldTileLight persFovCache persClear lid lvl =
  let lvlBodies = filter ((== lid) . blid . fst) $ EM.elems persFovCache
      actorLights = map (\(b, FovCache3{fovLight}) -> (bpos b, fovLight))
                        lvlBodies
      floorLights = lightOnFloor sitemFovCache lvl
      -- If there is light both on the floor and carried by actor,
      -- only the stronger light is taken into account.
      -- This is rare, so no point optimizing away the double computation.
      allLights = floorLights ++ actorLights
      litDynamic = map lightSources
                   $ lightSourcesFromItems (persClear EM.! lid) allLights
      litTiles = oldTileLight EM.! lid
  in LightSources $ ES.unions $ litTerrain litTiles : litDynamic

lightInDungeon :: PersLitTerrain -> PersFovCacheA -> PersClear -> ItemFovCache
               -> State
               -> PersLight
lightInDungeon oldTileLight persFovCache persClear sitemFovCache s =
  EM.mapWithKey
    (litOnLevel sitemFovCache oldTileLight persFovCache persClear)
    $ sdungeon s

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: ClearPoints  -- ^ the array with clear points
         -> Int          -- ^ scanning radius
         -> Point        -- ^ position of the spectator
         -> ES.EnumSet Point
fullscan clearPs radius spectatorPos =
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
  mapTr tr es1 = foldl' (flip $ ES.insert . tr) es1 $ scan (radius - 1) (isCl . tr)

  isCl :: Point -> Bool
  {-# INLINE isCl #-}
  isCl = (clearPs PointArray.!)

  -- This function is cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ Vector x y
