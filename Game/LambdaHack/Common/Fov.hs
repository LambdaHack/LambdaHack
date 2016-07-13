{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( dungeonPerception, perceptionFromResets, perceptionFromPTotal
  , clearInDungeon, lightInDungeon, fovCacheInDungeon
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
        then perCacheServerFromActor clearPs bcache
        else per
      only1 tbcache = EM.map (perCacheServerFromActor clearPs) tbcache
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
perCacheServerFromActor :: PointArray.Array Bool -> (Actor, FovCache3)
                        -> CacheBeforeLit
perCacheServerFromActor clearPs (body, FovCache3{fovSight, fovSmell}) =
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
visibleOnLevel :: PerReachable -> LightSources -> PerVisible
               -> PerVisible
visibleOnLevel PerReachable{preachable}
               LightSources{lightSources}
               (PerVisible nocto) =
  PerVisible $ nocto `ES.union` (preachable `ES.intersection` lightSources)

perceptionFromVoid :: PersLitA -> FactionId -> LevelId
                   -> (Perception, PerceptionCache)
perceptionFromVoid (persFovCache, persLight, persClear, _)
                   fid lid =
  -- Dying actors included, to let them see their own demise.
  let bodyMap = EM.filter (\(b, _) -> bfid b == fid && blid b == lid)
                          persFovCache
      clearPs = persClear EM.! lid
      perActor = EM.map (perCacheServerFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor persLight lid

-- | Calculate perception of a faction.
factionPerception :: PersLitA -> FactionId -> State -> (PerLid, PerCacheLid)
factionPerception persLit fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid persLit fid lid)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: ItemFovCache -> State
                  -> (PersLit, PerFid, PerCacheFid)
dungeonPerception sItemFovCache s =
  let persClear = clearInDungeon s
      persFovCache = fovCacheInDungeon sItemFovCache (sactorD s)
      addBodyToCache aid cache = (getActorBody aid s, cache)
      persFovCacheA = EM.mapWithKey addBodyToCache persFovCache
      (persLight, persTileLight) =
        lightInDungeon Nothing persFovCacheA persClear s sItemFovCache
      persLit = (persFovCache, persLight, persClear, persTileLight)
      persLitA = (persFovCacheA, persLight, persClear, persTileLight)
      f fid _ = factionPerception persLitA fid s
      em = EM.mapWithKey f $ sfactionD s
  in (persLit, EM.map fst em, EM.map snd em)

clearInDungeon :: State -> PersClear
clearInDungeon s =
  let Kind.COps{cotile} = scops s
      clearLvl (lid, Level{ltile}) =
        let clearTiles = PointArray.mapA (Tile.isClear cotile) ltile
        in (lid, clearTiles)
  in EM.fromDistinctAscList $ map clearLvl $ EM.assocs $ sdungeon s

fovCacheInDungeon :: ItemFovCache -> ActorDict -> PersFovCache
fovCacheInDungeon sitemFovCache actorD =
  EM.map (actorFovCache3 sitemFovCache) actorD

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or floor items. Note that an actor can be blind, in which case he doesn't see
-- his own light (but others, from his or other factions, possibly do).
litByItems :: PointArray.Array Bool -> [(Point, Int)]
           -> [LightSources]
litByItems clearPs allItems =
  let litPos :: (Point, Int) -> LightSources
      litPos (p, light) = LightSources $ fullscan clearPs light p
  in map litPos allItems

lightInDungeon :: Maybe PersLight -> PersFovCacheA -> PersClear -> State
               -> ItemFovCache
               -> (PersLight, PersLight)
lightInDungeon moldTileLight persFovCache persClear s sitemFovCache =
  let Kind.COps{cotile} = scops s
      processIid lightAcc (iid, (k, _)) =
        let FovCache3{fovLight} =
              EM.findWithDefault emptyFovCache3 iid sitemFovCache
        in k * fovLight + lightAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
      lightOnFloor :: Level -> [(Point, Int)]
      lightOnFloor lvl =
        let processPos (p, bag) = (p, processBag bag 0)
        in map processPos $ EM.assocs $ lfloor lvl  -- lembed are hidden
      -- Note that an actor can be blind,
      -- in which case he doesn't see his own light
      -- (but others, from his or other factions, possibly do).
      litOnLevel :: LevelId -> Level -> (LightSources, LightSources)
      litOnLevel lid lvl@Level{ltile} =
        let lvlBodies = filter ((== lid) . blid . fst) $ EM.elems persFovCache
            litSet set p t = if Tile.isLit cotile t then p : set else set
            litTiles = case moldTileLight of
              Nothing -> LightSources $ ES.fromDistinctAscList
                         $ PointArray.ifoldlA litSet [] ltile
              Just oldTileLight -> oldTileLight EM.! lid
            actorLights = map (\(b, FovCache3{fovLight}) -> (bpos b, fovLight))
                              lvlBodies
            floorLights = lightOnFloor lvl
            -- If there is light both on the floor and carried by actor,
            -- only the stronger light is taken into account.
            -- This is rare, so no point optimizing away the double computation.
            allLights = floorLights ++ actorLights
            litDynamic = map lightSources
                         $ litByItems (persClear EM.! lid) allLights
        in ( LightSources $ ES.unions $ lightSources litTiles : litDynamic
           , litTiles )
      litLvl (lid, lvl) = (lid, litOnLevel lid lvl)
      em = EM.fromDistinctAscList $ map litLvl $ EM.assocs $ sdungeon s
  in (EM.map fst em, EM.map snd em)

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: PointArray.Array Bool  -- ^ the array with clear points
         -> Int        -- ^ scanning radius
         -> Point      -- ^ position of the spectator
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
