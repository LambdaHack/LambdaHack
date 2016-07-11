{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( dungeonPerception, fidLidPerception, fidLidUsingReachable
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
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector

-- | Calculate faction's perception of a level based on the lit tiles cache.
fidLidPerception :: PerActor
                 -> Either Bool [ActorId]
                 -> PersLitA -> FactionId -> LevelId
                 -> (Perception, PerceptionCache)
fidLidPerception perActor0 resetsActor (persFovCache, persLight, persClear, _)
                 fid lid =
  let bodyMap = EM.filter (\(b, _) -> bfid b == fid && blid b == lid)
                          persFovCache
      litPs = persLight EM.! lid
      clearPs = persClear EM.! lid
      -- Dying actors included, to let them see their own demise.
      ourR aid bcache =
        if either id (aid `elem`) resetsActor
        then perCacheServerFromActor clearPs bcache
        else case EM.lookup aid perActor0 of
          Just per -> per
          Nothing -> assert `failure` (aid, bcache)
      -- We don't check if any actor changed, because almost surely one is.
      -- Exception: when an actor is destroyed, but then union differs.
      perActor = EM.mapWithKey ourR bodyMap
      ptotal = CacheBeforeLit
        { creachable = PerReachable
                       $ ES.unions $ map (preachable . creachable)
                       $ EM.elems perActor
        , cnocto = PerVisible
                   $ ES.unions $ map (pvisible . cnocto)
                   $ EM.elems perActor
        , csmell = PerSmelled
                   $ ES.unions $ map (psmelled . csmell)
                   $ EM.elems perActor }
      psight = visibleOnLevel (creachable ptotal) litPs (cnocto ptotal)
      psmell = csmell ptotal
  in (Perception{..}, PerceptionCache{..})

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
perCacheServerFromActor :: PointArray.Array Bool
                        -> (Actor, FovCache3)
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

fidLidUsingReachable :: CacheBeforeLit -> PersLit -> LevelId -> Perception
fidLidUsingReachable ptotal (_, persLight, _, _) lid =
  let litPs = persLight EM.! lid
      psight = visibleOnLevel (creachable ptotal) litPs (cnocto ptotal)
      psmell = csmell ptotal
  in Perception{..}

-- | Calculate perception of a faction.
factionPerception :: PersLitA -> FactionId -> State -> (PerLid, PerCacheLid)
factionPerception persLit fid s =
  let resetsAlways = Left True
      em = EM.mapWithKey
             (\lid _ -> fidLidPerception undefined resetsAlways persLit fid lid)
             (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: EM.EnumMap ItemId FovCache3 -> State
                  -> (PersLit, PerFid, PerCacheFid)
dungeonPerception sItemFovCache s =
  let persClear = clearInDungeon s
      persFovCache = fovCacheInDungeon s sItemFovCache
      addBodyToCache aid cache = (getActorBody aid s, cache)
      persFovCacheA = EM.mapWithKey addBodyToCache persFovCache
      (persLight, persTileLight) =
        lightInDungeon Nothing persFovCacheA persClear s sItemFovCache
      persLit = (persFovCache, persLight, persClear, persTileLight)
      persLitA = (persFovCacheA, persLight, persClear, persTileLight)
      f fid _ = factionPerception persLitA fid s
      em = EM.mapWithKey f $ sfactionD s
  in (persLit, EM.map fst em, EM.map snd em)

-- | Compute positions visible (reachable and seen) by the party.
-- A position can be directly lit by an ambient shine or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lit position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: PerReachable -> ES.EnumSet Point -> PerVisible
               -> PerVisible
visibleOnLevel PerReachable{preachable} litPs (PerVisible nocto) =
  PerVisible $ nocto `ES.union` (preachable `ES.intersection` litPs)

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or floor items. Note that an actor can be blind, in which case he doesn't see
-- his own light (but others, from his or other factions, possibly do).
litByItems :: PointArray.Array Bool -> [(Point, Int)]
           -> [ES.EnumSet Point]
litByItems clearPs allItems =
  let litPos :: (Point, Int) -> ES.EnumSet Point
      litPos (p, light) = fullscan clearPs light p
  in map litPos allItems

clearInDungeon :: State -> PersClear
clearInDungeon s =
  let Kind.COps{cotile} = scops s
      clearLvl (lid, Level{ltile}) =
        let clearTiles = PointArray.mapA (Tile.isClear cotile) ltile
        in (lid, clearTiles)
  in EM.fromDistinctAscList $ map clearLvl $ EM.assocs $ sdungeon s

lightInDungeon :: Maybe PersLight -> PersFovCacheA -> PersClear -> State
               -> EM.EnumMap ItemId FovCache3
               -> (PersLight, PersLight)
lightInDungeon moldTileLight persFovCache persClear s sItemFovCache =
  let Kind.COps{cotile} = scops s
      processIid lightAcc (iid, (k, _)) =
        let FovCache3{fovLight} =
              EM.findWithDefault emptyFovCache3 iid sItemFovCache
        in k * fovLight + lightAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
      lightOnFloor :: Level -> [(Point, Int)]
      lightOnFloor lvl =
        let processPos (p, bag) = (p, processBag bag 0)
        in map processPos $ EM.assocs $ lfloor lvl  -- lembed are hidden
      -- Note that an actor can be blind,
      -- in which case he doesn't see his own light
      -- (but others, from his or other factions, possibly do).
      litOnLevel :: LevelId -> Level -> (ES.EnumSet Point, ES.EnumSet Point)
      litOnLevel lid lvl@Level{ltile} =
        let lvlBodies = filter ((== lid) . blid . fst) $ EM.elems persFovCache
            litSet set p t = if Tile.isLit cotile t then p : set else set
            litTiles = case moldTileLight of
              Nothing ->
                ES.fromDistinctAscList $ PointArray.ifoldlA litSet [] ltile
              Just oldTileLight -> oldTileLight EM.! lid
            actorLights = map (\(b, FovCache3{fovLight}) -> (bpos b, fovLight))
                              lvlBodies
            floorLights = lightOnFloor lvl
            -- If there is light both on the floor and carried by actor,
            -- only the stronger light is taken into account.
            -- This is rare, so no point optimizing away the double computation.
            allLights = floorLights ++ actorLights
            litDynamic = litByItems (persClear EM.! lid) allLights
        in (ES.unions $ litTiles : litDynamic, litTiles)
      litLvl (lid, lvl) = (lid, litOnLevel lid lvl)
      em = EM.fromDistinctAscList $ map litLvl $ EM.assocs $ sdungeon s
  in (EM.map fst em, EM.map snd em)

fovCacheInDungeon :: State -> EM.EnumMap ItemId FovCache3 -> PersFovCache
fovCacheInDungeon s sItemFovCache =
  let processIid3 (FovCache3 sightAcc smellAcc lightAcc) (iid, (k, _)) =
        let FovCache3{..} =
              EM.findWithDefault emptyFovCache3 iid sItemFovCache
        in FovCache3 (k * fovSight + sightAcc)
                     (k * fovSmell + smellAcc)
                     (k * fovLight + lightAcc)
      processBag3 bag acc = foldl' processIid3 acc $ EM.assocs bag
      processActor b =
        let sslOrgan = processBag3 (borgan b) emptyFovCache3
        in processBag3 (beqp b) sslOrgan
  in EM.map processActor $ sactorD s

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
