{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( perFidInDungeon, perceptionFromResets, perceptionFromPTotal
  , clearInDungeon, updateTilesClear, litTerrainInDungeon, updateTilesLit
  , lightInDungeon, updateLight, aspectActorInDungeon, updateAspectActor
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

perceptionFromResets :: PerActor -> [(ActorId, Actor)]
                     -> PersLit -> LevelId -> State
                     -> (Perception, PerceptionCache)
perceptionFromResets perActor0 resetsBodies
                     (fovAspectActor, persLight, persClear, _) lid s =
  -- Dying actors included, to let them see their own demise.
  let clearPs = persClear EM.! lid
      f acc (aid, b) =
        if EM.member aid $ sactorD s
        then let fcache = fovAspectActor EM.! aid
                 newPer = cacheBeforeLitFromActor clearPs (b, fcache)
             in EM.alter (const $ Just newPer) aid acc
        else EM.delete aid acc  -- dead or stair-using actors removed
      perActor = foldl' f perActor0 resetsBodies
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
cacheBeforeLitFromActor :: ClearPoints -> (Actor, FovAspect) -> CacheBeforeLit
cacheBeforeLitFromActor clearPs (body, FovAspect{fovSight, fovSmell}) =
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

perceptionFromVoid :: PersLit -> FactionId -> LevelId -> State
                   -> (Perception, PerceptionCache)
perceptionFromVoid (fovAspectActor, persLight, persClear, _) fid lid s =
  let lvlBodies = EM.fromList $ actorAssocs (== fid) lid s
      bodyMap = EM.mapWithKey (\aid b -> (b, fovAspectActor EM.! aid)) lvlBodies
      clearPs = persClear EM.! lid
      perActor = EM.map (cacheBeforeLitFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor persLight lid

-- | Calculate perception of a faction.
perLidFromFaction :: PersLit -> FactionId -> State -> (PerLid, PerCacheLid)
perLidFromFaction persLit fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid persLit fid lid s)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
perFidInDungeon :: FovAspectItem -> State -> (PersLit, PerFid, PerCacheFid)
perFidInDungeon sFovAspectItem s =
  let persClear = clearInDungeon s
      fovAspectActor = aspectActorInDungeon sFovAspectItem s
      perLitTerrain = litTerrainInDungeon s
      persLight =
        lightInDungeon perLitTerrain fovAspectActor persClear sFovAspectItem s
      persLit = (fovAspectActor, persLight, persClear, perLitTerrain)
      f fid _ = perLidFromFaction persLit fid s
      em = EM.mapWithKey f $ sfactionD s
  in (persLit, EM.map fst em, EM.map snd em)

clearFromLevel :: Kind.COps -> Level -> ClearPoints
clearFromLevel Kind.COps{cotile} Level{ltile} =
  PointArray.mapA (Tile.isClear cotile) ltile

clearInDungeon :: State -> PersClear
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

updateTilesClear :: PersClear -> LevelId -> State -> PersClear
updateTilesClear oldClear lid s =
  let newTiles = clearFromLevel (scops s) (sdungeon s EM.! lid)
  in EM.adjust (const newTiles) lid oldClear

aspectActorInDungeon :: FovAspectItem -> State -> FovAspectActor
aspectActorInDungeon sfovAspectItem s =
  EM.map (actorFovAspect sfovAspectItem) $ sactorD s

updateAspectActor :: FovAspectActor -> ActorId -> FovAspectItem -> State
               -> FovAspectActor
updateAspectActor oldFC aid sfovAspectItem s =
  case EM.lookup aid $ sactorD s of
    Just b -> let newFC = actorFovAspect sfovAspectItem b
              in EM.alter (const $ Just newFC) aid oldFC
    Nothing -> EM.delete aid oldFC

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or floor items. Note that an actor can be blind, in which case he doesn't see
-- his own light (but others, from his or other factions, possibly do).
lightSourcesFromItems :: ClearPoints -> [(Point, Int)] -> [LightSources]
lightSourcesFromItems clearPs allItems =
  let litPos :: (Point, Int) -> LightSources
      litPos (p, light) = LightSources $ fullscan clearPs light p
  in map litPos allItems

lightOnFloor :: FovAspectItem -> Level -> [(Point, Int)]
lightOnFloor sfovAspectItem lvl =
  let processIid lightAcc (iid, (k, _)) =
        let FovAspect{fovLight} =
              EM.findWithDefault emptyFovAspect iid sfovAspectItem
        in k * fovLight + lightAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
  in [(p, radius) | (p, bag) <- EM.assocs $ lfloor lvl  -- lembed are hidden
                  , let radius = processBag bag 0
                  , radius > 0]

litTerrainFromLevel :: Kind.COps -> Level -> LitTerrain
litTerrainFromLevel Kind.COps{cotile} Level{ltile} =
  let litSet p t set = if Tile.isLit cotile t then p : set else set
  in LitTerrain $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

litTerrainInDungeon :: State -> PersLitTerrain
litTerrainInDungeon s = EM.map (litTerrainFromLevel (scops s)) $ sdungeon s

updateTilesLit :: PersLitTerrain -> LevelId -> State -> PersLitTerrain
updateTilesLit oldTileLight lid s =
  let newTiles = litTerrainFromLevel (scops s) (sdungeon s EM.! lid)
  in EM.adjust (const newTiles) lid oldTileLight

-- Note that an actor can be blind,
-- in which case he doesn't see his own light
-- (but others, from his or other factions, possibly do).
litOnLevel :: FovAspectItem -> PersLitTerrain -> FovAspectActor
           -> PersClear -> State -> LevelId -> Level
           -> LightSources
litOnLevel sfovAspectItem oldTileLight fovAspectActor persClear s lid lvl =
  let actorLights =
        [(bpos b, radius) | (aid, b) <- actorAssocs (const True) lid s
                          , let radius = fovLight $ fovAspectActor EM.! aid
                          , radius > 0]
      floorLights = lightOnFloor sfovAspectItem lvl
      -- If there is light both on the floor and carried by actor,
      -- only the stronger light is taken into account.
      -- This is rare, so no point optimizing away the double computation.
      allLights = floorLights ++ actorLights
      litDynamic = map lightSources
                   $ lightSourcesFromItems (persClear EM.! lid) allLights
      litTiles = oldTileLight EM.! lid
  in LightSources $ ES.unions $ litTerrain litTiles : litDynamic

lightInDungeon :: PersLitTerrain -> FovAspectActor -> PersClear -> FovAspectItem
               -> State
               -> PersLight
lightInDungeon oldTileLight fovAspectActor persClear sfovAspectItem s =
  EM.mapWithKey
    (litOnLevel sfovAspectItem oldTileLight fovAspectActor persClear s)
    $ sdungeon s

-- TODO: we may cache independently the sum of floor lights
-- and per-actor lights, because actor movement is the most common reset.
-- This is worthwhile for games with, e.g., lots of shining robots
-- or tracer gun bullets (the latter a bad idea, anyway, lots of resets).
updateLight :: PersLight -> LevelId -> PersLitTerrain
            -> FovAspectActor -> PersClear -> FovAspectItem -> State
            -> PersLight
updateLight oldLights lid oldTileLight fovAspectActor persClear sfovAspectItem s =
  let newLights = litOnLevel sfovAspectItem oldTileLight fovAspectActor persClear s
                             lid (sdungeon s EM.! lid)
  in EM.adjust (const newLights) lid oldLights

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
