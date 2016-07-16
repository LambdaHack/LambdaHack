{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( perFidInDungeon, perceptionFromResets, perceptionFromPTotal
  , clearInDungeon, updateTilesClear, fovLitInDungeon, updateTilesLit
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
                     (fovAspectActor, fovLucidLid, fovClearLid, _) lid s =
  -- Dying actors included, to let them see their own demise.
  let clearPs = fovClearLid EM.! lid
      f acc (aid, b) =
        if EM.member aid $ sactorD s
        then let fcache = fovAspectActor EM.! aid
                 newPer = cacheBeforeLucidFromActor clearPs (b, fcache)
             in EM.alter (const $ Just newPer) aid acc
        else EM.delete aid acc  -- dead or stair-using actors removed
      perActor = foldl' f perActor0 resetsBodies
  in perceptionFromPerActor perActor fovLucidLid lid

perceptionFromPerActor :: PerActor -> FovLucidLid -> LevelId
                       -> (Perception, PerceptionCache)
perceptionFromPerActor perActor fovLucidLid lid =
  -- We don't check if any actor changed, because almost surely one is.
  -- Exception: when an actor is destroyed, but then union differs.
  let ptotal = CacheBeforeLucid
        { creachable = PerReachable
                       $ ES.unions $ map (preachable . creachable)
                       $ EM.elems perActor
        , cnocto = PerVisible
                   $ ES.unions $ map (pvisible . cnocto)
                   $ EM.elems perActor
        , csmell = PerSmelled
                   $ ES.unions $ map (psmelled . csmell)
                   $ EM.elems perActor }
  in (perceptionFromPTotal ptotal fovLucidLid lid, PerceptionCache{..})

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
-- Also compute positions seen by noctovision and perceived by smell.
cacheBeforeLucidFromActor :: FovClear -> (Actor, FovAspect) -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs (body, FovAspect{fovSight, fovSmell}) =
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
  in CacheBeforeLucid{..}

perceptionFromPTotal :: CacheBeforeLucid -> FovLucidLid -> LevelId -> Perception
perceptionFromPTotal ptotal fovLucidLid lid =
  let litPs = fovLucidLid EM.! lid
      psight = visibleOnLevel (creachable ptotal) litPs (cnocto ptotal)
      psmell = csmell ptotal
  in Perception{..}

-- | Compute positions visible (reachable and seen) by the party.
-- A position can be directly lit by an ambient shine or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lit position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: PerReachable -> FovLucid -> PerVisible -> PerVisible
visibleOnLevel PerReachable{preachable}
               FovLucid{fovLucid}
               (PerVisible nocto) =
  PerVisible $ nocto `ES.union` (preachable `ES.intersection` fovLucid)

perceptionFromVoid :: PersLit -> FactionId -> LevelId -> State
                   -> (Perception, PerceptionCache)
perceptionFromVoid (fovAspectActor, fovLucidLid, fovClearLid, _) fid lid s =
  let lvlBodies = EM.fromList $ actorAssocs (== fid) lid s
      bodyMap = EM.mapWithKey (\aid b -> (b, fovAspectActor EM.! aid)) lvlBodies
      clearPs = fovClearLid EM.! lid
      perActor = EM.map (cacheBeforeLucidFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor fovLucidLid lid

-- | Calculate perception of a faction.
perLidFromFaction :: PersLit -> FactionId -> State -> (PerLid, PerCacheLid)
perLidFromFaction persLit fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid persLit fid lid s)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
perFidInDungeon :: FovAspectItem -> State -> (PersLit, PerFid, PerCacheFid)
perFidInDungeon sFovAspectItem s =
  let fovClearLid = clearInDungeon s
      fovAspectActor = aspectActorInDungeon sFovAspectItem s
      perFovLit = fovLitInDungeon s
      fovLucidLid =
        lightInDungeon perFovLit fovAspectActor fovClearLid sFovAspectItem s
      persLit = (fovAspectActor, fovLucidLid, fovClearLid, perFovLit)
      f fid _ = perLidFromFaction persLit fid s
      em = EM.mapWithKey f $ sfactionD s
  in (persLit, EM.map fst em, EM.map snd em)

clearFromLevel :: Kind.COps -> Level -> FovClear
clearFromLevel Kind.COps{cotile} Level{ltile} =
  PointArray.mapA (Tile.isClear cotile) ltile

clearInDungeon :: State -> FovClearLid
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

updateTilesClear :: FovClearLid -> LevelId -> State -> FovClearLid
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
fovLucidFromItems :: FovClear -> [(Point, Int)] -> [FovLucid]
fovLucidFromItems clearPs allItems =
  let litPos :: (Point, Int) -> FovLucid
      litPos (p, light) = FovLucid $ fullscan clearPs light p
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

fovLitFromLevel :: Kind.COps -> Level -> FovLit
fovLitFromLevel Kind.COps{cotile} Level{ltile} =
  let litSet p t set = if Tile.isLit cotile t then p : set else set
  in FovLit $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

fovLitInDungeon :: State -> FovLitLid
fovLitInDungeon s = EM.map (fovLitFromLevel (scops s)) $ sdungeon s

updateTilesLit :: FovLitLid -> LevelId -> State -> FovLitLid
updateTilesLit oldTileLight lid s =
  let newTiles = fovLitFromLevel (scops s) (sdungeon s EM.! lid)
  in EM.adjust (const newTiles) lid oldTileLight

-- Note that an actor can be blind,
-- in which case he doesn't see his own light
-- (but others, from his or other factions, possibly do).
litOnLevel :: FovAspectItem -> FovLitLid -> FovAspectActor
           -> FovClearLid -> State -> LevelId -> Level
           -> FovLucid
litOnLevel sfovAspectItem oldTileLight fovAspectActor fovClearLid s lid lvl =
  let actorLights =
        [(bpos b, radius) | (aid, b) <- actorAssocs (const True) lid s
                          , let radius = fovLight $ fovAspectActor EM.! aid
                          , radius > 0]
      floorLights = lightOnFloor sfovAspectItem lvl
      -- If there is light both on the floor and carried by actor,
      -- only the stronger light is taken into account.
      -- This is rare, so no point optimizing away the double computation.
      allLights = floorLights ++ actorLights
      litDynamic = map fovLucid
                   $ fovLucidFromItems (fovClearLid EM.! lid) allLights
      litTiles = oldTileLight EM.! lid
  in FovLucid $ ES.unions $ fovLit litTiles : litDynamic

lightInDungeon :: FovLitLid -> FovAspectActor -> FovClearLid -> FovAspectItem
               -> State
               -> FovLucidLid
lightInDungeon oldTileLight fovAspectActor fovClearLid sfovAspectItem s =
  EM.mapWithKey
    (litOnLevel sfovAspectItem oldTileLight fovAspectActor fovClearLid s)
    $ sdungeon s

-- TODO: we may cache independently the sum of floor lights
-- and per-actor lights, because actor movement is the most common reset.
-- This is worthwhile for games with, e.g., lots of shining robots
-- or tracer gun bullets (the latter a bad idea, anyway, lots of resets).
updateLight :: FovLucidLid -> LevelId -> FovLitLid
            -> FovAspectActor -> FovClearLid -> FovAspectItem -> State
            -> FovLucidLid
updateLight oldLights lid oldTileLight fovAspectActor fovClearLid sfovAspectItem s =
  let newLights = litOnLevel sfovAspectItem oldTileLight fovAspectActor fovClearLid s
                             lid (sdungeon s EM.! lid)
  in EM.adjust (const newLights) lid oldLights

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: FovClear  -- ^ the array with clear points
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
