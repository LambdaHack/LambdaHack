{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( perceptionFromResets, perceptionFromPTotal, perFidInDungeon
  , updateFovLucid, fovAspectFromActor
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , perceptionFromPerActor, cacheBeforeLucidFromActor, visibleOnLevel
  , perceptionFromVoid, perLidFromFaction
  , actorAspectInDungeon
  , clearFromLevel, clearInDungeon
  , litFromLevel, litInDungeon, shineFromLevel
  , floorLightSources, lucidFromItems, lucidFromLevel, lucidInDungeon
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

perceptionFromResets :: PerActor -> [(ActorId, Actor)]
                     -> ActorAspect -> FovLucidLid -> FovClearLid
                     -> LevelId -> State
                     -> (Perception, PerceptionCache)
perceptionFromResets perActor0 resetsBodies
                     actorAspect fovLucidLid fovClearLid lid s =
  -- Dying actors included, to let them see their own demise.
  let clearPs = fovClearLid EM.! lid
      f acc (aid, b) =
        if EM.member aid $ sactorD s
        then let fcache = actorAspect EM.! aid
                 newPer = cacheBeforeLucidFromActor clearPs (b, fcache)
             in EM.insert aid newPer acc
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
cacheBeforeLucidFromActor :: FovClear -> (Actor, AspectRecord) -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs (body, AspectRecord{..}) =
  let radius = min (fromIntegral $ bcalm body `div` (5 * oneM)) aSight
      creachable = PerReachable $ fullscan clearPs radius (bpos body)
      cnocto = PerVisible $ fullscan clearPs aNocto (bpos body)
      -- TODO: until AI can handle/ignore it, only radius 2 used
      smellRadius = if aSmell >= 2 then 2 else 0
      csmell = PerSmelled $ fullscan clearPs smellRadius (bpos body)
  in CacheBeforeLucid{..}

perceptionFromPTotal :: CacheBeforeLucid -> FovLucidLid -> LevelId -> Perception
perceptionFromPTotal ptotal fovLucidLid lid =
  let lucidPs = fovLucidLid EM.! lid
      psight = visibleOnLevel (creachable ptotal) lucidPs (cnocto ptotal)
      psmell = csmell ptotal
  in Perception{..}

-- | Compute positions visible (reachable and seen) by the party.
-- A position is lucid, if it's lit by an ambient light or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lucid position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: PerReachable -> FovLucid -> PerVisible -> PerVisible
visibleOnLevel PerReachable{preachable}
               FovLucid{fovLucid}
               (PerVisible nocto) =
  PerVisible $ nocto `ES.union` (preachable `ES.intersection` fovLucid)

perceptionFromVoid :: ActorAspect -> FovLucidLid -> FovClearLid
                   -> FactionId -> LevelId -> State
                   -> (Perception, PerceptionCache)
perceptionFromVoid actorAspect fovLucidLid fovClearLid fid lid s =
  let lvlBodies = EM.fromList $ actorAssocs (== fid) lid s
      bodyMap = EM.mapWithKey (\aid b -> (b, actorAspect EM.! aid)) lvlBodies
      clearPs = fovClearLid EM.! lid
      perActor = EM.map (cacheBeforeLucidFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor fovLucidLid lid

-- | Calculate perception of a faction.
perLidFromFaction :: ActorAspect -> FovLucidLid -> FovClearLid
                  -> FactionId -> State
                  -> (PerLid, PerCacheLid)
perLidFromFaction actorAspect fovLucidLid fovClearLid fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid actorAspect fovLucidLid fovClearLid fid lid s)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
perFidInDungeon :: DiscoveryAspect -> State
                -> ( ActorAspect, FovLucidLid, FovClearLid
                   , FovLitLid, PerFid, PerCacheFid )
perFidInDungeon discoAspect s =
  let actorAspect = actorAspectInDungeon discoAspect s
      fovClearLid = clearInDungeon s
      fovLitLid = litInDungeon s
      fovLucidLid =
        lucidInDungeon discoAspect actorAspect fovClearLid fovLitLid s
      f fid _ = perLidFromFaction actorAspect fovLucidLid fovClearLid fid s
      em = EM.mapWithKey f $ sfactionD s
  in ( actorAspect, fovLucidLid, fovClearLid, fovLitLid
     , EM.map fst em, EM.map snd em )

fovAspectFromActor :: DiscoveryAspect -> Actor -> AspectRecord
fovAspectFromActor discoAspect b =
  let processIid (iid, (k, _)) = (discoAspect EM.! iid, k)
      processBag ass = sumAspectRecord $ map processIid ass
  in processBag $ EM.assocs (borgan b) ++ EM.assocs (beqp b)

actorAspectInDungeon :: DiscoveryAspect -> State -> ActorAspect
actorAspectInDungeon discoAspect s =
  EM.map (fovAspectFromActor discoAspect) $ sactorD s

clearFromLevel :: Kind.COps -> Level -> FovClear
clearFromLevel Kind.COps{coTileSpeedup} Level{ltile} =
  PointArray.mapA (Tile.isClear coTileSpeedup) ltile

clearInDungeon :: State -> FovClearLid
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

litFromLevel :: Kind.COps -> Level -> FovLit
litFromLevel Kind.COps{coTileSpeedup} Level{ltile} =
  let litSet p t set = if Tile.isLit coTileSpeedup t then p : set else set
  in FovLit $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

litInDungeon :: State -> FovLitLid
litInDungeon s = EM.map (litFromLevel (scops s)) $ sdungeon s

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
      -- (or several projectile actors), its radius is the sum total.
  in FovShine $ EM.fromListWith (+) allLights

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or shining floor items. Note that an actor can be blind,
-- in which case he doesn't see his own light (but others,
-- from his or other factions, possibly do).
lucidFromItems :: FovClear -> [(Point, Int)] -> [FovLucid]
lucidFromItems clearPs allItems =
  let lucidPos (p, shine) = FovLucid $ fullscan clearPs shine p
  in map lucidPos allItems

lucidFromLevel :: DiscoveryAspect -> ActorAspect -> FovClearLid -> FovLitLid
               -> State -> LevelId -> Level
               -> FovLucid
lucidFromLevel discoAspect actorAspect fovClearLid fovLitLid s lid lvl =
  let FovShine shine = shineFromLevel discoAspect actorAspect s lid lvl
      shineLucids = lucidFromItems (fovClearLid EM.! lid) $ EM.assocs shine
      litTiles = fovLitLid EM.! lid
  in FovLucid $ ES.unions $ fovLit litTiles : map fovLucid shineLucids

lucidInDungeon :: DiscoveryAspect -> ActorAspect -> FovClearLid -> FovLitLid
               -> State
               -> FovLucidLid
lucidInDungeon discoAspect actorAspect fovClearLid fovLitLid s =
  EM.mapWithKey
    (lucidFromLevel discoAspect actorAspect fovClearLid fovLitLid s)
    $ sdungeon s

updateFovLucid :: FovLucidLid -> LevelId
               -> DiscoveryAspect -> ActorAspect -> FovClearLid -> FovLitLid
               -> State
               -> FovLucidLid
updateFovLucid fovLucidLidOld lid
               discoAspect actorAspect fovClearLid fovLitLid s =
  let newLights = lucidFromLevel discoAspect
                                 actorAspect fovClearLid fovLitLid
                                 s lid (sdungeon s EM.! lid)
  in EM.adjust (const newLights) lid fovLucidLidOld

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: FovClear  -- ^ the array with clear points
         -> Int       -- ^ scanning radius
         -> Point     -- ^ position of the spectator
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
  mapTr tr es1 =
    foldl' (flip $ ES.insert . tr) es1 $ scan (radius - 1) (isCl . tr)

  isCl :: Point -> Bool
  {-# INLINE isCl #-}
  isCl = (clearPs PointArray.!)

  -- This function is cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ Vector x y
