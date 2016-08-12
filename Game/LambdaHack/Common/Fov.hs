{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Common.Fov
  ( perceptionFromResets, perceptionFromPTotal, perFidInDungeon
  , updateFovAspectActor, updateFovLucid
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , perceptionFromPerActor, cacheBeforeLucidFromActor, visibleOnLevel
  , perceptionFromVoid, perLidFromFaction
  , fovAspectFromActor, aspectActorInDungeon
  , clearFromLevel, clearInDungeon
  , litFromLevel, litInDungeon, shineFromLevel, shineInDungeon
  , floorLightSources, lucidFromItems, lucidFromLevel, lucidInDungeon
  , fullscan
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Word

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
                     -> FovAspectActor -> FovLucidLid -> FovClearLid
                     -> LevelId -> State
                     -> (Perception, PerceptionCache)
perceptionFromResets perActor0 resetsBodies
                     fovAspectActor fovLucidLid fovClearLid lid s =
  -- Dying actors included, to let them see their own demise.
  let clearPs = fovClearLid EM.! lid
      f acc (aid, b) =
        if EM.member aid $ sactorD s
        then let fcache = fovAspectActor EM.! aid
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
cacheBeforeLucidFromActor :: FovClear -> (Actor, FovAspect) -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs (body, FovAspect{..}) =
  let radius = min (fromIntegral $ bcalm body `div` (5 * oneM)) fovSightR
      creachable = PerReachable $ fullscan clearPs radius (bpos body)
      cnocto = PerVisible $ fullscan clearPs fovNoctoR (bpos body)
      -- TODO: until AI can handle/ignore it, only radius 2 used
      smellRadius = if fovSmellR >= 2 then 2 else 0
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

perceptionFromVoid :: FovAspectActor -> FovLucidLid -> FovClearLid
                   -> FactionId -> LevelId -> State
                   -> (Perception, PerceptionCache)
perceptionFromVoid fovAspectActor fovLucidLid fovClearLid fid lid s =
  let lvlBodies = EM.fromList $ actorAssocs (== fid) lid s
      bodyMap = EM.mapWithKey (\aid b -> (b, fovAspectActor EM.! aid)) lvlBodies
      clearPs = fovClearLid EM.! lid
      perActor = EM.map (cacheBeforeLucidFromActor clearPs) bodyMap
  in perceptionFromPerActor perActor fovLucidLid lid

-- | Calculate perception of a faction.
perLidFromFaction :: FovAspectActor -> FovLucidLid -> FovClearLid
                  -> FactionId -> State
                  -> (PerLid, PerCacheLid)
perLidFromFaction fovAspectActor fovLucidLid fovClearLid fid s =
  let em = EM.mapWithKey (\lid _ -> perceptionFromVoid fovAspectActor fovLucidLid fovClearLid fid lid s)
                         (sdungeon s)
  in (EM.map fst em, EM.map snd em)

-- | Calculate the perception of the whole dungeon.
perFidInDungeon :: FovAspectItem -> State
                -> ( FovAspectActor, FovLucidLid, FovClearLid
                   , FovLitLid, FovShineLid
                   , PerFid, PerCacheFid )
perFidInDungeon sfovAspectItem s =
  let fovAspectActor = aspectActorInDungeon sfovAspectItem s
      fovClearLid = clearInDungeon s
      fovLitLid = litInDungeon s
      fovShineLid = shineInDungeon sfovAspectItem fovAspectActor s
      fovLucidLid =
        lucidInDungeon sfovAspectItem fovAspectActor fovClearLid fovLitLid s
      f fid _ = perLidFromFaction fovAspectActor fovLucidLid fovClearLid fid s
      em = EM.mapWithKey f $ sfactionD s
  in ( fovAspectActor, fovLucidLid, fovClearLid, fovLitLid, fovShineLid
     , EM.map fst em, EM.map snd em )

fovAspectFromActor :: FovAspectItem -> Actor -> FovAspect
fovAspectFromActor sfovAspectItem b =
  let processIid3 (FovAspect sightAcc smellAcc shineAcc noctoAcc) (iid, (k, _)) =
        let FovAspect{..} =
              EM.findWithDefault emptyFovAspect iid sfovAspectItem
        in FovAspect (k * fovSightR + sightAcc)
                     (k * fovSmellR + smellAcc)
                     (k * fovShineR + shineAcc)
                     (k * fovNoctoR + noctoAcc)
      processBag3 bag acc = foldl' processIid3 acc $ EM.assocs bag
      sslOrgan = processBag3 (borgan b) emptyFovAspect
  in processBag3 (beqp b) sslOrgan

aspectActorInDungeon :: FovAspectItem -> State -> FovAspectActor
aspectActorInDungeon sfovAspectItem s =
  EM.map (fovAspectFromActor sfovAspectItem) $ sactorD s

updateFovAspectActor :: FovAspectActor -> ActorId -> FovAspectItem -> State
                     -> FovAspectActor
updateFovAspectActor fovAspectActorOld aid sfovAspectItem s =
  case EM.lookup aid $ sactorD s of
    Just b -> let aspectActorNew = fovAspectFromActor sfovAspectItem b
              in EM.insert aid aspectActorNew fovAspectActorOld
    Nothing -> EM.delete aid fovAspectActorOld

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

floorLightSources :: FovAspectItem -> Level -> [(Point, Word8)]
floorLightSources sfovAspectItem lvl =
  let processIid shineAcc (iid, (k, _)) =
        let FovAspect{fovShineR} =
              EM.findWithDefault emptyFovAspect iid sfovAspectItem
        in k * fovShineR + shineAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
  in [ (p, toEnum radius)
     | (p, bag) <- EM.assocs $ lfloor lvl  -- lembed are hidden
     , let radius = processBag bag 0
     , radius > 0 ]

shineFromLevel :: FovAspectItem -> FovAspectActor -> State -> LevelId -> Level
               -> FovShine
shineFromLevel sfovAspectItem fovAspectActor s lid lvl =
  let actorLights =
        [ (bpos b, toEnum radius)
        | (aid, b) <- actorAssocs (const True) lid s
        , let radius = fovShineR $ fovAspectActor EM.! aid
        , radius > 0 ]
      floorLights = floorLightSources sfovAspectItem lvl
      -- If there is light both on the floor and carried by actor,
      -- only the stronger shine is taken into account.
      allLights = floorLights ++ actorLights
  in FovShine $ EM.fromListWith max allLights

shineInDungeon :: FovAspectItem -> FovAspectActor -> State -> FovShineLid
shineInDungeon sfovAspectItem fovAspectActor s =
  EM.mapWithKey (shineFromLevel sfovAspectItem fovAspectActor s) $ sdungeon s

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or shining floor items. Note that an actor can be blind,
-- in which case he doesn't see his own light (but others,
-- from his or other factions, possibly do).
lucidFromItems :: FovClear -> [(Point, Word8)] -> [FovLucid]
lucidFromItems clearPs allItems =
  let lucidPos :: (Point, Word8) -> FovLucid
      lucidPos (p, shine) = FovLucid $ fullscan clearPs (fromEnum shine) p
  in map lucidPos allItems

lucidFromLevel :: FovAspectItem -> FovAspectActor -> FovClearLid -> FovLitLid
               -> State -> LevelId -> Level
               -> FovLucid
lucidFromLevel sfovAspectItem fovAspectActor fovClearLid fovLitLid s lid lvl =
  let shine = shineFromLevel sfovAspectItem fovAspectActor s lid lvl
      shineLucids = lucidFromItems (fovClearLid EM.! lid)
                    $ EM.assocs $ fovShine shine
      litTiles = fovLitLid EM.! lid
  in FovLucid $ ES.unions $ fovLit litTiles : map fovLucid shineLucids

lucidInDungeon :: FovAspectItem -> FovAspectActor -> FovClearLid -> FovLitLid
               -> State
               -> FovLucidLid
lucidInDungeon sfovAspectItem fovAspectActor fovClearLid fovLitLid s =
  EM.mapWithKey
    (lucidFromLevel sfovAspectItem fovAspectActor fovClearLid fovLitLid s)
    $ sdungeon s

-- TODO: we may cache independently the sum of floor lights
-- and per-actor lights, because actor movement is the most common reset.
-- This is worthwhile for games with, e.g., lots of shining robots
-- or tracer gun bullets (the latter a bad idea, anyway, lots of resets).
updateFovLucid :: FovLucidLid -> LevelId
               -> FovAspectItem -> FovAspectActor -> FovClearLid -> FovLitLid
               -> State
               -> FovLucidLid
updateFovLucid fovLucidLidOld lid
               sfovAspectItem fovAspectActor fovClearLid fovLitLid s =
  let newLights = lucidFromLevel sfovAspectItem
                                 fovAspectActor fovClearLid fovLitLid
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
