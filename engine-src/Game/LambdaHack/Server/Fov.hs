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
  , perceptionFromPTotalNoStash, cacheBeforeLucidFromActor, shineFromLevel
  , floorLightSources, lucidFromItems, litFromLevel
  , litInDungeon, clearFromLevel, clearInDungeon, lucidInDungeon
  , perLidFromFaction, perceptionCacheFromLevel
  , Matrix, fullscan
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import qualified Data.IntSet as IS
import           GHC.Exts (inline)

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Server.FovDigital

-- * Perception cache types

data FovValid a =
    FovValid a
  | FovInvalid
  deriving (Show, Eq)

-- | Main perception validity map, for all factions.
--
-- The inner type is not a set, due to an unbenchmarked theory
-- that a constant shape map is faster.
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
perceptionFromPTotal :: FactionId -> LevelId
                     -> FovLucid -> CacheBeforeLucid -> State
                     -> Perception
perceptionFromPTotal fid lidPer fovLucid ptotal s =
  let per = perceptionFromPTotalNoStash fovLucid ptotal
  in case gstash $ sfactionD s EM.! fid of
       Just (lid, pos) | lid == lidPer ->
         per {psight = (psight per) {pvisible = ES.insert pos
                                                $ pvisible (psight per)}}
       _ -> per

perceptionFromPTotalNoStash :: FovLucid -> CacheBeforeLucid -> Perception
perceptionFromPTotalNoStash FovLucid{fovLucid} ptotal =
  let nocto = pvisible $ cnocto ptotal
      reach = preachable $ creachable ptotal
      psight = PerVisible $ nocto `ES.union` (reach `ES.intersection` fovLucid)
      psmell = csmell ptotal
  in Perception{..}

perActorFromLevel :: PerActor -> (ActorId -> Actor)
                  -> ActorMaxSkills -> FovClear
                  -> PerActor
perActorFromLevel perActorOld getActorB actorMaxSkills fovClear =
  -- Dying actors included, to let them see their own demise.
  let f _ fv@FovValid{} = fv
      f aid FovInvalid =
        let actorMaxSk = actorMaxSkills EM.! aid
            b = getActorB aid
        in FovValid $ cacheBeforeLucidFromActor fovClear b actorMaxSk
  in EM.mapWithKey f perActorOld

boundSightByCalm :: Int -> Int64 -> Int
boundSightByCalm sight calm = min (fromEnum $ calm `div` xM 5) sight

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
-- Also compute positions seen by noctovision and perceived by smell.
cacheBeforeLucidFromActor :: FovClear -> Actor -> Ability.Skills
                          -> CacheBeforeLucid
cacheBeforeLucidFromActor clearPs body actorMaxSk =
  let radius =
        boundSightByCalm (Ability.getSk Ability.SkSight actorMaxSk) (bcalm body)
      spectatorPos = bpos body
      creachable = PerReachable $ fullscan radius spectatorPos clearPs
      cnocto = PerVisible $ fullscan (Ability.getSk Ability.SkNocto actorMaxSk)
                                     spectatorPos
                                     clearPs
      smellRadius =
        if Ability.getSk Ability.SkSmell actorMaxSk >= 2 then 2 else 0
      csmell = PerSmelled $ fullscan smellRadius spectatorPos clearPs
  in CacheBeforeLucid{..}

totalFromPerActor :: PerActor -> CacheBeforeLucid
totalFromPerActor perActor =
  let fromValid = \case
        FovValid x -> x
        FovInvalid -> error $ "" `showFailure` perActor
      addCacheBeforeLucid x cbl1 =
        let cbl2 = fromValid x
        in CacheBeforeLucid
          { creachable = PerReachable
                         $ ES.union (preachable $ creachable cbl1)
                                    (preachable $ creachable cbl2)
          , cnocto = PerVisible
                     $ ES.union (pvisible $ cnocto cbl1)
                                (pvisible $ cnocto cbl2)
          , csmell = PerSmelled
                     $ ES.union (psmelled $ csmell cbl1)
                                (psmelled $ csmell cbl2)
          }
      emptyCacheBeforeLucid = CacheBeforeLucid
        { creachable = PerReachable ES.empty
        , cnocto = PerVisible ES.empty
        , csmell = PerSmelled ES.empty }
  in foldr addCacheBeforeLucid emptyCacheBeforeLucid $ EM.elems perActor

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
  -- Actors shine as if they were leaders, for speed and to prevent
  -- micromanagement by switching leader to see more.
  let actorLights =
        [ (bpos b, radius)
        | (aid, b) <- inline actorAssocs (const True) lid s
        , let radius = Ability.getSk Ability.SkShine $ getActorMaxSkills aid s
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
        let shine = IA.getSkill Ability.SkShine $ discoAspect EM.! iid
        in case compare shine 0 of
          EQ -> (accLight, accDouse)
          GT -> (max shine accLight, accDouse)
          LT -> (accLight, min shine accDouse)
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
  let lucidPos (!p, !shine) = FovLucid $ fullscan shine p clearPs
  in map lucidPos allItems

-- * Computation of initial perception and caches

-- | Calculate the perception and its caches for the whole dungeon.
perFidInDungeon :: State -> ( FovLitLid, FovClearLid, FovLucidLid
                            , PerValidFid, PerCacheFid, PerFid)
perFidInDungeon s =
  let fovLitLid = litInDungeon s
      fovClearLid = clearInDungeon s
      fovLucidLid = lucidInDungeon fovClearLid fovLitLid s
      perValidLid = EM.map (const True) (sdungeon s)
      perValidFid = EM.map (const perValidLid) (sfactionD s)
      f fid _ = perLidFromFaction fovLucidLid fovClearLid fid s
      em = EM.mapWithKey f $ sfactionD s
  in ( fovLitLid, fovClearLid, fovLucidLid
     , perValidFid, EM.map snd em, EM.map fst em)

litFromLevel :: COps -> Level -> FovLit
litFromLevel COps{coTileSpeedup} Level{ltile} =
  let litSet p t set = if Tile.isLit coTileSpeedup t then p : set else set
  in FovLit $ ES.fromDistinctAscList $ PointArray.ifoldrA' litSet [] ltile

litInDungeon :: State -> FovLitLid
litInDungeon s = EM.map (litFromLevel (scops s)) $ sdungeon s

clearFromLevel :: COps -> Level -> FovClear
clearFromLevel COps{coTileSpeedup} Level{ltile} =
  FovClear $ PointArray.mapA (Tile.isClear coTileSpeedup) ltile

clearInDungeon :: State -> FovClearLid
clearInDungeon s = EM.map (clearFromLevel (scops s)) $ sdungeon s

lucidInDungeon :: FovClearLid -> FovLitLid -> State -> FovLucidLid
lucidInDungeon fovClearLid fovLitLid s =
  EM.mapWithKey
    (\lid lvl -> FovValid $ lucidFromLevel fovClearLid fovLitLid s lid lvl)
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
      per lid pc = perceptionFromPTotal
                     fid lid (fovLucid lid) (getValid (ptotal pc)) s
  in (EM.mapWithKey per em, em)

perceptionCacheFromLevel :: FovClearLid -> FactionId -> LevelId -> State
                         -> PerceptionCache
perceptionCacheFromLevel fovClearLid fid lid s =
  let fovClear = fovClearLid EM.! lid
      lvlBodies = inline actorAssocs (== fid) lid s
      f (aid, b) =
        -- Actors see and smell as if they were leaders, for speed
        -- and to prevent micromanagement by switching leader to see more.
        let actorMaxSk = getActorMaxSkills aid s
        in if Ability.getSk Ability.SkSight actorMaxSk <= 0
              && Ability.getSk Ability.SkNocto actorMaxSk <= 0
              && Ability.getSk Ability.SkSmell actorMaxSk <= 0
           then Nothing  -- dumb missile
           else Just (aid, FovValid
                           $ cacheBeforeLucidFromActor fovClear b actorMaxSk)
      lvlCaches = mapMaybe f lvlBodies
      perActor = EM.fromDistinctAscList lvlCaches
      total = totalFromPerActor perActor
  in PerceptionCache{ptotal = FovValid total, perActor}

-- * The actual Fov algorithm

type Matrix = (Int, Int, Int, Int)

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view.
-- The actor's own position is considred in his field of view.
fullscan :: Int       -- ^ scanning radius
         -> Point     -- ^ position of the spectator
         -> FovClear  -- ^ the array with clear positions
         -> ES.EnumSet Point
fullscan !radius spectatorPos fc = case radius of
  2 -> squareUnsafeSet spectatorPos
  1 -> ES.singleton spectatorPos
  0 -> ES.empty  -- e.g., smell for non-smelling
  _ | radius <= 0 -> ES.empty
  _ ->
    let !FovClear{fovClear} = fc
        !spectatorI = fromEnum spectatorPos
        mapTr :: Matrix -> [PointI]
        mapTr m@(!_, !_, !_, !_) = scan (radius - 1) isClear (trV m)
        trV :: Matrix -> Bump -> PointI
        {-# INLINE trV #-}
        trV (x1, y1, x2, y2) B{..} =
          spectatorI + fromEnum (Vector (x1 * bx + y1 * by) (x2 * bx + y2 * by))
        isClear :: PointI -> Bool
        {-# INLINE isClear #-}
        isClear = PointArray.accessI fovClear
    in ES.intSetToEnumSet $ IS.fromList
       $ [spectatorI]
         ++ mapTr (1, 0, 0, -1)   -- quadrant I
         ++ mapTr (0, 1, 1, 0)    -- II (counter-clockwise)
         ++ mapTr (-1, 0, 0, 1)   -- III
         ++ mapTr (0, -1, -1, 0)  -- IV
