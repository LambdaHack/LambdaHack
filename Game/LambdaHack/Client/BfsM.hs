{-# LANGUAGE CPP, TupleSections #-}
-- | Breadth first search and realted algorithms using the client monad.
module Game.LambdaHack.Client.BfsM
  ( invalidateBfsAid, invalidateBfsLid, invalidateBfsAll
  , getCacheBfsAndPath, getCacheBfs, getCachePath, unexploredDepth
  , closestUnknown, closestSuspect, closestSmell, furthestKnown
  , closestTriggers, closestItems, closestFoes
#ifdef EXPOSE_INTERNAL
  , createBfs, updatePathFromBfs, condBFS
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Arrow ((&&&))
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import Data.Word

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind (TileKind)

invBfs :: BfsAndPath -> BfsAndPath
invBfs bfsAnd = BfsInvalid $ bfsArr bfsAnd

invalidateBfs :: ActorId
              -> EM.EnumMap ActorId BfsAndPath
              -> EM.EnumMap ActorId BfsAndPath
invalidateBfs = EM.adjust invBfs

invalidateBfsAid :: MonadClient m => ActorId -> m ()
invalidateBfsAid aid =
  modifyClient $ \cli -> cli {sbfsD = invalidateBfs aid (sbfsD cli)}

invalidateBfsLid :: MonadClient m => LevelId -> m ()
invalidateBfsLid lid = do
  side <- getsClient sside
  ass <- getsState $ actorAssocs (== side) lid
  let as = map fst . filter (not . bproj . snd) $ ass
  mapM_ invalidateBfsAid as

invalidateBfsAll :: MonadClient m => m ()
invalidateBfsAll =
  modifyClient $ \cli -> cli {sbfsD = EM.map invBfs (sbfsD cli)}

createBfs :: MonadClient m
          => Bool -> Word8 -> Maybe BfsAndPath
          -> ActorId
          -> m (PointArray.Array BfsDistance)
createBfs canMove alterSkill mbfs aid = do
  b <- getsState $ getActorBody aid
  let lid = blid b
      source = bpos b
  lvl <- getLevel lid
  let !aInitial = case mbfs of
        Just bfsAnd ->  -- TODO: we should verify size
      -- We need to use the safe set, because previous values
      -- of the BFS array for the actor can be stuck unevaluated
      -- in thunks and we are not allowed to overwrite them.
      -- OTOH, there is now only moderate deacrease in ARR_WORDS memory
      -- and increase in speed with unsafeSetA (33.7clips/s vs 33.3 with
      -- createBfs repeated 20 times, for easier benchmarking),
      -- so it may already be in-place to some extent.
      -- but OTOH, this speedup doesn't seem to affect memory nor time at all,
      -- but it's 1.6KB per actor per move, so it's puzzling.
      -- So perhaps it's not in-place at all, but allocating and freeing
      -- such vectors is so cheap, that it all doesn't change much.
      -- Perhaps setting is much more expensive than allocating and GC.
          PointArray.safeSetA apartBfs $ bfsArr bfsAnd
        Nothing ->
          PointArray.replicateA (lxsize lvl) (lysize lvl) apartBfs
  let !_ = PointArray.unsafeWriteA aInitial source minKnownBfs
  when canMove $ do
    salter <- getsClient salter
    let !lalter = salter EM.! lid
        !_a = fillBfs lalter alterSkill source aInitial
    return ()
  return aInitial

updatePathFromBfs :: MonadClient m
                  => Bool -> BfsAndPath -> ActorId -> Point
                  -> m (PointArray.Array BfsDistance, AndPath)
updatePathFromBfs !canMove bfsAndPathOld aid target = do
  let (oldBfsArr, oldBfsPath) = case bfsAndPathOld of
        BfsAndPath{bfsArr, bfsPath} -> (bfsArr, bfsPath)
        BfsOnly{bfsArr} -> (bfsArr, EM.empty)
        BfsInvalid{} -> assert `failure` (bfsAndPathOld, aid, target)
  let bfsArr = oldBfsArr
  if not canMove
  then return (bfsArr, NoPath)
  else do
    b <- getsState $ getActorBody aid
    let lid = blid b
        source = bpos b
    seps <- getsClient seps
    salter <- getsClient salter
    let !lalter = salter EM.! lid
        !mpath = findPathBfs lalter source target seps bfsArr
        !bfsPath = EM.insert target mpath oldBfsPath
        bap = BfsAndPath{..}
    modifyClient $ \cli -> cli {sbfsD = EM.insert aid bap $ sbfsD cli}
    return (bfsArr, mpath)

-- | Get cached BFS array and path or, if not stored, generate and store first.
getCacheBfsAndPath :: forall m. MonadClient m
                   => ActorId -> Point
                   -> m (PointArray.Array BfsDistance, AndPath)
getCacheBfsAndPath aid target = do
  b <- getsState $ getActorBody aid
  let source = bpos b
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just bap@BfsAndPath{..} ->
      assert (bfsArr PointArray.! source == minKnownBfs) $
        case EM.lookup target bfsPath of
          Nothing -> do
            (!canMove, _) <- condBFS aid
            updatePathFromBfs canMove bap aid target
          Just mpath -> return (bfsArr, mpath)
    Just bap@BfsOnly{bfsArr} ->
      assert (bfsArr PointArray.! source == minKnownBfs) $ do
        (!canMove, _) <- condBFS aid
        updatePathFromBfs canMove bap aid target
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill mbfs aid
      updatePathFromBfs canMove BfsOnly{bfsArr} aid target

-- | Get cached BFS array or, if not stored, generate and store first.
getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
getCacheBfs aid = do
  b <- getsState $ getActorBody aid
  let source = bpos b
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just BfsAndPath{bfsArr} ->
      assert (bfsArr PointArray.! source == minKnownBfs) $
        return bfsArr
    Just BfsOnly{bfsArr} ->
      assert (bfsArr PointArray.! source == minKnownBfs) $
        return bfsArr
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill mbfs aid
      modifyClient $ \cli ->
        cli {sbfsD = EM.insert aid BfsOnly{bfsArr} (sbfsD cli)}
      return bfsArr

-- | Get cached BFS path or, if not stored, generate and store first.
getCachePath :: MonadClient m => ActorId -> Point -> m AndPath
getCachePath aid target = do
  b <- getsState $ getActorBody aid
  let source = bpos b
  if | source == target -> return $! AndPath source [] target 0  -- speedup
     | otherwise -> snd <$> getCacheBfsAndPath aid target

condBFS :: MonadClient m => ActorId -> m (Bool, Word8)
condBFS aid = do
  side <- getsClient sside
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway). Otherwise we'd have
  -- to reset BFS after leader changes, but it would still lead to
  -- wasted movement if, e.g., non-leaders move but only leaders open doors
  -- and leader change is very rare.
  activeItems <- activeItemsClient aid
  let actorMaxSk = sumSkills activeItems
      alterSkill =
        min (maxBound - 1)  -- @maxBound :: Word8@ means unalterable
            (toEnum $ EM.findWithDefault 0 Ability.AbAlter actorMaxSk)
      canMove = EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
                || EM.findWithDefault 0 Ability.AbDisplace actorMaxSk > 0
                -- TODO: needed for now, because AI targets and shoots enemies
                -- based on the path to them, not LOS to them.
                || EM.findWithDefault 0 Ability.AbProject actorMaxSk > 0
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      enterSuspect = smarkSuspect || underAI
      skill | enterSuspect = alterSkill  -- dig and search at will
            | otherwise = 0  -- only walkable tiles
  return (canMove, skill)

-- | Furthest (wrt paths) known position.
furthestKnown :: MonadClient m => ActorId -> m Point
furthestKnown aid = do
  bfs <- getCacheBfs aid
  getMaxIndex <- rndToAction $ oneOf [ PointArray.maxIndexA
                                     , PointArray.maxLastIndexA ]
  let furthestPos = getMaxIndex bfs
      dist = bfs PointArray.! furthestPos
  return $! assert (dist > apartBfs `blame` (aid, furthestPos, dist))
                   furthestPos

-- | Closest reachable unknown tile position, if any.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  body <- getsState $ getActorBody aid
  lvl@Level{lxsize, lysize} <- getLevel $ blid body
  bfs <- getCacheBfs aid
  let closestPoss = PointArray.minIndexesA bfs
      dist = bfs PointArray.! head closestPoss
  if dist >= apartBfs then do
    when (lclear lvl == lseen lvl) $ do  -- explored fully, mark it once for all
      let !_A = assert (lclear lvl >= lseen lvl) ()
      modifyClient $ \cli ->
        cli {sexplored = ES.insert (blid body) (sexplored cli)}
    return Nothing
  else do
    let unknownAround p =
          let vic = vicinity lxsize lysize p
              posUnknown pos = bfs PointArray.! pos < apartBfs
              vicUnknown = filter posUnknown vic
          in length vicUnknown
        cmp = comparing unknownAround
    return $ Just $ maximumBy cmp closestPoss

-- TODO: this is costly, because target has to be changed every
-- turn when walking along trail. But inverting the sort and going
-- to the newest smell, while sometimes faster, may result in many
-- actors following the same trail, unless we wipe the trail as soon
-- as target is assigned (but then we don't know if we should keep the target
-- or not, because somebody already followed it). OTOH, trails are not
-- common and so if wiped they can't incur a large total cost.
-- TODO: remove targets where the smell is likely to get too old by the time
-- the actor gets there.
-- | Finds smells closest to the actor, except under the actor.
closestSmell :: MonadClient m => ActorId -> m [(Int, (Point, Tile.SmellTime))]
closestSmell aid = do
  body <- getsState $ getActorBody aid
  Level{lsmell, ltime} <- getLevel $ blid body
  let smells = filter ((> ltime) . snd) $ EM.assocs lsmell
  case smells of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ts = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) smells
          ds = filter (\(d, _) -> d /= 0) ts  -- bpos of aid
      return $! sortBy (comparing (fst &&& absoluteTimeNegate . snd . snd)) ds

-- | Closest (wrt paths) suspect tile.
closestSuspect :: MonadClient m => ActorId -> m [Point]
closestSuspect aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  let f :: [Point] -> Point -> Kind.Id TileKind -> [Point]
      f acc p t = if Tile.isSuspect coTileSpeedup t then p : acc else acc
      suspect = PointArray.ifoldlA' f [] $ ltile lvl
  case suspect of
    [] -> do
      -- We assume @closestSuspect@ is called only when there are no more
      -- pathable unknown tiles on the map.
      -- If the level has inaccessible open areas (at least from some stairs)
      -- here finally mark it explored, to enable transition to other levels.
      -- We should generally avoid such levels, because digging and/or trying
      -- to find other stairs leading to disconnected areas is not KISS
      -- so we don't do this in AI, so AI is at a disadvantage.
      modifyClient $ \cli ->
        cli {sexplored = ES.insert (blid body) (sexplored cli)}
      return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\p -> fmap (,p) (accessBfs bfs p)) suspect
      return $! map snd $ sortBy (comparing fst) ds

-- TODO: We assume linear dungeon in @unexploredD@,
-- because otherwise we'd need to calculate shortest paths in a graph, etc.
-- | Closest (wrt paths) triggerable open tiles.
-- The level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels.
closestTriggers :: MonadClient m => Maybe Bool -> ActorId -> m (Frequency Point)
closestTriggers onlyDir aid = do
  Kind.COps{cotile, coTileSpeedup} <- getsState scops
  body <- getsState $ getActorBody aid
  explored <- getsClient sexplored
  let lid = blid body
  lvl <- getLevel lid
  dungeon <- getsState sdungeon
  let escape = any (not . null . lescape) $ EM.elems dungeon
  unexploredD <- unexploredDepth
  let allExplored = ES.size explored == EM.size dungeon
      -- If lid not explored, aid equips a weapon and so can leave level.
      lidExplored = ES.member (blid body) explored
      f :: [(Int, Point)] -> Point -> Kind.Id TileKind -> [(Int, Point)]
      f acc p t =
        if Tile.isWalkable coTileSpeedup t && not (null $ Tile.causeEffects cotile t)
        then case Tile.ascendTo cotile t of
          [] ->
            -- Escape (or guard) only after exploring, for high score, etc.
            if isNothing onlyDir && allExplored
            then (9999999, p) : acc  -- all from that level congregate here
            else acc
          l ->
            if not escape && allExplored
            -- Direction irrelevant; wander randomly.
            then map (,p) l ++ acc
            else let g k =
                       let easier = signum k /= signum (fromEnum lid)
                           unexpForth = unexploredD (signum k) lid
                           unexpBack = unexploredD (- signum k) lid
                           aiCond = if unexpForth
                                    then easier
                                         || not unexpBack && lidExplored
                                    else not unexpBack && lidExplored
                                         && null (lescape lvl)
                       in maybe aiCond (\d -> d == (k > 0)) onlyDir
                 in map (,p) (filter g l) ++ acc
        else acc
      triggersAll = PointArray.ifoldlA' f [] $ ltile lvl
      -- Don't target stairs under the actor. Most of the time they
      -- are blocked and stay so, so we seek other stairs, if any.
      -- If no other stairs in this direction, let's wait here,
      -- unless the actor has just returned via the very stairs.
      triggers = filter ((/= bpos body) . snd) triggersAll
  bfs <- getCacheBfs aid
  return $ if  -- keep lazy
    | null triggers -> mzero
    | isNothing onlyDir && not escape && allExplored ->
      -- Distance also irrelevant, to ensure random wandering.
      toFreq "closestTriggers when allExplored" triggers
    | otherwise ->
      -- Prefer stairs to easier levels.
      -- If exactly one escape, these stairs will all be in one direction.
      let mix (k, p) dist =
            let easier = signum k /= signum (fromEnum lid)
                depthDelta = if easier then 2 else 1
                maxd = fromEnum (maxBound :: BfsDistance)
                       - fromEnum apartBfs
                v = (maxd * maxd * maxd) `div` ((dist + 1) * (dist + 1))
            in (depthDelta * v, p)
          ds = mapMaybe (\(k, p) -> mix (k, p) <$> accessBfs bfs p) triggers
      in toFreq "closestTriggers" ds

unexploredDepth :: MonadClient m => m (Int -> LevelId -> Bool)
unexploredDepth = do
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  let allExplored = ES.size explored == EM.size dungeon
      unexploredD p =
        let unex lid = allExplored
                       && not (null $ lescape $ dungeon EM.! lid)
                       || ES.notMember lid explored
                       || unexploredD p lid
        in any unex . ascendInBranch dungeon p
  return unexploredD

-- | Closest (wrt paths) items and changeable tiles (e.g., item caches).
closestItems :: MonadClient m => ActorId -> m [(Int, (Point, Maybe ItemBag))]
closestItems aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  body <- getsState $ getActorBody aid
  lvl@Level{lfloor} <- getLevel $ blid body
  let items = EM.assocs lfloor
      f :: [Point] -> Point -> Kind.Id TileKind -> [Point]
      f acc p t = if Tile.isChangeable coTileSpeedup t then p : acc else acc
      changeable = PointArray.ifoldlA' f [] $ ltile lvl
  if null items && null changeable then return []
  else do
    bfs <- getCacheBfs aid
    let is = mapMaybe (\(p, bag) ->
                        fmap (, (p, Just bag)) (accessBfs bfs p)) items
        cs = mapMaybe (\p ->
                        fmap (, (p, Nothing)) (accessBfs bfs p)) changeable
    return $! sortBy (comparing fst) $ is ++ cs

-- | Closest (wrt paths) enemy actors.
closestFoes :: MonadClient m
            => [(ActorId, Actor)] -> ActorId -> m [(Int, (ActorId, Actor))]
closestFoes foes aid =
  case foes of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(_, b) -> fmap (,x) (accessBfs bfs (bpos b))) foes
      return $! sortBy (comparing fst) ds
