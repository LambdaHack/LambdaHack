{-# LANGUAGE TupleSections #-}
-- | Breadth first search and realted algorithms using the client monad.
module Game.LambdaHack.Client.BfsM
  ( invalidateBfsAid, invalidateBfsLid, invalidateBfsAll
  , getCacheBfsAndPath, getCacheBfs, getCachePath, unexploredDepth
  , closestUnknown, closestSmell, furthestKnown
  , closestTriggers, closestItems, closestFoes
#ifdef EXPOSE_INTERNAL
  , createBfs, updatePathFromBfs, condBFS
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import Data.Word

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
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
import Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)

invalidateBfsAid :: MonadClient m => ActorId -> m ()
invalidateBfsAid aid =
  modifyClient $ \cli -> cli {sbfsD = EM.insert aid BfsInvalid (sbfsD cli)}

invalidateBfsLid :: MonadClient m => LevelId -> m ()
invalidateBfsLid lid = do
  side <- getsClient sside
  let f (_, b) = blid b == lid && bfid b == side && not (bproj b)
  as <- getsState $ filter f . EM.assocs . sactorD
  mapM_ (invalidateBfsAid . fst) as

invalidateBfsAll :: MonadClient m => m ()
invalidateBfsAll =
  modifyClient $ \cli -> cli {sbfsD = EM.map (const BfsInvalid) (sbfsD cli)}

createBfs :: MonadClient m
          => Bool -> Word8 -> ActorId -> m (PointArray.Array BfsDistance)
createBfs canMove alterSkill aid = do
  b <- getsState $ getActorBody aid
  let lid = blid b
  Level{lxsize, lysize} <- getLevel lid
  let !aInitial = PointArray.replicateA lxsize lysize apartBfs
      !source = bpos b
      !_ = PointArray.unsafeWriteA aInitial source minKnownBfs
  when canMove $ do
    salter <- getsClient salter
    let !lalter = salter EM.! lid
        !_a = fillBfs lalter alterSkill source aInitial
    return ()
  return aInitial

updatePathFromBfs :: MonadClient m
                  => Bool -> BfsAndPath -> ActorId -> Point
                  -> m (PointArray.Array BfsDistance, AndPath)
updatePathFromBfs canMove bfsAndPathOld aid !target = do
  let (oldBfsArr, oldBfsPath) = case bfsAndPathOld of
        BfsAndPath{bfsArr, bfsPath} -> (bfsArr, bfsPath)
        BfsOnly{bfsArr} -> (bfsArr, EM.empty)
        BfsInvalid -> assert `failure` (bfsAndPathOld, aid, target)
  let bfsArr = oldBfsArr
  if not canMove
  then return (bfsArr, NoPath)
  else do
    b <- getsState $ getActorBody aid
    let lid = blid b
    seps <- getsClient seps
    salter <- getsClient salter
    let !lalter = salter EM.! lid
        !source = bpos b
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
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just bap@BfsAndPath{..} ->
      case EM.lookup target bfsPath of
        Nothing -> do
          (!canMove, _) <- condBFS aid
          updatePathFromBfs canMove bap aid target
        Just mpath -> return (bfsArr, mpath)
    Just bap@BfsOnly{} -> do
      (!canMove, _) <- condBFS aid
      updatePathFromBfs canMove bap aid target
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
      updatePathFromBfs canMove BfsOnly{bfsArr} aid target

-- | Get cached BFS array or, if not stored, generate and store first.
getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
getCacheBfs aid = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just BfsAndPath{bfsArr} -> return bfsArr
    Just BfsOnly{bfsArr} -> return bfsArr
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
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
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      actorMaxSk = aSkills ar
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
--
-- Note: some of these tiles are behind suspect tiles and they are chosen
-- in preference to more distant directly accessible unknown tiles.
-- This is in principle OK, but in dungeons with few hidden doors
-- AI is at a disadvantage (and with many hidden doors, it fares as well
-- as a human that deduced the dungeon properties). Changing Bfs to accomodate
-- all dungeon styles would be complex and would slow down the engine.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  bfs <- getCacheBfs aid
  let closestPoss = PointArray.minIndexesA bfs
      dist = bfs PointArray.! head closestPoss
  when (lclear lvl <= lseen lvl) $ do
    -- Some unknown may still be accessible through suspect tiles,
    -- so we return them below, but we already know the unknown (or the suspect)
    -- are not clear or not reachable, so we mark the level explored.
    -- If the level has inaccessible open areas (at least from stairs AI used)
    -- the level will nevertheless here finally marked explored,
    -- to enable transition to other levels.
    -- We should generally avoid such levels, because digging and/or trying
    -- to find other stairs leading to disconnected areas is not KISS
    -- so we don't do this in AI, so AI is at a disadvantage.
    let !_A = assert (lclear lvl >= lseen lvl) ()
    modifyClient $ \cli ->
      cli {sexplored = ES.insert (blid body) (sexplored cli)}
  if dist >= apartBfs then return Nothing
  else do
    let unknownAround pos =
          let vic = vicinityUnsafe pos
              countUnknown :: Int -> Point -> Int
              countUnknown c p = if isUknownSpace $ lvl `at` p then c + 1 else c
          in foldl' countUnknown 0 vic
        cmp = comparing unknownAround
    return $ Just $ maximumBy cmp closestPoss

-- TODO: this is costly, because target has to be changed every
-- turn when walking along trail. But inverting the sort and going
-- to the newest smell doesn't help, because smell radius is 1
-- and so only 1 smell ever visible, normally.
-- | Finds smells closest to the actor, except under the actor.
-- Of the closest, prefers the newest smell.
closestSmell :: MonadClient m => ActorId -> m [(Int, (Point, Time))]
closestSmell aid = do
  body <- getsState $ getActorBody aid
  Level{lsmell, ltime} <- getLevel $ blid body
  let smells = filter ((> ltime) . snd) $ EM.assocs lsmell
  case smells of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ts = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) smells
      return $! sortBy (comparing (fst &&& absoluteTimeNegate . snd . snd)) ts

-- TODO: We assume linear dungeon in @unexploredD@,
-- because otherwise we'd need to calculate shortest paths in a graph, etc.
-- | Closest (wrt paths) triggerable tiles.
-- The level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels.
closestTriggers :: MonadClient m => Maybe Bool -> ActorId -> m (Frequency Point)
closestTriggers onlyDir aid = do
  Kind.COps{cotile, coTileSpeedup} <- getsState scops
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      actorMaxSk = aSkills ar
  body <- getsState $ getActorBody aid
  let lid = blid body
  lvl <- getLevel lid
  let alterMinSkill p = Tile.alterMinSkill coTileSpeedup $ lvl `at` p
      alterSkill = EM.findWithDefault 0 Ability.AbAlter actorMaxSk
  explored <- getsClient sexplored
  dungeon <- getsState sdungeon
  let escape = any (not . null . lescape) $ EM.elems dungeon
  unexploredD <- unexploredDepth
  let allExplored = ES.size explored == EM.size dungeon
      -- If lid not explored, aid equips a weapon and so can leave level.
      lidExplored = ES.member (blid body) explored
      -- Causable tiles are alterable, so they are in BFS (if skill permits).
      f :: Point -> Kind.Id TileKind -> [(Int, Point)] -> [(Int, Point)]
      f p t acc =
        if Tile.hasCauses coTileSpeedup t
           && alterSkill >= fromEnum (alterMinSkill p)
        then case Tile.ascendTo cotile t of
          [] ->
            -- Escape (or guard) only after exploring, for high score, etc.
            if isNothing onlyDir && allExplored
            then (9999999, p) : acc  -- all from level congregate here
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
      triggers = PointArray.ifoldrA f [] $ ltile lvl
  bfs <- getCacheBfs aid
  -- The advantage of only targeting the tiles in vicinity of triggers is that
  -- triggers don't need to be pathable (and so AI doesn't bump into them
  -- by chance while walking elsewhere) and that many accesses to the tiles
  -- are more likely to be targeted by different AI actors (even starting
  -- from the same location), so there is less risk of clogging stairs and,
  -- OTOH, siege of stairs or escapes is more effective.
  let vicTrigger (v, p0) = map (\p -> (v, p)) $ vicinityUnsafe p0
      vicAll = concatMap vicTrigger triggers
      vicNoDist = filter (isJust . accessBfs bfs . snd) vicAll
  return $ if  -- keep lazy
    | null triggers -> mzero
    | isNothing onlyDir && not escape && allExplored ->
      -- Distance also irrelevant, to ensure random wandering.
      toFreq "closestTriggers when allExplored" vicNoDist
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
          ds = mapMaybe (\(k, p) -> mix (k, p) <$> accessBfs bfs p) vicAll
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
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      actorMaxSk = aSkills ar
  if EM.findWithDefault 0 Ability.AbMoveItem actorMaxSk <= 0 then return []
  else do
    body <- getsState $ getActorBody aid
    salter <- getsClient salter
    let lalter = salter EM.! blid body
        alterSkill = EM.findWithDefault 0 Ability.AbAlter actorMaxSk
    lvl@Level{lfloor} <- getLevel $ blid body
    let items = EM.assocs lfloor
        -- Changeable tiles are alterable so they are in BFS (if skill permits).
        -- We assume they may generate items, but the items are most useful
        -- when picked up, so @AbMoveItem@ is necessary.
        f :: Point -> Kind.Id TileKind -> [Point] -> [Point]
        f p t acc = if Tile.isChangeable coTileSpeedup t
                       && alterSkill >= fromEnum (lalter PointArray.! p)
                    then p : acc
                    else acc
        changeable = PointArray.ifoldrA f [] $ ltile lvl
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
