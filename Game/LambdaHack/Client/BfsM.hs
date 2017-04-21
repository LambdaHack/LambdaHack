{-# LANGUAGE TupleSections #-}
-- | Breadth first search and realted algorithms using the client monad.
module Game.LambdaHack.Client.BfsM
  ( invalidateBfsAid, invalidateBfsLid, invalidateBfsAll
  , createBfs, condBFS, getCacheBfsAndPath, getCacheBfs
  , getCachePath, createPath
  , unexploredDepth
  , closestUnknown, closestSmell, furthestKnown
  , FleeViaStairsOrEscape(..), embedBenefit, closestTriggers
  , closestItems, closestFoes
  , condEnoughGearM
#ifdef EXPOSE_INTERNAL
  , updatePathFromBfs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (isUknownSpace)

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
  Kind.COps{coTileSpeedup} <- getsState scops
  let (oldBfsArr, oldBfsPath) = case bfsAndPathOld of
        BfsAndPath{bfsArr, bfsPath} -> (bfsArr, bfsPath)
        BfsInvalid -> assert `failure` (bfsAndPathOld, aid, target)
  let bfsArr = oldBfsArr
  if not canMove
  then return (bfsArr, NoPath)
  else do
    b <- getsState $ getActorBody aid
    let lid = blid b
    seps <- getsClient seps
    salter <- getsClient salter
    lvl <- getLevel lid
    let !lalter = salter EM.! lid
        fovLit p = Tile.isLit coTileSpeedup $ lvl `at` p
        !source = bpos b
        !mpath = findPathBfs lalter fovLit source target seps bfsArr
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
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
      let bfsPath = EM.empty
      updatePathFromBfs canMove BfsAndPath{..} aid target

-- | Get cached BFS array or, if not stored, generate and store first.
getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
getCacheBfs aid = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just BfsAndPath{bfsArr} -> return bfsArr
    _ -> do
      (!canMove, !alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
      let bfsPath = EM.empty
      modifyClient $ \cli ->
        cli {sbfsD = EM.insert aid BfsAndPath{..} (sbfsD cli)}
      return bfsArr

-- | Get cached BFS path or, if not stored, generate and store first.
getCachePath :: MonadClient m => ActorId -> Point -> m AndPath
getCachePath aid target = do
  b <- getsState $ getActorBody aid
  let source = bpos b
  if | source == target -> return $! AndPath [] target 0  -- speedup
     | otherwise -> snd <$> getCacheBfsAndPath aid target

createPath :: MonadClient m => ActorId -> Target -> m TgtAndPath
createPath aid tapTgt = do
  Kind.COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let stopAtUnwalkable tapPath@AndPath{..} =
        let (walkable, rest) =
              -- Unknown tiles are not walkable, so path stops before them,
              -- which is good, because by the time actor reaches them,
              -- they are no longer unknown, so target invalidated.
              span (Tile.isWalkable coTileSpeedup . at lvl) pathList
        in case rest of
          [] -> TgtAndPath{..}
          [g] | g == pathGoal -> TgtAndPath{..}
          newGoal : _ ->
            let newTgt = TPoint TKnown (blid b) newGoal
                newPath = AndPath{ pathList = walkable ++ [newGoal]
                                 , pathGoal = newGoal
                                 , pathLen = length walkable + 1 }
            in TgtAndPath{tapTgt = newTgt, tapPath = newPath}
      stopAtUnwalkable tapPath@NoPath = TgtAndPath{..}
  mpos <- aidTgtToPos aid (blid b) tapTgt
  case mpos of
    Nothing -> return TgtAndPath{tapTgt, tapPath=NoPath}
    Just p -> do
      path <- getCachePath aid p
      return $! stopAtUnwalkable path

condBFS :: MonadClient m => ActorId -> m (Bool, Word8)
condBFS aid = do
  side <- getsClient sside
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway). Otherwise we'd have
  -- to reset BFS after leader changes, but it would still lead to
  -- wasted movement if, e.g., non-leaders move but only leaders open doors
  -- and leader change is very rare.
  actorMaxSk <- maxActorSkillsClient aid
  let alterSkill =
        min (maxBound - 1)  -- @maxBound :: Word8@ means unalterable
            (toEnum $ EM.findWithDefault 0 Ability.AbAlter actorMaxSk)
      canMove = EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
                || EM.findWithDefault 0 Ability.AbDisplace actorMaxSk > 0
                || EM.findWithDefault 0 Ability.AbProject actorMaxSk > 0
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
      -- Under UI, playing a hero party, we let AI set our target each
      -- turn for henchmen that can't move and can't alter, usually to TUnknown.
      -- This is rather useless, but correct.
      enterSuspect = smarkSuspect > 0 || underAI
      skill | enterSuspect = alterSkill  -- dig and search at will
            | otherwise = 1  -- only walkable tiles and unknown
  return (canMove, skill)  -- keep it lazy

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
--
-- If the level has inaccessible open areas (at least from the stairs AI used)
-- the level will be nevertheless here finally marked explored,
-- to enable transition to other levels.
-- We should generally avoid such levels, because digging and/or trying
-- to find other stairs leading to disconnected areas is not KISS
-- so we don't do this in AI, so AI is at a disadvantage.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  bfs <- getCacheBfs aid
  let closestPoss = PointArray.minIndexesA bfs
      dist = bfs PointArray.! head closestPoss
      !_A = assert (lclear lvl >= lseen lvl) ()
  if lclear lvl <= lseen lvl
       -- Some unknown may still be visible and even pathable, but we already
       -- know from global level info that they are blocked.
     || dist >= apartBfs
       -- Global level info may tell us that terrain was changed and so
       -- some new explorable tile appeared, but we don't care about those
       -- and we know we already explored all initially seen unknown tiles
       -- and it's enough for us (otherwise we'd need to hunt all around
       -- the map for tiles altered by enemies).
  then do
    modifyClient $ \cli ->
      cli {sexplored = ES.insert (blid body) (sexplored cli)}
    return Nothing
  else do
    let unknownAround pos =
          let vic = vicinityUnsafe pos
              countUnknown :: Int -> Point -> Int
              countUnknown c p = if isUknownSpace $ lvl `at` p then c + 1 else c
          in foldl' countUnknown 0 vic
        cmp = comparing unknownAround
    return $ Just $ maximumBy cmp closestPoss

-- | Finds smells closest to the actor, except under the actor,
-- because actors consume smell only moving over them, not standing.
-- Of the closest, prefers the newest smell.
closestSmell :: MonadClient m => ActorId -> m [(Int, (Point, Time))]
closestSmell aid = do
  body <- getsState $ getActorBody aid
  Level{lsmell, ltime} <- getLevel $ blid body
  let smells = filter (\(p, sm) -> sm > ltime && p /= bpos body)
                      (EM.assocs lsmell)
  case smells of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ts = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) smells
      return $! sortBy (comparing (fst &&& absoluteTimeNegate . snd . snd)) ts

data FleeViaStairsOrEscape =
  ViaStairs | ViaStairsUp | ViaStairsDown | ViaEscape | ViaNothing | ViaAnything
  deriving (Show, Eq)

embedBenefit :: MonadClient m
             => FleeViaStairsOrEscape -> ActorId
             -> [(Point, ItemBag)]
             -> m [(Int, (Point, ItemBag))]
embedBenefit fleeVia aid pbags = do
  Kind.COps{coitem=Kind.Ops{okind}, coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  actorSk <- if fleeVia == ViaAnything  -- targeting, e.g., when not a leader
             then maxActorSkillsClient aid
             else currentSkillsClient aid
  let alterSkill = EM.findWithDefault 0 Ability.AbAlter actorSk
  fact <- getsState $ (EM.! bfid b) . sfactionD
  lvl <- getLevel (blid b)
  unexploredTrue <- unexploredDepth True (blid b)
  unexploredFalse <- unexploredDepth False (blid b)
  condEnoughGear <- condEnoughGearM aid
  discoKind <- getsClient sdiscoKind
  discoBenefit <- getsClient sdiscoBenefit
  s <- getState
  let aiAlterMinSkill p = Tile.aiAlterMinSkill coTileSpeedup $ lvl `at` p
      lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      -- Ignoring the number of items, because only one of each @iid@
      -- is triggered at the same time, others are left to be used later on.
      iidToEffs iid = case EM.lookup (jkindIx $ getItemBody iid s) discoKind of
        Nothing -> []
        Just KindMean{kmKind} -> IK.ieffects $ okind kmKind
      isEffEscapeOrAscend IK.Ascend{} = True
      isEffEscapeOrAscend IK.Escape{} = True
      isEffEscapeOrAscend _ = False
      feats bag = concatMap iidToEffs $ EM.keys bag
      -- For simplicity, we assume at most one exit at each position.
      -- AI uses exit regardless of traps or treasures at the spot.
      bens (_, bag) = case find isEffEscapeOrAscend $ feats bag of
        Just IK.Escape{} ->
          -- Escape (or guard) only after exploring, for high score, etc.
          let escapeOrGuard = fcanEscape (gplayer fact)
                              || fleeVia == ViaAnything  -- targeting to guard
          in if fleeVia `elem` [ViaEscape, ViaAnything]
                && escapeOrGuard
                && allExplored
             then 10000
             else 0  -- don't escape prematurely
        Just (IK.Ascend up) ->  -- change levels sensibly, in teams
          let easier = up /= (fromEnum (blid b) > 0)
              unexpForth = if up then unexploredTrue else unexploredFalse
              unexpBack = if not up then unexploredTrue else unexploredFalse
              -- Forbid loops via peeking at unexplored and getting back.
              aiCond = if unexpForth
                       then easier && condEnoughGear
                            || (not unexpBack || easier) && lidExplored
                       else easier && allExplored && null (lescape lvl)
              -- Prefer one direction of stairs, to team up
              -- and prefer embed (may, e.g.,  create loot) over stairs.
              v = if aiCond then if easier then 10 else 1 else 0
          in case fleeVia of
            ViaStairsUp | up -> 1
            ViaStairsDown | not up -> 1
            ViaStairs -> v
            ViaAnything -> v
            _ -> 0  -- don't ascend prematurely
        _ ->
          if fleeVia `elem` [ViaNothing, ViaAnything]

          then -- Actor uses the embedded item on himself, hence @effApply@.
               sum $ mapMaybe (\iid -> benApply <$> EM.lookup iid discoBenefit)
                              (EM.keys bag)
          else 0
      interestingHere p =
        -- Assume secrets have no loot, unless marked as considered by AI:
        Tile.consideredByAI coTileSpeedup (lvl `at` p)
        -- Only actors with high enough AbAlter can trigger embedded items.
        && alterSkill >= fromEnum (aiAlterMinSkill p)
      ebags = filter (interestingHere . fst) pbags
      benFeats = map (\pbag -> (bens pbag, pbag)) ebags
  return $! filter ((> 0 ) . fst) benFeats

-- | Closest (wrt paths) AI-triggerable tiles with embedded items.
-- In AI, the level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels, but before that, he triggers other tiles
-- in hope of some loot or beneficial effect to enter next level with.
closestTriggers :: MonadClient m => FleeViaStairsOrEscape -> ActorId
                -> m [(Int, (Point, (Point, ItemBag)))]
closestTriggers fleeVia aid = do
  b <- getsState $ getActorBody aid
  lvl <- getLevel (blid b)
  let pbags = EM.assocs $ lembed lvl
  efeat <- embedBenefit fleeVia aid pbags
  -- The advantage of targeting the tiles in vicinity of triggers is that
  -- triggers don't need to be pathable (and so AI doesn't bump into them
  -- by chance while walking elsewhere) and that many accesses to the tiles
  -- are more likely to be targeted by different AI actors (even starting
  -- from the same location), so there is less risk of clogging stairs and,
  -- OTOH, siege of stairs or escapes is more effective.
  bfs <- getCacheBfs aid
  let vicTrigger (cid, (p0, bag)) =
        map (\p -> (cid, (p, (p0, bag)))) $ vicinityUnsafe p0
      vicAll = concatMap vicTrigger efeat
  return $  -- keep lazy
    let mix (benefit, ppbag) dist =
          let maxd = fromEnum (maxBound :: BfsDistance)
                     - fromEnum apartBfs
              v | dist == 0 = maxd * maxd * 1000
                    -- if already adjacent, usually keep, but fuzz, to guard
                | otherwise = (maxd * maxd * 100) `div` (dist * dist)
          in (benefit * v, ppbag)
    in mapMaybe (\bpp@(_, (p, _)) ->
         mix bpp <$> accessBfs bfs p) vicAll

-- | Check whether the actor has enough gear to go look for enemies.
-- We assume weapons in equipment are better than any among organs
-- or at least provide some essential diversity.
-- Disabled if, due to tactic, actors follow leader and so would
-- repeatedly move towards and away form stairs at leader change,
-- depending on current leader's gear.
-- Number of items of a single kind is ignored, because variety is needed.
condEnoughGearM :: MonadClient m => ActorId -> m Bool
condEnoughGearM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let followTactic = ftactic (gplayer fact) `elem` [TFollow, TFollowNoItems]
  eqpAssocs <- getsState $ getActorAssocs aid CEqp
  invAssocs <- getsState $ getActorAssocs aid CInv
  return $ not followTactic  -- keep it lazy
           && (any (isMelee . snd) eqpAssocs
               || length (eqpAssocs ++ invAssocs) >= 5)

unexploredDepth :: MonadClient m => Bool -> LevelId -> m Bool
unexploredDepth !up !lidCurrent = do
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  let allExplored = ES.size explored == EM.size dungeon
      unexploredD =
        let unex !lid = allExplored
                        && not (null $ lescape $ dungeon EM.! lid)
                        || ES.notMember lid explored
                        || unexploredD lid
        in any unex . ascendInBranch dungeon up
  return $ unexploredD lidCurrent  -- keep it lazy

-- | Closest (wrt paths) items.
closestItems :: MonadClient m => ActorId -> m [(Int, (Point, ItemBag))]
closestItems aid = do
  actorMaxSk <- maxActorSkillsClient aid
  if EM.findWithDefault 0 Ability.AbMoveItem actorMaxSk <= 0 then return []
  else do
    body <- getsState $ getActorBody aid
    Level{lfloor} <- getLevel $ blid body
    if EM.null lfloor then return [] else do
      bfs <- getCacheBfs aid
      let mix pbag dist =
            let maxd = fromEnum (maxBound :: BfsDistance)
                       - fromEnum apartBfs
                v = (maxd * maxd * maxd) `div` ((dist + 1) * (dist + 1))
            in (v, pbag)
      return $! mapMaybe (\(p, bag) ->
        mix (p, bag) <$> accessBfs bfs p) (EM.assocs lfloor)

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
