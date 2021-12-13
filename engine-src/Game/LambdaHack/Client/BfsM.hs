{-# LANGUAGE TupleSections #-}
-- | Breadth first search and related algorithms using the client monad.
module Game.LambdaHack.Client.BfsM
  ( invalidateBfsAid, invalidateBfsPathAid
  , invalidateBfsLid, invalidateBfsPathLid
  , invalidateBfsAll, invalidateBfsPathAll
  , createBfs, getCacheBfsAndPath, getCacheBfs
  , getCachePath, createPath, condBFS
  , furthestKnown, closestUnknown, closestSmell
  , FleeViaStairsOrEscape(..)
  , embedBenefit, closestTriggers, condEnoughGearM, closestItems, closestFoes
  , closestStashes, oursExploringAssocs, closestHideout
#ifdef EXPOSE_INTERNAL
  , unexploredDepth, updatePathFromBfs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Word

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (isUknownSpace)
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

invalidateBfsAid :: MonadClient m => ActorId -> m ()
invalidateBfsAid aid =
  modifyClient $ \cli ->
    cli {sbfsD = EM.adjust (const BfsInvalid) aid (sbfsD cli)}

invalidateBfsPathAid :: MonadClient m => ActorId -> m ()
invalidateBfsPathAid aid = do
  let f BfsInvalid = BfsInvalid
      f (BfsAndPath bfsArr _) = BfsAndPath bfsArr EM.empty
  modifyClient $ \cli -> cli {sbfsD = EM.adjust f aid (sbfsD cli)}

-- Even very distant actors affected, e.g., when a hidden door found in a wall.
invalidateBfsLid :: MonadClient m => LevelId -> m ()
invalidateBfsLid lid = do
  lvl <- getLevel lid
  -- No need to filter, because foes won't be in our BFS map and looking up
  -- in our BFS map is faster than in all actors map.
  mapM_ invalidateBfsAid $ EM.elems $ lbig lvl

-- We invalidate, but not when actors move, since they are likely to move
-- out of the way in time. We only do, when they appear or disappear,
-- because they may be immobile or too close to move away before we get there.
-- We also don't consider far actors, since they are likely to disappear
-- again or to be far from our path. If they close enough to be lit
-- by our light, or one step further, that's worth taking seriously.
invalidateBfsPathLid :: MonadClient m => Actor -> m ()
invalidateBfsPathLid body = do
  lvl <- getLevel $ blid body
  let close (p, _) = chessDist p (bpos body) <= 3  -- heuristic
  -- No need to filter more, because foes won't be in our BFS map and looking up
  -- in our BFS map is faster than in all actors map.
  mapM_ (invalidateBfsPathAid . snd) $ filter close $ EM.assocs $ lbig lvl

invalidateBfsAll :: MonadClient m => m ()
invalidateBfsAll =
  modifyClient $ \cli -> cli {sbfsD = EM.map (const BfsInvalid) (sbfsD cli)}

invalidateBfsPathAll :: MonadClient m => m ()
invalidateBfsPathAll = do
  let f BfsInvalid = BfsInvalid
      f (BfsAndPath bfsArr _) = BfsAndPath bfsArr EM.empty
  modifyClient $ \cli -> cli {sbfsD = EM.map f (sbfsD cli)}

createBfs :: MonadClientRead m
          => Bool -> Word8 -> ActorId -> m (PointArray.Array BfsDistance)
createBfs canMove alterSkill0 aid =
  if canMove then do
    b <- getsState $ getActorBody aid
    salter <- getsClient salter
    let source = bpos b
        lalter = salter EM.! blid b
        alterSkill = max 1 alterSkill0
          -- We increase 0 skill to 1, to also path through unknown tiles.
          -- Since there are no other tiles that require skill 1, this is safe.
    stabs <- getsClient stabs
    return $! fillBfs lalter alterSkill source stabs
  else return PointArray.empty

updatePathFromBfs :: MonadClient m
                  => Bool -> BfsAndPath -> ActorId -> Point
                  -> m (PointArray.Array BfsDistance, Maybe AndPath)
updatePathFromBfs canMove bfsAndPathOld aid !target = do
  COps{coTileSpeedup} <- getsState scops
  let (oldBfsArr, oldBfsPath) = case bfsAndPathOld of
        (BfsAndPath bfsArr bfsPath) -> (bfsArr, bfsPath)
        BfsInvalid -> error $ "" `showFailure` (bfsAndPathOld, aid, target)
  let bfsArr = oldBfsArr
  if not canMove
  then return (bfsArr, Nothing)
  else do
    getActorB <- getsState $ flip getActorBody
    let b = getActorB aid
    fact <- getsState $ (EM.! bfid b) . sfactionD
    seps <- getsClient seps
    salter <- getsClient salter
    lvl <- getLevel (blid b)
    let !lalter = salter EM.! blid b
        fovLit p = Tile.isLit coTileSpeedup $ PointArray.fromUnboxRep
                                            $ ltile lvl `PointArray.accessI` p
        addFoeVicinity (p, aid2) =
          let b2 = getActorB aid2
          in if isFoe (bfid b) fact (bfid b2)
             then p : vicinityUnsafe p
             else [p]
        bigAdj = ES.fromList $ concatMap addFoeVicinity $ EM.assocs
                 $ EM.delete source $ lbig lvl  -- don't sidestep oneself
        !source = bpos b
        !mpath = findPathBfs bigAdj lalter fovLit source target seps bfsArr
        !bfsPath =
          maybe oldBfsPath (\path -> EM.insert target path oldBfsPath) mpath
        bap = BfsAndPath bfsArr bfsPath
    modifyClient $ \cli -> cli {sbfsD = EM.insert aid bap $ sbfsD cli}
    return (bfsArr, mpath)

-- | Get cached BFS array and path or, if not stored, generate and store first.
getCacheBfsAndPath :: forall m. MonadClient m
                   => ActorId -> Point
                   -> m (PointArray.Array BfsDistance, Maybe AndPath)
getCacheBfsAndPath aid target = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just bap@(BfsAndPath bfsArr bfsPath) ->
      case EM.lookup target bfsPath of
        Nothing -> do
          (!canMove, _) <- condBFS aid
          updatePathFromBfs canMove bap aid target
        mpath@Just{} -> return (bfsArr, mpath)
    _ -> do
      (canMove, alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
      let bfsPath = EM.empty
      updatePathFromBfs canMove (BfsAndPath bfsArr bfsPath) aid target

-- | Get cached BFS array or, if not stored, generate and store first.
getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
getCacheBfs aid = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just (BfsAndPath bfsArr _) -> return bfsArr
    _ -> do
      (canMove, alterSkill) <- condBFS aid
      !bfsArr <- createBfs canMove alterSkill aid
      let bfsPath = EM.empty
      modifyClient $ \cli ->
        cli {sbfsD = EM.insert aid (BfsAndPath bfsArr bfsPath) (sbfsD cli)}
      return bfsArr

-- | Get cached BFS path or, if not stored, generate and store first.
getCachePath :: MonadClient m => ActorId -> Point -> m (Maybe AndPath)
getCachePath aid target = do
  b <- getsState $ getActorBody aid
  let source = bpos b
  if source == target
  then return $ Just $ AndPath (bpos b) [] target 0  -- speedup
  else snd <$> getCacheBfsAndPath aid target

createPath :: MonadClient m => ActorId -> Target -> m TgtAndPath
createPath aid tapTgt = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let stopAtUnwalkable tapPath@(Just AndPath{..}) =
        let (walkable, rest) =
              -- Unknown tiles are not walkable, so path stops just before.
              -- which is good, because by the time actor reaches the tile,
              -- it is known and target is recalculated with new info,
              -- perhaps sidestepping the tile, e.g., if explosive.
              span (Tile.isWalkable coTileSpeedup . at lvl) pathList
        in case rest of
          _ | null walkable -> TgtAndPath{..}
          [] -> TgtAndPath{..}
          [g] | g == pathGoal -> TgtAndPath{..}
            -- the exception is when the tile is explicitly targeted
          newGoal : _ ->
            let newTgt = TPoint TBlock (blid b) newGoal
                newPath = AndPath{ pathSource = bpos b
                                 , pathList = walkable  -- no @newGoal@
                                 , pathGoal = newGoal
                                 , pathLen = length walkable + 1 }
            in TgtAndPath{tapTgt = newTgt, tapPath = Just newPath}
      stopAtUnwalkable Nothing = TgtAndPath{tapTgt, tapPath=Nothing}
  mpos <- getsState $ aidTgtToPos (Just aid) (blid b) (Just tapTgt)
  case mpos of
    Nothing -> return TgtAndPath{tapTgt, tapPath=Nothing}
    Just p -> do
      path <- getCachePath aid p
      return $! stopAtUnwalkable path

condBFS :: MonadClientRead m => ActorId -> m (Bool, Word8)
condBFS aid = do
  side <- getsClient sside
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of skills as the leader, anyway). Otherwise we'd have
  -- to reset BFS after leader changes, but it would still lead to
  -- wasted movement if, e.g., non-leaders move but only leaders open doors
  -- and leader change is very rare.
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let alterSkill =
        min (maxBound - 1)  -- @maxBound :: Word8@ means unalterable
            (toEnum $ max 0 $ Ability.getSk Ability.SkAlter actorMaxSk)
      canMove = Ability.getSk Ability.SkMove actorMaxSk > 0
                || Ability.getSk Ability.SkDisplace actorMaxSk > 0
                || Ability.getSk Ability.SkProject actorMaxSk > 0
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! side) . sfactionD
  let -- Under UI, playing a hero party, we let AI set our target each
      -- turn for non-pointmen that can't move and can't alter,
      -- usually to TUnknown. This is rather useless, but correct.
      enterSuspect = smarkSuspect > 0 || gunderAI fact
      skill | enterSuspect = alterSkill  -- dig and search as skill allows
            | otherwise = 0  -- only walkable tiles
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
-- the level will be nevertheless here finally labeled as explored,
-- to enable transition to other levels.
-- We should generally avoid such levels, because digging and/or trying
-- to find other stairs leading to disconnected areas is not KISS
-- so we don't do this in AI, so AI is at a disadvantage.
--
-- If the closest unknown is more than 126 tiles away from the targeting
-- actor, the level will marked as explored. We could complicate the code
-- and not mark if the unknown is too far as opposed to inaccessible,
-- but then if it is both too distant and inaccessible, AI would be
-- permanently stuck on such levels. To cope with this, escapes need to be
-- placed on open or small levels, or in dispersed enough that they don't
-- appear in such potentially unexplored potions of caves. Other than that,
-- this is rather harmless and hard to exploit, so let it be.
-- The principled way to fix this would be to extend BFS to @Word16@,
-- but then it takes too long to compute on maze levels, so we'd need
-- to optimize hard for JS.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  bfs <- getCacheBfs aid
  let closestPoss = PointArray.minIndexesA bfs
      dist = bfs PointArray.! head closestPoss
      !_A = assert (lexpl lvl >= lseen lvl) ()
  return $!
    if lexpl lvl <= lseen lvl
         -- Some unknown may still be visible and even pathable, but we already
         -- know from global level info that they are inaccessible.
       || dist >= apartBfs
         -- Global level info may tell us that terrain was changed and so
         -- some new explorable tile appeared, but we don't care about those
         -- and we know we already explored all initially seen unknown tiles
         -- and it's enough for us (otherwise we'd need to hunt all around
         -- the map for tiles altered by enemies).
    then Nothing
    else let unknownAround pos =
               let vic = vicinityUnsafe pos
                   countUnknown :: Int -> Point -> Int
                   countUnknown c p =
                     if isUknownSpace $ lvl `at` p then c + 1 else c
               in foldl' countUnknown 0 vic
             cmp = comparing unknownAround
         in Just $ maximumBy cmp closestPoss

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
      return $! sortOn (fst &&& absoluteTimeNegate . snd . snd) ts

data FleeViaStairsOrEscape =
    ViaStairs
  | ViaStairsUp
  | ViaStairsDown
  | ViaEscape
  | ViaExit  -- can change whenever @sexplored@ changes
  | ViaNothing
  | ViaAnything
  deriving (Show, Eq)

embedBenefit :: MonadClientRead m
             => FleeViaStairsOrEscape -> ActorId
             -> [(Point, ItemBag)]
             -> m [(Double, (Point, ItemBag))]
embedBenefit fleeVia aid pbags = do
  COps{cocave, coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  lvl <- getLevel (blid b)
  oursExploring <- getsState $ oursExploringAssocs (bfid b)
  let oursExploringLid =
        filter (\(_, body) -> blid body == blid b) oursExploring
      spawnFreqs = CK.cactorFreq $ okind cocave $ lkind lvl
      hasGroup grp = fromMaybe 0 (lookup grp spawnFreqs) > 0
      lvlSpawnsUs = any (hasGroup . fst) $ filter ((> 0) . snd)
                                         $ fgroups (gkind fact)
  actorSk <- if fleeVia `elem` [ViaAnything, ViaExit]
                  -- targeting, possibly when not a leader
             then getsState $ getActorMaxSkills aid
             else currentSkillsClient aid
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
  condOurAdj <- getsState $ any (\(_, b2) -> isFriend (bfid b) fact (bfid b2))
                            . adjacentBigAssocs b
  unexploredTrue <- unexploredDepth True (blid b)
  unexploredFalse <- unexploredDepth False (blid b)
  condEnoughGear <- condEnoughGearM aid
  discoBenefit <- getsClient sdiscoBenefit
  getKind <- getsState $ flip getIidKind
  let alterMinSkill p = Tile.alterMinSkill coTileSpeedup $ lvl `at` p
      lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      -- Ignoring the number of items, because only one of each @iid@
      -- is triggered at the same time, others are left to be used later on.
      -- Taking the kind the item hides under into consideration, because
      -- it's a best guess only, for AI and UI.
      iidToEffs iid = IK.ieffects $ getKind iid
      feats bag = concatMap iidToEffs $ EM.keys bag
      -- For simplicity, we assume at most one exit at each position.
      -- AI uses exit regardless of traps or treasures at the spot.
      bens (_, bag) = case find IK.isEffEscapeOrAscend $ feats bag of
        Just IK.Escape{} ->
          -- Escape (or guard) only after exploring, for high score, etc.
          let escapeOrGuard =
                fcanEscape (gkind fact)
                || fleeVia == ViaExit  -- target to guard after explored
          in if fleeVia `elem` [ViaAnything, ViaEscape, ViaExit]
                && escapeOrGuard
                && allExplored
             then 10
             else 0  -- don't escape prematurely
        Just (IK.Ascend up) ->  -- change levels sensibly, in teams
          let easier = up /= (fromEnum (blid b) > 0)
              unexpForth = if up then unexploredTrue else unexploredFalse
              unexpBack = if not up then unexploredTrue else unexploredFalse
              -- Forbid loops via peeking at unexplored and getting back.
              aiCond = if unexpForth
                       then easier && condEnoughGear
                            || (not unexpBack || easier) && lidExplored
                       else not unexpBack && easier && allExplored
                            && null (lescape lvl)
              -- Prefer one direction of stairs, to team up
              -- and prefer embed (may, e.g., create loot) over stairs.
              v = if aiCond then if easier then 10 else 1 else 0
              guardingStash = case gstash fact of
                Nothing -> False
                Just (lid, p) ->
                  lid == blid b
                  && (length oursExploring > 1
                      || lvlSpawnsUs)
                  && (length oursExploringLid <= 1
                        -- not @==@ in case guard temporarily nonmoving
                      || p == bpos b && not condOurAdj)
                           -- don't leave the post; let the others explore
          in case fleeVia of
            _ | guardingStash -> 0
            ViaStairsUp | up -> 1
            ViaStairsDown | not up -> 1
            ViaStairs -> v
            ViaExit -> v
            ViaAnything -> v
            _ -> 0  -- don't ascend prematurely
        _ ->
          if fleeVia `elem` [ViaNothing, ViaAnything]
          then -- Actor uses the embedded item on himself, hence @benApply@.
               -- Let distance be the deciding factor and also prevent
               -- overflow on 32-bit machines.
               let sacrificeForExperiment = 101  -- single explosion acceptable
                   sumBen = sum $ map (\iid ->
                     benApply $ discoBenefit EM.! iid) (EM.keys bag)
               in min 1000 $ sacrificeForExperiment + sumBen
          else 0
      underFeet p = p == bpos b  -- if enter and alter, be more permissive
      -- Only actors with high enough @SkAlter@ can trigger terrain.
      -- Blocking actors and items not checked, because they can be moved
      -- before the actor gets to the location, or after.
      f (p, _) = underFeet p
                 || alterSkill >= fromEnum (alterMinSkill p)
                 || Tile.isSuspect coTileSpeedup (lvl `at` p)
                    && alterSkill >= 2
      benFeats = map (\pbag -> (bens pbag, pbag)) $ filter f pbags
      considered (benefitAndSacrifice, (p, _bag)) =
        benefitAndSacrifice > 0
        -- For speed and to avoid greedy AI loops, only experiment with few.
        && Tile.consideredByAI coTileSpeedup (lvl `at` p)
  return $! filter considered benFeats

-- | Closest (wrt paths) AI-triggerable tiles with embedded items.
-- In AI, the level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels, but before that, he triggers other tiles
-- in hope of some loot or beneficial effect to enter next level with.
closestTriggers :: MonadClient m => FleeViaStairsOrEscape -> ActorId
                -> m [(Int, (Point, (Point, ItemBag)))]
closestTriggers fleeVia aid = do
  COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
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
        map (\p -> (cid, (p, (p0, bag))))
            (vicinityBounded rWidthMax rHeightMax p0)
      vicAll = concatMap vicTrigger efeat
  return $!
    let mix (benefit, ppbag) dist =
          let maxd = subtractBfsDistance maxBfsDistance apartBfs
              v = intToDouble $ maxd `div` (dist + 1)
          in (ceiling $ benefit * v, ppbag)
    in mapMaybe (\bpp@(_, (p, _)) ->
         mix bpp <$> accessBfs bfs p) vicAll

-- | Check whether the actor has enough gear to go look for enemies.
-- We assume weapons in equipment are better than any among organs
-- or at least provide some essential diversity.
-- Disabled if, due to doctrine, actors follow leader and so would
-- repeatedly move towards and away from stairs at leader change,
-- depending on current leader's gear.
-- Number of items of a single kind is ignored, because variety is needed.
condEnoughGearM :: MonadClientRead m => ActorId -> m Bool
condEnoughGearM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let followDoctrine =
        gdoctrine fact `elem` [Ability.TFollow, Ability.TFollowNoItems]
  eqpAssocs <- getsState $ fullAssocs aid [CEqp]
  return $ not followDoctrine  -- keep it lazy
           && (any (IA.checkFlag Ability.Meleeable
                    . aspectRecordFull . snd) eqpAssocs
               || length eqpAssocs >= 3)

unexploredDepth :: MonadClientRead m => Bool -> LevelId -> m Bool
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
  body <- getsState $ getActorBody aid
  Level{lfloor, lbig} <- getLevel $ blid body
  factionD <- getsState sfactionD
  per <- getPerFid $ blid body
  let canSee p = ES.member p (totalVisible per)
  -- Don't consider items at any stash location that an actor stands over
  -- or can stand over, but it's out of our LOS.
  -- In case of the own stash, don't consider regardless of actors and LOS.
  -- Own stash items are already owned, enemy stash is already targetted
  -- and allied or neutral stashes with actors on top are unlikely
  -- to be vacated and cause AI to wonder around forever or look up,
  -- leave, return hopeful, find a guard, repeat.
  let stashes = map (second gstash) $ EM.assocs factionD
      stashToRemove :: (FactionId, Maybe (LevelId, Point)) -> [Point]
      stashToRemove (fid, Just (lid, pos))
        | lid == blid body
          && (fid == bfid body || pos `EM.member` lbig || not (canSee pos)) =
            [pos]
      stashToRemove _ = []
      stashesToRemove = ES.fromList $ concatMap stashToRemove stashes
      lfloorBarStashes = EM.withoutKeys lfloor stashesToRemove
  if EM.null lfloorBarStashes then return [] else do
    bfs <- getCacheBfs aid
    let mix pbag dist =
          let maxd = subtractBfsDistance maxBfsDistance apartBfs
              -- Beware of overflowing 32-bit integers.
              -- Here distance is the only factor influencing frequency.
              -- Whether item is desirable is checked later on.
              v = (maxd * 10) `div` (dist + 1)
          in (v, pbag)
    return $! mapMaybe (\(p, bag) ->
      mix (p, bag) <$> accessBfs bfs p) (EM.assocs lfloorBarStashes)

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

-- | Closest (wrt paths) enemy or our unguarded stash locations. If it's ours,
-- we want to guard it, it enemy, loot it. Neutral and friendly stashes
-- not chased to avoid loops of bloodless takeovers.
closestStashes :: MonadClient m => ActorId -> m [(Int, (FactionId, Point))]
closestStashes aid = do
  COps{cocave} <- getsState scops
  factionD <- getsState sfactionD
  b <- getsState $ getActorBody aid
  lvl <- getLevel (blid b)
  oursExploring <- getsState $ oursExploringAssocs (bfid b)
  let fact = factionD EM.! bfid b
      spawnFreqs = CK.cactorFreq $ okind cocave $ lkind lvl
      hasGroup grp = fromMaybe 0 (lookup grp spawnFreqs) > 0
      lvlSpawnsUs = any (hasGroup . fst) $ filter ((> 0) . snd)
                                         $ fgroups (gkind fact)
      qualifyStash (fid2, Faction{gstash}) = case gstash of
        Nothing -> Nothing
        Just (lid, pos) ->
          -- The condition below is more strict that in @updateTgt@
          -- to avoid loops by changing target of actor displacing
          -- and walking over stash to @TStash@.
          if lid == blid b
             && (fid2 == bfid b
                 && isNothing (posToBigLvl pos lvl)  -- unguarded
                 && (length oursExploring > 1  -- other actors able to explore
                     || lvlSpawnsUs)  -- or future spawned will be able
                 || isFoe (bfid b) fact fid2)
          then Just (fid2, pos)
          else Nothing
  case mapMaybe qualifyStash $ EM.assocs factionD of
    [] -> return []
    stashes -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(_, pos) -> fmap (,x) (accessBfs bfs pos)) stashes
      return $! sortBy (comparing fst) ds

oursExploringAssocs :: FactionId -> State -> [(ActorId, Actor)]
oursExploringAssocs fid s =
  let f (!aid, !b) = bfid b == fid
                     && not (bproj b)
                     && bhp b > 0  -- dead can stay forever on a frozen level
                     && (bwatch b `elem` [WSleep, WWake]
                           -- if asleep, probably has walking skill normally;
                           -- when left alone will wake up and guard or explore
                        || let actorMaxSk = sactorMaxSkills s EM.! aid
                           in Ability.getSk Ability.SkMove actorMaxSk > 0
                              || Ability.getSk Ability.SkMove actorMaxSk < -50)
                                   -- a hacky way to rule out tmp immobile
  in filter f $ EM.assocs $ sactorD s

-- | Find the nearest walkable position in dark, if any. Deterministic,
-- to let all friends gather up and defend in the same shelter.
-- Ignore position underfoot.
closestHideout :: MonadClient m => ActorId -> m (Maybe (Point, Int))
closestHideout aid = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel (blid b)
  bfs <- getCacheBfs aid
  let minHideout :: (Point, BfsDistance) -> Point -> BfsDistance
                 -> (Point, BfsDistance)
      minHideout (pMin, distMin) p dist =
        if dist > minKnownBfs && dist < distMin
           && Tile.isHideout coTileSpeedup (lvl `at` p)
        then (p, dist)
        else (pMin, distMin)
      (p1, dist1) = PointArray.ifoldlA' minHideout (bpos b, maxBfsDistance) bfs
  return $! if p1 == bpos b  -- possibly hideout underfoot; ignore
            then Nothing
            else Just (p1, subtractBfsDistance dist1 apartBfs)
