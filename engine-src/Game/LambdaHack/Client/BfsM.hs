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
  , closestStashes
#ifdef EXPOSE_INTERNAL
  , unexploredDepth, updatePathFromBfs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Ord
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
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (isUknownSpace)
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

invalidateBfsAid :: MonadClient m => ActorId -> m ()
invalidateBfsAid aid =
  modifyClient $ \cli -> cli {sbfsD = EM.insert aid BfsInvalid (sbfsD cli)}

invalidateBfsPathAid :: MonadClient m => ActorId -> m ()
invalidateBfsPathAid aid = do
  let f BfsInvalid = BfsInvalid
      f (BfsAndPath bfsArr _) = BfsAndPath bfsArr EM.empty
  modifyClient $ \cli -> cli {sbfsD = EM.adjust f aid (sbfsD cli)}

invalidateBfsLid :: MonadClient m => LevelId -> m ()
invalidateBfsLid lid = do
  side <- getsClient sside
  let f (_, b) = blid b == lid && bfid b == side && not (bproj b)
  as <- getsState $ filter f . EM.assocs . sactorD
  mapM_ (invalidateBfsAid . fst) as

invalidateBfsPathLid :: MonadClient m => LevelId -> Point -> m ()
invalidateBfsPathLid lid pos = do
  side <- getsClient sside
  let f (_, b) = blid b == lid && bfid b == side && not (bproj b)
                 && chessDist pos (bpos b) < 10  -- heuristic
  as <- getsState $ filter f . EM.assocs . sactorD
  mapM_ (invalidateBfsPathAid . fst) as

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
  if | source == target ->
       return $ Just $ AndPath (bpos b) [] target 0  -- speedup
     | otherwise -> snd <$> getCacheBfsAndPath aid target

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
  mpos <- getsState $ aidTgtToPos aid (blid b) (Just tapTgt)
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
  let underAI = isAIFact fact
      -- Under UI, playing a hero party, we let AI set our target each
      -- turn for non-pointmen that can't move and can't alter,
      -- usually to TUnknown. This is rather useless, but correct.
      enterSuspect = smarkSuspect > 0 || underAI
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
      return $! sortBy (comparing (fst &&& absoluteTimeNegate . snd . snd)) ts

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
  COps{coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  actorSk <- if fleeVia `elem` [ViaAnything, ViaExit]
                  -- targeting, possibly when not a leader
             then getsState $ getActorMaxSkills aid
             else currentSkillsClient aid
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
  fact <- getsState $ (EM.! bfid b) . sfactionD
  lvl <- getLevel (blid b)
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
                fcanEscape (gplayer fact)
                || fleeVia `elem` [ViaExit]  -- target to guard after explored
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
                       else easier && allExplored && null (lescape lvl)
              -- Prefer one direction of stairs, to team up
              -- and prefer embed (may, e.g., create loot) over stairs.
              v = if aiCond then if easier then 10 else 1 else 0
          in case fleeVia of
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
      -- If apply skill not high enough for embedded items, AI will only
      -- guard such tiles, assuming they must be advanced and so crucial.
      f (p, _) = underFeet p || alterSkill >= fromEnum (alterMinSkill p)
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
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
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
        map (\p -> (cid, (p, (p0, bag)))) $ vicinityBounded rXmax rYmax p0
      vicAll = concatMap vicTrigger efeat
  return $!
    let mix (benefit, ppbag) dist =
          let maxd = subtractBfsDistance maxBfsDistance apartBfs
              v = fromIntegral $ (1 + maxd - dist) ^ (2 :: Int)
          in (ceiling $ benefit * v, ppbag)
    in mapMaybe (\bpp@(_, (p, _)) ->
         mix bpp <$> accessBfs bfs p) vicAll

-- | Check whether the actor has enough gear to go look for enemies.
-- We assume weapons in equipment are better than any among organs
-- or at least provide some essential diversity.
-- Disabled if, due to doctrine, actors follow leader and so would
-- repeatedly move towards and away form stairs at leader change,
-- depending on current leader's gear.
-- Number of items of a single kind is ignored, because variety is needed.
condEnoughGearM :: MonadClientRead m => ActorId -> m Bool
condEnoughGearM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let followDoctrine = fdoctrine (gplayer fact)
                       `elem` [Ability.TFollow, Ability.TFollowNoItems]
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
  actorMaxSk <- getsState $ getActorMaxSkills aid
  if Ability.getSk Ability.SkMoveItem actorMaxSk <= 0 then return []
  else do
    body <- getsState $ getActorBody aid
    Level{lfloor} <- getLevel $ blid body
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid body
    -- Don't consider own stash an ordinary pile of items.
    let lfloorBarStash = case mstash of
          Just (lid, pos) | lid == blid body -> EM.delete pos lfloor
          _ -> lfloor
    if EM.null lfloorBarStash then return [] else do
      bfs <- getCacheBfs aid
      let mix pbag dist =
            let maxd = subtractBfsDistance maxBfsDistance apartBfs
                -- Beware of overflowing 32-bit integers.
                -- Here distance is the only factor influencing frequency.
                -- Whether item is desirable is checked later on.
                v = (maxd * 10) `div` (dist + 1)
            in (v, pbag)
      return $! mapMaybe (\(p, bag) ->
        mix (p, bag) <$> accessBfs bfs p) (EM.assocs lfloorBarStash)

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

-- | Closest (wrt paths) enemy stash locations.
closestStashes :: MonadClient m => ActorId -> m [(Int, (FactionId, Point))]
closestStashes aid = do
  factionD <- getsState sfactionD
  b <- getsState $ getActorBody aid
  let qualifyStash (fid, Faction{gstash}) = case gstash of
        Nothing -> Nothing
        Just (lid, pos) ->
          if lid == blid b && isFoe (bfid b) (factionD EM.! bfid b) fid
          then Just (fid, pos)
          else Nothing
  case mapMaybe qualifyStash $ EM.assocs factionD of
    [] -> return []
    stashes -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(_, pos) -> fmap (,x) (accessBfs bfs pos)) stashes
      return $! sortBy (comparing fst) ds
