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
import Game.LambdaHack.Common.ItemStrongest
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
  b <- getsState $ getActorBody aid
  mpos <- aidTgtToPos aid (blid b) tapTgt
  case mpos of
    Nothing -> return TgtAndPath{tapTgt, tapPath=NoPath}
    Just p -> do
      tapPath <- getCachePath aid p
      return $! TgtAndPath{..}

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

data TriggerClass = TriggerUp | TriggerDown | TriggerEscape | TriggerOther
  deriving Eq

-- | Closest (wrt paths) AI-triggerable tiles with embedded items.
-- In AI, the level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels, but before that, he triggers other tiles
-- in hope of some loot or beneficial effect to enter next level with.
closestTriggers :: MonadClient m => Maybe Bool -> ActorId
                -> m [(Int, (Point, (Point, ItemBag)))]
closestTriggers onlyDir aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  actorMaxSk <- maxActorSkillsClient aid
  body <- getsState $ getActorBody aid
  let lid = blid body
  lvl <- getLevel lid
  let aiAlterMinSkill p = Tile.aiAlterMinSkill coTileSpeedup $ lvl `at` p
      alterSkill = EM.findWithDefault 0 Ability.AbAlter actorMaxSk
  explored <- getsClient sexplored
  dungeon <- getsState sdungeon
  unexploredD <- unexploredDepth
  condEnoughGear <- condEnoughGearM aid
  classIid <- getsState $ \s iid ->
    -- Contrived, for now.
    let Item{jname} = getItemBody iid s
    in if | jname == "staircase up" -> TriggerUp
          | jname == "staircase down" -> TriggerDown
          | jname == "escape" -> TriggerEscape
          | otherwise -> TriggerOther
  let allExplored = ES.size explored == EM.size dungeon
      -- If lid not explored, aid equips a weapon and so can leave level.
      lidExplored = ES.member (blid body) explored
      f :: Point -> ItemBag -> [(TriggerClass, (Point, ItemBag))]
        -> [(TriggerClass, (Point, ItemBag))]
      f p bag acc =
        if alterSkill < fromEnum (aiAlterMinSkill p) then acc else
        let classes = map classIid $ EM.keys bag
        in case filter (/= TriggerOther) classes of
          [] -> -- Neither stairs nor escape, so explore if close enough.
                if isJust onlyDir
                   || Tile.isSuspect coTileSpeedup (lvl `at` p)
                      && not (Tile.consideredByAI coTileSpeedup (lvl `at` p))
                then acc  -- assume secrets have no loot, unless ConsideredByAI
                else (TriggerOther, (p, bag)) : acc
          TriggerEscape : _ ->  -- normally only one present, so just take first
            -- Escape (or guard) only after exploring, for high score, etc.
            if isNothing onlyDir && allExplored
            then (TriggerEscape, (p, bag)) : acc
            else acc
          cid : _ ->  -- normally only one present, so just take first
            let up = cid == TriggerUp
                easier = up /= (fromEnum lid > 0)
                unexpForth = unexploredD up lid
                unexpBack = unexploredD (not up) lid
                aiCond = if unexpForth
                         then easier && condEnoughGear
                              || (not unexpBack || easier) && lidExplored
                         else easier && allExplored && null (lescape lvl)
                interesting = case onlyDir of
                  Just d -> d == up
                  Nothing -> aiCond
            in if interesting then (cid, (p, bag)) : acc else acc
      triggers = EM.foldrWithKey f [] $ lembed lvl
  -- The advantage of targeting the tiles in vicinity of triggers is that
  -- triggers don't need to be pathable (and so AI doesn't bump into them
  -- by chance while walking elsewhere) and that many accesses to the tiles
  -- are more likely to be targeted by different AI actors (even starting
  -- from the same location), so there is less risk of clogging stairs and,
  -- OTOH, siege of stairs or escapes is more effective.
  bfs <- getCacheBfs aid
  let vicTrigger (cid, (p0, bag)) =
        map (\p -> (cid, (p, (p0, bag)))) $ vicinityUnsafe p0
      vicAll = concatMap vicTrigger triggers
  return $  -- keep lazy
    let mix (cid, ppbag) dist =
          -- Prefer stairs to easier levels. Prefer loot over stairs.
          let depthDelta = case cid of
                TriggerUp -> if fromEnum lid > 0 then 1 else 2
                TriggerDown -> if fromEnum lid > 0 then 2 else 1
                TriggerEscape -> 10  -- congregate at escape
                TriggerOther -> 4
              maxd = fromEnum (maxBound :: BfsDistance)
                     - fromEnum apartBfs
              v | dist == 0 = maxd * maxd * 1000
                    -- if already adjacent, usually keep, but fuzz, to guard
                | otherwise = (maxd * maxd * 100) `div` (dist * dist)
          in (depthDelta * v, ppbag)
    in mapMaybe (\(cid, (p, pbag)) ->
         mix (cid, (p, pbag)) <$> accessBfs bfs p) vicAll

data FleeViaStairsOrEscape = ViaStairs | ViaEscape | ViaNothing | ViaAnything
  deriving (Show, Eq)

embedBenefit :: MonadClient m
             => ActorId -> FleeViaStairsOrEscape -> [(Point, ItemBag)]
             -> m [(Int, (Point, ItemBag))]
embedBenefit aid fleeVia pbags = do
  Kind.COps{coitem=Kind.Ops{okind}, coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  actorSk <- currentSkillsClient aid
  let alterSkill = EM.findWithDefault 0 Ability.AbAlter actorSk
  fact <- getsState $ (EM.! bfid b) . sfactionD
  lvl <- getLevel (blid b)
  unexploredD <- unexploredDepth
  condEnoughGear <- condEnoughGearM aid
  discoKind <- getsClient sdiscoKind
  discoBenefit <- getsClient sdiscoBenefit
  s <- getState
  let aiAlterMinSkill p = Tile.aiAlterMinSkill coTileSpeedup $ lvl `at` p
      lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      unexploredTrue = unexploredD True (blid b)
      unexploredFalse = unexploredD False (blid b)
      -- Ignoring the number of items, because only one of each @iid@
      -- is triggered at the same time, others are left to be used later on.
      iidToEffs iid = case EM.lookup (jkindIx $ getItemBody iid s) discoKind of
        Nothing -> []
        Just KindMean{kmKind} -> IK.ieffects $ okind kmKind
      isEffAscend IK.Ascend{} = True
      isEffAscend _ = False
      isEffEscape IK.Escape{} = True
      isEffEscape _ = False
      -- For simplicity, we assume at most one exit at each position
      -- and we force AI to use exit even if terrible traps protect it.
      bens (pos, bag) = do
        let fs = concatMap iidToEffs $ EM.keys bag
        case find isEffEscape fs of
          Just{} ->
            -- Only some factions try to escape but they first explore all
            -- for high score.
            return $! if fleeVia `notElem` [ViaEscape, ViaAnything]
                         || not (fcanEscape $ gplayer fact)
                         || not allExplored
                      then 0  -- don't escape prematurely
                      else 10000
          Nothing -> case find isEffAscend fs of
            Just (IK.Ascend up) -> do  -- change levels sensibly, in teams
              let easier = up /= (fromEnum (blid b) > 0)
                  unexpForth = if up then unexploredTrue else unexploredFalse
                  unexpBack = if not up then unexploredTrue else unexploredFalse
                  -- Forbid loops via peeking at unexplored and getting back.
                  aiCond = if unexpForth
                           then easier && condEnoughGear
                                || (not unexpBack || easier) && lidExplored
                           else easier && allExplored && null (lescape lvl)
                  -- Prefer one direction of stairs, to team up.
                  eben = if aiCond then if easier then 10 else 1 else 0
              return $! if fleeVia `notElem` [ViaStairs, ViaAnything]
                        then 0  -- don't flee prematurely
                        else 10000 * eben
            _ -> return $!
              if fleeVia `elem` [ViaNothing, ViaAnything]
                 && (not (Tile.isSuspect coTileSpeedup (lvl `at` pos))
                     || Tile.consideredByAI coTileSpeedup (lvl `at` pos))
              then
                -- Actor uses he embedded item on himself, hence @snd@,
                -- the same as when flinging item at an enemy.
                sum $ mapMaybe (\iid -> snd <$> EM.lookup iid discoBenefit)
                               (EM.keys bag)
              else 0
      -- Only actors with high enough AbAlter can trigger embedded items.
      enterableHere p = alterSkill >= fromEnum (aiAlterMinSkill p)
      ebags = filter (enterableHere . fst) pbags
  benFeats <- mapM bens ebags
  return $ filter ((> 0 ) . fst) $ zip benFeats ebags

-- | Check whether the actor has enough gear to go look for enemies.
-- We assume weapons in equipment are better than any among organs
-- or at least provide some essential diversity.
-- Disable if, due to tactic, actors follow leader and so would
-- repeatedly move towards and away form stairs at leader change,
-- depending on current leader's gear.
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

unexploredDepth :: MonadClient m => m (Bool -> LevelId -> Bool)
unexploredDepth = do
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  let allExplored = ES.size explored == EM.size dungeon
      unexploredD up =
        let unex lid = allExplored
                       && not (null $ lescape $ dungeon EM.! lid)
                       || ES.notMember lid explored
                       || unexploredD up lid
        in any unex . ascendInBranch dungeon up
  return unexploredD

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
