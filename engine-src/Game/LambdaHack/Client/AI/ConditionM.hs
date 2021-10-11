-- | Assorted conditions used later on in AI logic.
module Game.LambdaHack.Client.AI.ConditionM
  ( condAimEnemyTargetedM
  , condAimEnemyOrStashM
  , condAimEnemyOrRememberedM
  , condAimNonEnemyPresentM
  , condAimCrucialM
  , condTgtNonmovingEnemyM
  , condAdjTriggerableM
  , meleeThreatDistList
  , condBlocksFriendsM
  , condFloorWeaponM
  , condNoEqpWeaponM
  , condCanProjectM
  , condProjectListM
  , benAvailableItems
  , hinders
  , condDesirableFloorItemM
  , benGroundItems
  , desirableItem
  , condSupport
  , condAloneM
  , condShineWouldBetrayM
  , fleeList
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Client.Bfs
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
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.RuleKind as RK
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- All conditions are (partially) lazy, because they are not always
-- used in the strict monadic computations they are in.

-- | Require that a target enemy is visible by the party.
condAimEnemyTargetedM :: MonadClientRead m => ActorId -> m Bool
condAimEnemyTargetedM aid = do
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TEnemy _) -> True
    _ -> False

-- | Require that a target enemy or enemy stash is visible by the party.
condAimEnemyOrStashM :: MonadClientRead m => ActorId -> m Bool
condAimEnemyOrStashM aid = do
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TEnemy _) -> True
    Just (TPoint (TStash _) _ _) -> True  -- speedup from: lid == blid b
    _ -> False

-- | Require that a target enemy is remembered on the actor's level.
condAimEnemyOrRememberedM :: MonadClientRead m => ActorId -> m Bool
condAimEnemyOrRememberedM aid = do
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TEnemy _) -> True
    Just (TPoint (TEnemyPos _) lid _) -> lid == blid b
    Just (TPoint (TStash _) lid _) -> lid == blid b
    _ -> False

-- | Require that a target non-enemy is visible by the party.
condAimNonEnemyPresentM :: MonadClientRead m => ActorId -> m Bool
condAimNonEnemyPresentM aid = do
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TNonEnemy _) -> True
    _ -> False

-- | Require that the target is crucial to success, e.g., an item,
-- or that it's not too far away and so the changes to get it are high.
condAimCrucialM :: MonadClientRead m => ActorId -> m Bool
condAimCrucialM aid = do
  b <- getsState $ getActorBody aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  return $ case mtgtMPath of
    Just TgtAndPath{tapTgt=TEnemy _} -> True
    Just TgtAndPath{tapTgt=TPoint tgoal lid _, tapPath=Just AndPath{pathLen}} ->
      lid == blid b
      && (pathLen < 10  -- close enough to get there first
          || tgoal `notElem` [TUnknown, TKnown])
    Just TgtAndPath{tapTgt=TVector{}, tapPath=Just AndPath{pathLen}} ->
      pathLen < 7  -- can't say if the target important, but the constants
                   -- from @take6@ and @traSlack7@ ensure target is
                   -- already approached or close to level edge
                   -- or not a random @traSlack7@ wandering
    _ -> False  -- includes the case of target with no path

-- | Check if the target is a nonmoving enemy.
condTgtNonmovingEnemyM :: MonadClientRead m => ActorId -> m Bool
condTgtNonmovingEnemyM aid = do
  btarget <- getsClient $ getTarget aid
  case btarget of
    Just (TEnemy enemy) -> do
      actorMaxSk <- getsState $ getActorMaxSkills enemy
      return $ Ability.getSk Ability.SkMove actorMaxSk <= 0
    _ -> return False

-- | Require the actor stands on or adjacent to a triggerable tile
-- (e.g., stairs).
condAdjTriggerableM :: MonadStateRead m => Ability.Skills -> ActorId -> m Bool
condAdjTriggerableM actorSk aid = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
      alterMinSkill p = Tile.alterMinSkill coTileSpeedup $ lvl `at` p
      underFeet p = p == bpos b  -- if enter and alter, be more permissive
      -- Before items are applied (which AI attempts even if apply
      -- skills too low), tile must be alerable, hence both checks.
      hasTriggerable p = (underFeet p
                          || alterSkill >= fromEnum (alterMinSkill p))
                         && p `EM.member` lembed lvl
  return $ any hasTriggerable $ bpos b : vicinityUnsafe (bpos b)

-- | Produce the chess-distance-sorted list of non-low-HP,
-- melee-cabable foes on the level. We don't consider path-distance,
-- because we are interested in how soon the foe can close in to hit us,
-- which can diverge greately from path distance for short distances,
-- e.g., when terrain gets revealed. We don't consider non-moving actors,
-- because they can't chase us and also because they can't be aggresive
-- so to resolve the stalemate, the opposing AI has to be aggresive
-- by ignoring them and closing in to melee distance.
meleeThreatDistList :: [(ActorId, Actor)] -> ActorId -> State
                    -> [(Int, (ActorId, Actor))]
meleeThreatDistList foeAssocs aid s =
  let actorMaxSkills = sactorMaxSkills s
      b = getActorBody aid s

      strongActor (aid2, b2) =
        let actorMaxSk = actorMaxSkills EM.! aid2
            nonmoving = Ability.getSk Ability.SkMove actorMaxSk <= 0
        in not (hpTooLow b2 actorMaxSk || nonmoving)
           && actorCanMeleeToHarm actorMaxSkills aid2 b2
      allThreats = filter strongActor foeAssocs
      addDist (aid2, b2) = (chessDist (bpos b) (bpos b2), (aid2, b2))
  in sortBy (comparing fst) $ map addDist allThreats

-- | Require the actor blocks the paths of any of his party members.
condBlocksFriendsM :: MonadClientRead m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  targetD <- getsClient stargetD
  let blocked aid2 = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just TgtAndPath{tapPath=Just AndPath{pathList=q : _}} | q == bpos b ->
            True
          _ -> False
  any blocked <$> getsState (fidActorRegularIds (bfid b) (blid b))

-- | Require the actor stands over a weapon that would be auto-equipped,
-- if only it was a desirable item (checked elsewhere).
condFloorWeaponM :: MonadStateRead m => ActorId -> m Bool
condFloorWeaponM aid =
  any (IA.checkFlag Ability.Meleeable . aspectRecordFull . snd) <$>
    getsState (fullAssocs aid [CGround])

-- | Check whether the actor has no weapon in equipment.
condNoEqpWeaponM :: MonadStateRead m => ActorId -> m Bool
condNoEqpWeaponM aid =
  all (not . IA.checkFlag Ability.Meleeable . aspectRecordFull . snd) <$>
    getsState (fullAssocs aid [CEqp])

-- | Require that the actor can project any items.
condCanProjectM :: MonadClientRead m => Int -> ActorId -> m Bool
condCanProjectM skill aid = do
  side <- getsClient sside
  curChal <- getsClient scurChal
  fact <- getsState $ (EM.! side) . sfactionD
  if skill < 1
     || ckeeper curChal && fhasUI (gplayer fact)
  then return False
  else  -- shortcut
    -- Compared to conditions in @projectItem@, range and charge are ignored,
    -- because they may change by the time the position for the fling
    -- is reached.
    not . null <$> condProjectListM skill aid

condProjectListM :: MonadClientRead m
                 => Int -> ActorId
                 -> m [(Double, CStore, ItemId, ItemFull, ItemQuant)]
condProjectListM skill aid = do
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  discoBenefit <- getsClient sdiscoBenefit
  getsState $ projectList discoBenefit skill aid
                          condShineWouldBetray condAimEnemyOrRemembered

projectList :: DiscoveryBenefit -> Int -> ActorId -> Bool -> Bool -> State
            -> [(Double, CStore, ItemId, ItemFull, ItemQuant)]
projectList discoBenefit skill aid
            condShineWouldBetray condAimEnemyOrRemembered s =
  let b = getActorBody aid s
      actorMaxSk = getActorMaxSkills aid s
      calmE = calmEnough b actorMaxSk
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltasSerious (bcalmDelta b)
      uneasy = condAimEnemyOrRemembered
               || not calmE
               || heavilyDistressed
        -- don't take recent fleeing into account when item can be lost
      coeff CGround = 2  -- pickup turn saved
      coeff COrgan = error $ "" `showFailure` benList
      coeff CEqp = 1000  -- must hinder currently (or be very potent);
                         -- note: not larger, to avoid Int32 overflow
      coeff CStash = 1
      -- This detects if the value of keeping the item in eqp is in fact < 0.
      hind = hinders condShineWouldBetray uneasy actorMaxSk
      goodMissile (Benefit{benInEqp, benFling}, cstore, iid, itemFull, kit) =
        let arItem = aspectRecordFull itemFull
            benR = coeff cstore * benFling
        in if benR < -1  -- ignore very weak projectiles
              && (not benInEqp  -- can't wear, so OK to risk losing or breaking
                  || not (IA.checkFlag Ability.Meleeable arItem)
                       -- anything else expendable
                     && hind itemFull)  -- hinders now, so possibly often
              && permittedProjectAI skill calmE itemFull
           then Just (benR, cstore, iid, itemFull, kit)
           else Nothing
      stores = [CStash, CGround] ++ [CEqp | calmE]
      benList = benAvailableItems discoBenefit aid stores s
  in mapMaybe goodMissile benList

-- | Produce the list of items from the given stores available to the actor
-- and the items' values.
benAvailableItems :: DiscoveryBenefit -> ActorId -> [CStore] -> State
                  -> [(Benefit, CStore, ItemId, ItemFull, ItemQuant)]
benAvailableItems discoBenefit aid cstores s =
  let b = getActorBody aid s
      mstash = gstash $ sfactionD s EM.! bfid b
      ben _ CGround | mstash == Just (blid b, bpos b) = []
      ben bag cstore =
        [ (discoBenefit EM.! iid, cstore, iid, itemToFull iid s, kit)
        | (iid, kit) <- EM.assocs bag]
      benCStore cs = ben (getBodyStoreBag b cs s) cs
  in concatMap benCStore cstores

hinders :: Bool -> Bool -> Ability.Skills -> ItemFull -> Bool
hinders condShineWouldBetray uneasy actorMaxSk itemFull =
  let arItem = aspectRecordFull itemFull
      itemShine = 0 < IA.getSkill Ability.SkShine arItem
      -- @condAnyFoeAdj@ is not checked, because it's transient and also item
      -- management is unlikely to happen during melee, anyway
      itemShineBad = condShineWouldBetray && itemShine
  in -- In the presence of enemies (seen, remembered or unseen but distressing)
     -- actors want to hide in the dark.
     uneasy && itemShineBad  -- even if it's a weapon, take it off
     -- Fast actors want to hit hard, because they hit much more often
     -- than receive hits.
     || gearSpeed actorMaxSk > speedWalk
        && not (IA.checkFlag Ability.Meleeable arItem)
             -- in case it's the only weapon
        && 0 > IA.getSkill Ability.SkHurtMelee arItem

-- | Require that the actor stands over a desirable item.
condDesirableFloorItemM :: MonadClientRead m => ActorId -> m Bool
condDesirableFloorItemM aid = not . null <$> benGroundItems aid

-- | Produce the list of items on the ground beneath the actor
-- that are worth picking up.
benGroundItems :: MonadClientRead m
               => ActorId
               -> m [(Benefit, CStore, ItemId, ItemFull, ItemQuant)]
benGroundItems aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  discoBenefit <- getsClient sdiscoBenefit
  let canEsc = fcanEscape (gplayer fact)
      isDesirable (ben, _, _, itemFull, _) =
        desirableItem cops canEsc (benPickup ben)
                      (aspectRecordFull itemFull) (itemKind itemFull)
                      99  -- fake, because no time is wasted walking to item
  filter isDesirable
    <$> getsState (benAvailableItems discoBenefit aid [CGround])

desirableItem :: COps -> Bool -> Double -> IA.AspectRecord -> IK.ItemKind -> Int
              -> Bool
desirableItem COps{corule}
              canEsc benPickup arItem itemKind k =
  let loneProjectile =
        IK.isymbol itemKind == IK.rsymbolProjectile (RK.ritemSymbols corule)
        && k == 1
        && Dice.infDice (IK.icount itemKind) > 1
             -- never generated as lone; usually means weak
      useful = if canEsc
               then benPickup > 0
                    || IA.checkFlag Ability.Precious arItem
               else -- A hack to prevent monsters from picking up
                    -- treasure meant for heroes.
                 let preciousNotUseful = IA.isHumanTrinket itemKind
                 in benPickup > 0 && not preciousNotUseful
  in useful && not loneProjectile

condSupport :: MonadClientRead m
            => [(ActorId, Actor)] -> Int -> ActorId -> m Bool
{-# INLINE condSupport #-}
condSupport friendAssocs param aid = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  getsState $ strongSupport friendAssocs param aid mtgtMPath

strongSupport :: [(ActorId, Actor)]
              -> Int -> ActorId -> Maybe TgtAndPath -> State
              -> Bool
strongSupport friendAssocs param aid mtgtMPath s =
  -- The smaller the area scanned for friends, the lower number required.
  let actorMaxSkills = sactorMaxSkills s
      actorMaxSk = actorMaxSkills EM.! aid
      n = min 2 param - Ability.getSk Ability.SkAggression actorMaxSk
      b = getActorBody aid s
      approaching b2 = case mtgtMPath of
        Just TgtAndPath{tapTgt=TEnemy{},tapPath=Just AndPath{pathGoal}} ->
            chessDist (bpos b2) pathGoal <= 1 + param  -- will soon melee anyway
        _ -> False
      closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist > 0 && (dist <= max 2 param || approaching b2)
      closeAndStrong (aid2, b2) = closeEnough b2
                                  && actorCanMeleeToHarm actorMaxSkills aid2 b2
      closeAndStrongFriends = filter closeAndStrong friendAssocs
  in n <= 0 || not (null (drop (n - 1) closeAndStrongFriends))
       -- optimized: length closeAndStrongFriends >= n

-- The numbers reflect fleeing AI conditions for non-aggresive actors
-- so that actors don't wait for support that is not possible due to not
-- enough friends on the level, even counting sleeping ones.
condAloneM :: MonadStateRead m => [(ActorId, Actor)] -> ActorId -> m Bool
condAloneM friendAssocs aid = do
  b <- getsState $ getActorBody aid
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let onStashLevel = case mstash of
        Nothing -> False
        Just (lid, _) -> lid == blid b
  return $! length friendAssocs <= if onStashLevel then 3 else 2

-- | Require that the actor stands in the dark and so would be betrayed
-- by his own equipped light,
condShineWouldBetrayM :: MonadStateRead m => ActorId -> m Bool
condShineWouldBetrayM aid = do
  b <- getsState $ getActorBody aid
  aInAmbient <- getsState $ actorInAmbient b
  return $ not aInAmbient  -- tile is dark, so actor could hide

-- | Produce a list of acceptable adjacent points to flee to.
fleeList :: MonadClientRead m
         => [(ActorId, Actor)] -> ActorId -> m ([(Int, Point)], [(Int, Point)])
fleeList foeAssocs aid = do
  COps{coTileSpeedup} <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  -- Prefer fleeing along the path to target, unless the target is a foe,
  -- in which case flee in the opposite direction.
  let etgtPath = case mtgtMPath of
        Just TgtAndPath{ tapPath=Just AndPath{pathList, pathGoal}
                       , tapTgt } -> case tapTgt of
          TEnemy{} -> Left pathGoal
          TPoint TEnemyPos{} _ _ -> Left pathGoal
            -- this is too weak, because only one is recorded and sometimes
            -- many are needed to decide to flee next turn as well
          _ -> Right pathList
        _ -> Right []
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  localTime <- getsState $ getLocalTime (blid b)
  fleeD <- getsClient sfleeD
  -- But if fled recently, prefer even more fleeing further this turn.
  let eOldFleeOrTgt = case EM.lookup aid fleeD of
        Just (fleeStart, time) | timeRecent5 localTime time -> Left fleeStart
        _ -> etgtPath
      myVic = vicinityUnsafe $ bpos b
      dist p | null foeAssocs = 100
             | otherwise = minimum $ map (chessDist p . bpos . snd) foeAssocs
      dVic = map (dist &&& id) myVic
      -- Flee, if possible. Direct access required; not enough time to open.
      -- Can't be occupied.
      accWalkUnocc p = Tile.isWalkable coTileSpeedup (lvl `at` p)
                       && not (occupiedBigLvl p lvl)
                       && not (occupiedProjLvl p lvl)
      accWalkVic = filter (accWalkUnocc . snd) dVic
      gtVic = filter ((> dist (bpos b)) . fst) accWalkVic
      eqVicRaw = filter ((== dist (bpos b)) . fst) accWalkVic
      (eqVicOld, eqVic) = partition ((== boldpos b) . Just . snd) eqVicRaw
      accNonWalkUnocc p = not (Tile.isWalkable coTileSpeedup (lvl `at` p))
                          && Tile.isEasyOpen coTileSpeedup (lvl `at` p)
                          && not (occupiedBigLvl p lvl)
                          && not (occupiedProjLvl p lvl)
      accNonWalkVic = filter (accNonWalkUnocc . snd) dVic
      gtEqNonVic = filter ((>= dist (bpos b)) . fst) accNonWalkVic
      ltAllVic = filter ((< dist (bpos b)) . fst) dVic
      rewardPath mult (d, p) = case eOldFleeOrTgt of
        Right tgtPathList | p `elem` tgtPathList ->
          (100 * mult * d, p)
        Right tgtPathList | any (adjacent p) tgtPathList ->
          (10 * mult * d, p)
        Left pathGoal | bpos b /= pathGoal ->
          let venemy = towards (bpos b) pathGoal
              vflee = towards (bpos b) p
              sq = euclidDistSqVector venemy vflee
              skew = case compare sq 2 of
                GT -> 100 * sq
                EQ -> 10 * sq
                LT -> sq  -- going towards enemy (but may escape adjacent foes)
          in (mult * skew * d, p)
        _ -> (mult * d, p)  -- far from target path or even on target goal
      goodVic = map (rewardPath 10000) gtVic
                ++ map (rewardPath 100) eqVic
      badVic = map (rewardPath 1) $ gtEqNonVic ++ eqVicOld ++ ltAllVic
  return (goodVic, badVic)
