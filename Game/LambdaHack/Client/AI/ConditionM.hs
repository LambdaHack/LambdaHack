-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionM
  ( condAimEnemyPresentM
  , condAimEnemyRememberedM
  , condTgtNonmovingM
  , condAnyFoeAdjM
  , condAdjTriggerableM
  , condBlocksFriendsM
  , condFloorWeaponM
  , condNoEqpWeaponM
  , condCanProjectM
  , condProjectListM
  , condDesirableFloorItemM
  , condSupport
  , benAvailableItems
  , hinders
  , benGroundItems
  , desirableItem
  , meleeThreatDistList
  , condShineWouldBetrayM
  , fleeList
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Ord

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
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

-- All conditions are (partially) lazy, because they are not always
-- used in the strict monadic computations they are in.

-- | Require that the target enemy is visible by the party.
condAimEnemyPresentM :: MonadClient m => ActorId -> m Bool
condAimEnemyPresentM aid = do
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TEnemy _ permit) -> not permit
    _ -> False

-- | Require that the target enemy is remembered on the actor's level.
condAimEnemyRememberedM :: MonadClient m => ActorId -> m Bool
condAimEnemyRememberedM aid = do
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  return $ case btarget of
    Just (TPoint (TEnemyPos _ permit) lid _) | lid == blid b -> not permit
    _ -> False

-- | Check if the target is nonmoving.
condTgtNonmovingM :: MonadClient m => ActorId -> m Bool
condTgtNonmovingM aid = do
  btarget <- getsClient $ getTarget aid
  case btarget of
    Just (TEnemy enemy _) -> do
      actorMaxSk <- maxActorSkillsClient enemy
      return $ EM.findWithDefault 0 Ability.AbMove actorMaxSk <= 0
    _ -> return False

-- | Require that any non-dying foe is adjacent, except projectiles
-- that (possibly) explode upon contact.
condAnyFoeAdjM :: MonadStateRead m => ActorId -> m Bool
condAnyFoeAdjM aid = getsState $ anyFoeAdj aid

-- | Require the actor stands adjacent to a triggerable tile (e.g., stairs).
condAdjTriggerableM :: MonadStateRead m => ActorId -> m Bool
condAdjTriggerableM aid = do
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let hasTriggerable p = p `EM.member` lembed lvl
  return $ any hasTriggerable $ vicinityUnsafe $ bpos b

-- | Produce the chess-distance-sorted list of non-low-HP,
-- melee-cabable foes on the level. We don't consider path-distance,
-- because we are interested in how soon the foe can close in to hit us,
-- which can diverge greately from path distance for short distances,
-- e.g., when terrain gets revealed. We don't consider non-moving actors,
-- because they can't chase us and also because they can't be aggresive
-- so to resolve the stalemate, the opposing AI has to be aggresive
-- by ignoring them and then when melee is started, it's usually too late
-- to retreat.
meleeThreatDistList :: MonadClient m => ActorId -> m [(Int, (ActorId, Actor))]
meleeThreatDistList aid = do
  actorAspect <- getsClient sactorAspect
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allAtWar <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let strongActor (aid2, b2) =
        let ar = actorAspect EM.! aid2
            actorMaxSkE = aSkills ar
            nonmoving = EM.findWithDefault 0 Ability.AbMove actorMaxSkE <= 0
        in not (hpTooLow b2 ar || nonmoving)
           && actorCanMelee actorAspect aid2 b2
      allThreats = filter strongActor allAtWar
      addDist (aid2, b2) = (chessDist (bpos b) (bpos b2), (aid2, b2))
  return $ sortBy (comparing fst) $ map addDist allThreats

-- | Require the actor blocks the paths of any of his party members.
condBlocksFriendsM :: MonadClient m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  ours <- getsState $ actorRegularIds (== bfid b) (blid b)
  targetD <- getsClient stargetD
  let blocked aid2 = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just TgtAndPath{tapPath=AndPath{pathList=q : _}} | q == bpos b -> True
          _ -> False
  return $ any blocked ours

-- | Require the actor stands over a weapon that would be auto-equipped.
condFloorWeaponM :: MonadClient m => ActorId -> m Bool
condFloorWeaponM aid = do
  floorAssocs <- getsState $ getActorAssocs aid CGround
  let lootIsWeapon = any (isMelee . snd) floorAssocs
  return lootIsWeapon

-- | Check whether the actor has no weapon in equipment.
condNoEqpWeaponM :: MonadClient m => ActorId -> m Bool
condNoEqpWeaponM aid = do
  eqpAssocs <- getsState $ getActorAssocs aid CEqp
  return $ all (not . isMelee . snd) eqpAssocs

-- | Require that the actor can project any items.
condCanProjectM :: MonadClient m => Int -> ActorId -> m Bool
condCanProjectM skill aid = do
  benList <- condProjectListM skill aid
  -- This is a simplified version of conditions in @projectItem@.
  -- In particular, range and charge are ignored, because they may change
  -- by the time the position for the fling is reached.
  let isMissile ((Nothing, _), (_, itemFull)) =
        -- Melee weapon is usually needed in hand.
        not (isMelee $ itemBase itemFull)
      isMissile ((Just (_, (_, effFoe)), _), (_, itemFull)) =
        effFoe < 0 && not (isMelee $ itemBase itemFull)
  return $ any isMissile benList

condProjectListM :: MonadClient m
                 => Int -> ActorId
                 -> m [( (Maybe (Int, (Int, Int)), (Int, CStore))
                       , (ItemId, ItemFull) )]
condProjectListM skill aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
             Just aspectRecord -> aspectRecord
             Nothing -> assert `failure` aid
      calmE = calmEnough b ar
      q = permittedProjectAI skill calmE
  benAvailableItems aid q $ [CEqp, CInv, CGround] ++ [CSha | calmE]

-- | Produce the list of items with a given property available to the actor
-- and the items' values.
benAvailableItems :: MonadClient m
                  => ActorId
                  -> (ItemFull -> Bool)
                  -> [CStore]
                  -> m [( (Maybe (Int, (Int, Int)), (Int, CStore))
                        , (ItemId, ItemFull) )]
benAvailableItems aid permitted cstores = do
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
  actorAspect <- getsClient sactorAspect
  discoBenefit <- getsClient sdiscoBenefit
  s <- getState
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      condNotCalmEnough = not (calmEnough b ar)
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltaSerious (bcalmDelta b)
      ben cstore bag =
        [ ((benefit, (k, cstore)), (iid, itemFull))
        | (iid, kit@(k, _)) <- EM.assocs bag
        , let itemFull = itemToF iid kit
              benefit = EM.lookup iid discoBenefit
              hind = hinders condShineWouldBetray condAimEnemyPresent
                             heavilyDistressed condNotCalmEnough
                             b ar itemFull
        , permitted itemFull && (cstore /= CEqp || hind) ]
      benCStore cs = ben cs $ getBodyStoreBag b cs s
  return $ concatMap benCStore cstores

hinders :: Bool -> Bool -> Bool -> Bool -> Actor -> AspectRecord -> ItemFull
        -> Bool
hinders condShineWouldBetray condAimEnemyPresent
        heavilyDistressed condNotCalmEnough
          -- guess that enemies have projectiles and used them now or recently
        body ar itemFull =
  let itemShine = 0 < aShine (aspectRecordFull itemFull)
      -- @condAnyFoeAdj@ is not checked, because it's transient and also item
      -- management is unlikely to happen during melee, anyway
      itemShineBad = condShineWouldBetray && itemShine
  in -- In the presence of enemies (seen, or unseen but distressing)
     -- actors want to hide in the dark.
     (condAimEnemyPresent || condNotCalmEnough || heavilyDistressed)
     && itemShineBad
     -- Fast actors want to hit hard, because they hit much more often
     -- than receive hits.
     || bspeed body ar > speedWalk
        && 0 > aHurtMelee (aspectRecordFull itemFull)

-- | Require that the actor stands over a desirable item.
condDesirableFloorItemM :: MonadClient m => ActorId -> m Bool
condDesirableFloorItemM aid = do
  benItemL <- benGroundItems aid
  return $ not $ null benItemL

-- | Produce the list of items on the ground beneath the actor
-- that are worth picking up.
benGroundItems :: MonadClient m
               => ActorId
               -> m [( (Maybe (Int, (Int, Int))
                     , (Int, CStore)), (ItemId, ItemFull) )]
benGroundItems aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let canEsc = fcanEscape (gplayer fact)
      isDesirable ((Nothing, _), (_, itemFull)) =
        desirableItem canEsc Nothing (itemBase itemFull)
      isDesirable ((Just (totalSum, _), _), (_, itemFull)) =
        desirableItem canEsc (Just totalSum) (itemBase itemFull)
  benList <- benAvailableItems aid (const True) [CGround]
  return $ filter isDesirable benList

desirableItem :: Bool -> Maybe Int -> Item -> Bool
desirableItem canEsc use item =
  if canEsc
  then use /= Just 0
       || IK.Precious `elem` jfeature item
  else
    -- A hack to prevent monsters from picking up treasure meant for heroes.
    let preciousNotUseful =  -- suspect and probably useless jewelry
          IK.Precious `elem` jfeature item  -- risk from treasure hunters
          && IK.Equipable `notElem` jfeature item  -- can't wear
    in use /= Just 0
       && not preciousNotUseful  -- hack for elixir: even if @use@ positive

condSupport :: MonadClient m => Int -> ActorId -> m Bool
condSupport param aid = do
  actorAspect <- getsClient sactorAspect
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  mtgtPos <- case btarget of
    Nothing -> return Nothing
    Just target -> aidTgtToPos aid (blid b) target
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condAimEnemyRemembered <- condAimEnemyRememberedM aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
      ar = actorAspect EM.! aid
  friends <- getsState $ actorRegularAssocs friendlyFid (blid b)
  let approaching = case mtgtPos of
        Just tgtPos | condAimEnemyPresent || condAimEnemyRemembered ->
          \b2 -> chessDist (bpos b2) tgtPos <= 1 + param
        _ -> const False
      closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist > 0 && (dist <= param || approaching b2)
      closeAndStrong (aid2, b2) = closeEnough b2
                                  && actorCanMelee actorAspect aid2 b2
      closeAndStrongFriends = filter closeAndStrong friends
      -- The smaller area scanned for friends, the lower number required.
      suport = length closeAndStrongFriends >= param - aAggression ar
               || length friends <= 1  -- solo fighters aggresive
  return suport

-- | Require that the actor stands in the dark and so would be betrayed
-- by his own equipped light,
condShineWouldBetrayM :: MonadClient m => ActorId -> m Bool
condShineWouldBetrayM aid = do
  b <- getsState $ getActorBody aid
  aInAmbient <- getsState $ actorInAmbient b
  return $ not aInAmbient  -- tile is dark, so actor could hide

-- | Produce a list of acceptable adjacent points to flee to.
fleeList :: MonadClient m => ActorId -> m ([(Int, Point)], [(Int, Point)])
fleeList aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  -- Prefer fleeing along the path to target, unless the target is a foe,
  -- in which case flee in the opposite direction.
  let etgtPath = case mtgtMPath of
        Just TgtAndPath{tapPath=AndPath{pathList}, tapTgt} -> case tapTgt of
          TEnemy{} -> Left pathList
          TPoint TEnemyPos{} _ _ -> Left pathList
          _ -> Right pathList
        _ -> Right []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  s <- getState
  let posFoes = map bpos allFoes
      myVic = vicinity lxsize lysize $ bpos b
      dist p | null posFoes = 100
             | otherwise = minimum $ map (chessDist p) posFoes
      dVic = map (dist &&& id) myVic
      -- Flee, if possible. Direct access required; not enough time to open.
      -- Can't be occupied.
      accUnocc p = Tile.isWalkable coTileSpeedup (lvl `at` p)
                   && null (posToAssocs p (blid b) s)
      accVic = filter (accUnocc . snd) dVic
      gtVic = filter ((> dist (bpos b)) . fst) accVic
      eqVic = filter ((== dist (bpos b)) . fst) accVic
      ltVic = filter ((< dist (bpos b)) . fst) accVic
      rewardPath mult (d, p) = case etgtPath of
        Right tgtPath | p `elem` tgtPath ->
          (100 * mult * d, p)
        Right tgtPath | any (\q -> adjacent p q) tgtPath ->
          (10 * mult * d, p)
        Left tgtPath@(_ : _) ->
          let venemy = towards (bpos b) (last tgtPath)
              vflee = towards (bpos b) p
              sq = euclidDistSqVector venemy vflee
              skew = case compare sq 2 of
                GT -> 100 * sq
                EQ -> 10 * sq
                LT -> sq  -- going towards enemy (but may escape adjacent foes)
          in (mult * skew * d, p)
        _ -> (mult * d, p)
      goodVic = map (rewardPath 10000) gtVic
                ++ map (rewardPath 100) eqVic
      badVic = map (rewardPath 1) ltVic
  return (goodVic, badVic)
