-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionClient
  ( condTgtEnemyPresentM
  , condTgtEnemyRememberedM
  , condAnyFoeAdjM
  , condHpTooLowM
  , condOnTriggerableM
  , condBlocksFriendsM
  , condFloorWeaponM
  , condNoWeaponM
  , condCanProjectM
  , condNotCalmEnoughM
  , condDesirableFloorItemM
  , condMeleeBadM
  , condLightBetraysM
  , maxUsefulness
  , benAvailableItems
  , benGroundItems
  , threatDistList
  , fleeList
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector

-- | Require that the target enemy is visible by the party.
condTgtEnemyPresentM :: MonadClient m => ActorId -> m Bool
condTgtEnemyPresentM aid = do
  btarget <- getsClient $ getTarget aid
  return $! case btarget of
    Just TEnemy{} -> True
    _ -> False

-- | Require that the target enemy is remembered on the actor's level.
condTgtEnemyRememberedM :: MonadClient m => ActorId -> m Bool
condTgtEnemyRememberedM aid = do
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  return $! case btarget of
    Just (TEnemyPos _ lid _ _) | lid == blid b -> True
    _ -> False

-- | Require that any non-dying foe is adjacent.
condAnyFoeAdjM :: MonadStateRead m => ActorId -> m Bool
condAnyFoeAdjM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  return $ any (adjacent (bpos b) . bpos) allFoes  -- keep it lazy

-- | Require the actor's HP is low enough.
condHpTooLowM :: MonadClient m => ActorId -> m Bool
condHpTooLowM aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  return $! hpTooLow coactor b

-- | Require the actor stands over a triggerable tile.
condOnTriggerableM :: MonadStateRead m => ActorId -> m Bool
condOnTriggerableM aid = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
  return $! not $ null $ Tile.causeEffects cotile t

-- | Produce the chess-distance-sorted list of non-low-HP foes on the level.
-- We don't consider path-distance, because we are interested in how soon
-- the foe can hit us, which can diverge greately from path distance
-- for short distances.
threatDistList :: MonadStateRead m => ActorId -> m [(Int, (ActorId, Actor))]
threatDistList aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allThreats <- getsState $ filter (not . hpTooLow coactor . snd)
                          . actorRegularAssocs (isAtWar fact) (blid b)
  let addDist (aid2, b2) = (chessDist (bpos b) (bpos b2), (aid2, b2))
  return $ sort $ map addDist allThreats

-- | Require the actor blocks the paths of any of his party members.
condBlocksFriendsM :: MonadClient m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  ours <- getsState $ actorRegularAssocs (== bfid b) (blid b)
  targetD <- getsClient stargetD
  let blocked (aid2, _) = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just (_, Just (_ : q : _, _)) | q == bpos b -> True
          _ -> False
  return $ any blocked ours  -- keep it lazy

-- | Require the actor stands over a weapon.
condFloorWeaponM :: MonadClient m => ActorId -> m Bool
condFloorWeaponM aid = do
  floorAssocs <- fullAssocsClient aid [CGround]
  -- We do consider OFF weapons, because e.g., enemies might have turned
  -- them off or they can be wrong for other party members, but are OK for us.
  let lootIsWeapon =
        not $ null $ strongestSlot IF.EqpSlotWeapon floorAssocs
  return $ lootIsWeapon  -- keep it lazy

-- | Require the actor doesn't stand over a weapon, unless it's deactivated.
condNoWeaponM :: MonadClient m => ActorId -> m Bool
condNoWeaponM aid = do
  allAssocs <- fullAssocsClient aid [CEqp]
  -- We do not consider OFF weapons, because they apparently are not good.
  return $ null $ strongestSlot IF.EqpSlotWeapon allAssocs
    -- keep it lazy

-- | Require that the actor can project any items.
condCanProjectM :: MonadClient m => ActorId -> m Bool
condCanProjectM aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
  actorBlind <- radiusBlind <$> sumBodyEqpClient IF.EqpSlotSightRadius aid
  benList <- benAvailableItems aid permittedRanged
  let missiles = filter (maybe True (< 0) . fst . fst) benList
  return $ not actorBlind && calmEnough b ak && not (null missiles)
    -- keep it lazy

-- | Produce the benefit-sorted list of items with a given symbol
-- available to the actor.
benAvailableItems :: MonadClient m
                  => ActorId -> (ItemFull -> Bool)
                  -> m [((Maybe Int, CStore), (ItemId, ItemFull))]
benAvailableItems aid permitted = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
      ben cstore bag =
        [ ((benefit, cstore), (iid, itemFull))
        | (iid, k) <- EM.assocs bag
        , let itemFull = itemToF iid k
        , let benefit = maxUsefulness cops b itemFull
        , benefit /= Just 0
        , permitted itemFull ]
  groundBag <- getsState $ getActorBag aid CGround
  eqpBag <- getsState $ getActorBag aid CEqp
  invBag <- getsState $ getActorBag aid CInv
  shaBag <- if calmEnough b ak
            then getsState $ getActorBag aid CSha
            else return EM.empty
  return $ ben CGround groundBag
         ++ ben CEqp eqpBag
         ++ ben CInv invBag
         ++ ben CSha shaBag
    -- keep it lazy

-- | Require the actor is not calm enough.
condNotCalmEnoughM :: MonadClient m => ActorId -> m Bool
condNotCalmEnoughM aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
  return $! not (calmEnough b ak)

-- | Require that the actor stands over a desirable item.
condDesirableFloorItemM :: MonadClient m => ActorId -> m Bool
condDesirableFloorItemM aid = do
  benItemL <- benGroundItems aid
  return $ not $ null benItemL  -- keep it lazy

-- | Produce the benefit-sorted list of items on the ground beneath the actor.
benGroundItems :: MonadClient m => ActorId -> m [((Int, Int), (ItemId, Item))]
benGroundItems aid = do
  cops <- getsState scops
  itemToF <- itemToFullClient
  body <- getsState $ getActorBody aid
  itemD <- getsState sitemD
  floorItems <- getsState $ getActorBag aid CGround
  fightsSpawners <- fightsAgainstSpawners (bfid body)
  let desirableItem item use
        | fightsSpawners = use /= Just 0 || IF.Precious `elem` jfeature item
        | otherwise = use /= Just 0
      mapDesirable (iid, k) =
        let item = itemD EM.! iid
            use = maxUsefulness cops body (itemToF iid k)
            value = fromMaybe 5 use  -- experimenting fun
        in if desirableItem item use
           then Just ((value, k), (iid, item))
           else Nothing
  return $ reverse $ sort $ mapMaybe mapDesirable $ EM.assocs floorItems
    -- keep it lazy

-- | Determine the maximum absolute benefit from an item.
maxUsefulness :: Kind.COps -> Actor -> ItemFull -> Maybe Int
maxUsefulness cops body itemFull = do
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jaspects, jeffects}} ->
      Just $ effAspToBenefit cops body jeffects jaspects
    _ -> Nothing

-- | Require the actor is in a bad position to melee.
-- We do not check if the actor has a weapon, because having
-- no innate weapon is rare.
condMeleeBadM :: MonadClient m => ActorId -> m Bool
condMeleeBadM aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  mleader <- getsClient _sleader
  actorSk <- actorSkills aid mleader
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  let closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist < 3 && dist > 0
      closeFriends = filter closeEnough friends
      strongCloseFriends = filter (not . hpTooLow coactor) closeFriends
      noFriendlyHelp = length closeFriends < 3 && null strongCloseFriends
  return $ EM.findWithDefault 0 Ability.AbMelee actorSk <= 0
           || noFriendlyHelp  -- still not getting friends' help
    -- no $!; keep it lazy

-- | Require that the actor stands in the dark, but is betrayed
-- by his own light,
condLightBetraysM :: MonadClient m => ActorId -> m Bool
condLightBetraysM aid = do
  b <- getsState $ getActorBody aid
  aShines <- getsState $ actorShines b
  aInAmbient<- getsState $ actorInAmbient b
  return $! not aInAmbient  -- tile is dark, so actor could hide
            && aShines      -- but actor betrayed by his light

-- | Produce a list of acceptable adjacent points to flee to.
fleeList :: MonadClient m => ActorId -> m [(Int, Point)]
fleeList aid = do
  cops <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer fleeing along the path to target
        Just (_, Just (_ : path, _)) -> path
        _ -> []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let posFoes = map bpos allFoes
      accessibleHere = accessible cops lvl $ bpos b
      myVic = vicinity lxsize lysize $ bpos b
      dist p | null posFoes = assert `failure` b
             | otherwise = minimum $ map (chessDist p) posFoes
      dVic = map (dist &&& id) myVic
      -- Flee, if possible. Access required.
      accVic = filter (accessibleHere . snd) $ dVic
      gtVic = filter ((> dist (bpos b)) . fst) accVic
      -- At least don't get closer to enemies, but don't stay adjacent.
      eqVic = filter (\(d, _) -> d == dist (bpos b) && d > 1) accVic
      rewardPath (d, p) =
        if p `elem` tgtPath then Just (9 * d, p)
        else if any (\q -> chessDist p q == 1) tgtPath then Just (d, p)
        else Nothing
      pathVic = mapMaybe rewardPath gtVic
                ++ filter ((`elem` tgtPath) . snd) eqVic
  return pathVic  -- keep it lazy, until other conditions verify danger
