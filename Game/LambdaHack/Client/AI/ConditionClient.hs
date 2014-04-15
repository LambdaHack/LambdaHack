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
  , benAvailableItems
  , benGroundItems
  , threatDistList
  , fleeList
  ) where

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind

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

condFloorWeaponM :: MonadClient m => ActorId -> m Bool
condFloorWeaponM aid = do
  cops <- getsState scops
  disco <- getsClient sdisco
  b <- getsState $ getActorBody aid
  floorAssocs <- getsState $ getFloorAssocs (blid b) (bpos b)
  let lootIsWeapon = isJust $ strongestSword cops disco floorAssocs
  return $ lootIsWeapon  -- keep it lazy

condNoWeaponM :: MonadClient m => ActorId -> m Bool
condNoWeaponM aid = do
  cops <- getsState scops
  disco <- getsClient sdisco
  b <- getsState $ getActorBody aid
  eqpAssocs <- getsState $ getEqpAssocs b
  return $ isNothing $ strongestSword cops disco eqpAssocs  -- keep it lazy

condCanProjectM :: MonadClient m => ActorId -> m Bool
condCanProjectM aid = do
  Kind.COps{coactor=Kind.Ops{okind}, corule} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
      permitted = (if True  -- aiq mk >= 10 -- TODO; let server enforce?
                   then ritemProject
                   else ritemRanged)
                  $ Kind.stdRuleset corule
  benList <- benAvailableItems aid permitted
  let missiles = filter (maybe True (< 0) . fst . fst) benList
  return $ asight ak && calmEnough b ak && not (null missiles)  -- keep it lazy

benAvailableItems :: MonadClient m
            => ActorId -> [Char] -> m [((Maybe Int, CStore), (ItemId, Item))]
benAvailableItems aid permitted = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  disco <- getsClient sdisco
  itemD <- getsState sitemD
  let ak = okind $ bkind b
      getItemB iid =
        fromMaybe (assert `failure` "item body not found"
                          `twith` (iid, itemD)) $ EM.lookup iid itemD
      ben cstore bag =
        [ ((benefit, cstore), (iid, item))
        | (iid, item) <- map (\iid -> (iid, getItemB iid))
                         $ EM.keys bag
        , let benefit = case jkind disco item of
                Nothing -> Nothing
                Just _ki ->
                  let _kik = undefined -- iokind _ki
                      _unneeded = isymbol _kik
                  in Just $ effectToBenefit cops b (jeffect item)
        , benefit /= Just 0
        , jsymbol item `elem` permitted ]
  floorBag <- getsState $ getActorBag aid CGround
  eqpBag <- getsState $ getActorBag aid CEqp
  invBag <- if calmEnough b ak
            then getsState $ getActorBag aid CInv
            else return EM.empty
  return $ ben CGround floorBag ++ ben CEqp eqpBag ++ ben CInv invBag
    -- keep it lazy

condNotCalmEnoughM :: MonadClient m => ActorId -> m Bool
condNotCalmEnoughM aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
  return $! not (calmEnough b ak)

condDesirableFloorItemM :: MonadClient m => ActorId -> m Bool
condDesirableFloorItemM aid = do
  benItemL <- benGroundItems aid
  return $ not $ null benItemL  -- keep it lazy

benGroundItems :: MonadClient m => ActorId -> m [((Int, Int), (ItemId, Item))]
benGroundItems aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  itemD <- getsState sitemD
  disco <- getsClient sdisco
  floorItems <- getsState $ getActorBag aid CGround
  fightsSpawners <- fightsAgainstSpawners (bfid body)
  let itemUsefulness item =
        case jkind disco item of
          Nothing -> 5  -- experimenting is fun
          Just _ki -> effectToBenefit cops body $ jeffect item
      desirableItem item use k
        | fightsSpawners = use /= 0 || itemPrice (item, k) > 0
        | otherwise = use /= 0
      mapDesirable (iid, k) = let item = itemD EM.! iid
                                  use = itemUsefulness item
                              in if desirableItem item use k
                                 then Just ((use, k), (iid, item))
                                 else Nothing
  return $ reverse $ sort $ mapMaybe mapDesirable $ EM.assocs floorItems
    -- keep it lazy

condMeleeBadM :: MonadClient m => ActorId -> m Bool
condMeleeBadM aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  let closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist < 4 && dist > 0
      closeFriends = filter closeEnough friends
      strongCloseFriends = filter (not . hpTooLow coactor) closeFriends
      noFriendlyHelp = length closeFriends < 3 && null strongCloseFriends
      condHpTooLow = hpTooLow coactor b
  return $ noFriendlyHelp  -- still not getting friends' help
           && (condHpTooLow  -- too wounded to fight alone
               || length friends > 1)  -- friends somewhere, let's flee to them
    -- keep it lazy

condLightBetraysM :: MonadClient m => ActorId -> m Bool
condLightBetraysM aid = do
  cops@Kind.COps{cotile} <- getsState scops
  disco <- getsClient sdisco
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  eqpAss <- getsState $ getEqpAssocs b
  floorAss <- getsState $ getFloorAssocs (blid b) (bpos b)
  return $! not (Tile.isLit cotile (lvl `at` bpos b))     -- in the dark, but
            && (isJust (strongestBurn cops disco eqpAss)  -- betrayed by light
                || isJust (strongestBurn cops disco floorAss))

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
