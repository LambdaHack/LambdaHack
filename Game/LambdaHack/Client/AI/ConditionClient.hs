-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionClient
  ( condTgtEnemyPresentM
  , condAnyFoeAdjacentM
  , condThreatAdjacentM
  , condHpTooLowM
  , condOnTriggerableM
  , condThreatCloseM
  , condNoFriendsM
  , condBlocksFriendsM
  , condWeaponAvailableM
  , condNoWeaponM
  , condCannotProjectM
  , condNotCalmEnoughM
  , benefitList
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
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
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind

-- | Require that the target enemy is visible by the party.
condTgtEnemyPresentM :: MonadClient m => ActorId -> m Bool
condTgtEnemyPresentM aid = do
  btarget <- getsClient $ getTarget aid
  let mfAid =
        case btarget of
          Just (TEnemy enemyAid _) -> Just enemyAid
          _ -> Nothing
  return $! isJust mfAid

-- | Require that any non-dying foe is adjacent.
condAnyFoeAdjacentM :: MonadStateRead m => ActorId -> m Bool
condAnyFoeAdjacentM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  return $! any (adjacent (bpos b) . bpos) allFoes

-- | Require that any non-low-HP foe is adjacent.
condThreatAdjacentM :: MonadStateRead m => ActorId -> m Bool
condThreatAdjacentM aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ filter (not . hpTooLow coactor)
                         . actorRegularList (isAtWar fact) (blid b)
  return $! any (adjacent (bpos b) . bpos) allFoes

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

-- | Require that any non-low-HP foe is nearby.
condThreatCloseM :: MonadStateRead m => ActorId -> m Bool
condThreatCloseM aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ filter (not . hpTooLow coactor)
                         . actorRegularList (isAtWar fact) (blid b)
  return $! any ((< nearby) . chessDist (bpos b) . bpos) allFoes

-- Don't care if the friends low-hp or not.
condNoFriendsM :: MonadStateRead m => ActorId -> m Bool
condNoFriendsM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  let notCloseEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                          in dist < 4 && dist > 0
  return $! all notCloseEnough friends

condBlocksFriendsM :: MonadClient m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  ours <- getsState $ actorRegularAssocs (== bfid b) (blid b)
  targetD <- getsClient stargetD
  let blocked (aid2, _) = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just (_, Just (_ : q : _, _)) | q == bpos b -> True
          _ -> False
  return $! any blocked ours

condWeaponAvailableM :: MonadStateRead m => ActorId -> m Bool
condWeaponAvailableM aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  floorAssocs <- getsState $ getFloorAssocs (blid b) (bpos b)
  invAssocs <- getsState $ getInvAssocs b
  let lootIsWeapon = isJust $ strongestSword cops floorAssocs
      weaponinInv = isJust $ strongestSword cops invAssocs
  return $! lootIsWeapon || weaponinInv

condNoWeaponM :: MonadStateRead m => ActorId -> m Bool
condNoWeaponM aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  eqpAssocs <- getsState $ getEqpAssocs b
  return $! isNothing $ strongestSword cops eqpAssocs

condCannotProjectM :: MonadClient m => ActorId -> m Bool
condCannotProjectM aid = do
  Kind.COps{coactor=Kind.Ops{okind}, corule} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
      permitted = (if True  -- aiq mk >= 10 -- TODO; let server enforce?
                   then ritemProject
                   else ritemRanged)
                  $ Kind.stdRuleset corule
  benList <- benefitList aid permitted
  let missiles = filter (maybe True (< 0) . fst . fst) benList
  return $! not (asight ak) || not (calmEnough b ak) || null missiles

benefitList :: MonadClient m
            => ActorId -> [Char] -> m [((Maybe Int, CStore), (ItemId, Item))]
benefitList aid permitted = do
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
  return $! ben CGround floorBag ++ ben CEqp eqpBag ++ ben CInv invBag

condNotCalmEnoughM :: MonadClient m => ActorId -> m Bool
condNotCalmEnoughM aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let ak = okind $ bkind b
  return $! not (calmEnough b ak)
