-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionClient
  ( condTgtEnemyPresentM
  , condAnyFoeAdjacentM
  , condHpTooLowM
  , condOnTriggerableM
  , condEnemiesCloseM
  , condNoFriendsM
  , condBlocksFriendsM
  , condNoWeaponM
  , condWeaponAvailableM
  ) where

import qualified Data.EnumMap.Strict as EM
import Data.Maybe

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
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
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ filter (not . actorDying)
                         . actorNotProjList (isAtWar fact) (blid b)
  return $! any (adjacent (bpos b) . bpos) allFoes

-- | Require the actor's HP is low enough.
condHpTooLowM :: MonadStateRead m => ActorId -> m Bool
condHpTooLowM aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let kind = okind $ bkind b
  return $! bhp b == 1 || 5 * bhp b < Dice.maxDice (ahp kind)

condOnTriggerableM :: MonadStateRead m => ActorId -> m Bool
condOnTriggerableM aid = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
  return $! not $ null $ Tile.causeEffects cotile t

condEnemiesCloseM :: MonadStateRead m => ActorId -> m Bool
condEnemiesCloseM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ filter (not . actorDying)
                         . actorNotProjList (isAtWar fact) (blid b)
  return $! any ((< nearby) . chessDist (bpos b) . bpos) allFoes

condNoFriendsM :: MonadStateRead m => ActorId -> m Bool
condNoFriendsM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorNotProjList friendlyFid (blid b)
  let notCloseEnough b2 = chessDist (bpos b) (bpos b2) `notElem` [1, 2]
  return $! all notCloseEnough friends

condBlocksFriendsM :: MonadClient m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  ours <- getsState $ actorNotProjAssocs (== bfid b) (blid b)
  targetD <- getsClient stargetD
  let blocked (aid2, _) = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just (_, Just (_ : q : _, _)) | q == bpos b -> True
          _ -> False
  return $! any blocked ours

condNoWeaponM :: MonadStateRead m => ActorId -> m Bool
condNoWeaponM aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  eqpAssocs <- getsState $ getEqpAssocs b
  return $! isNothing $ strongestSword cops eqpAssocs

condWeaponAvailableM :: MonadStateRead m => ActorId -> m Bool
condWeaponAvailableM aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  floorAssocs <- getsState $ getFloorAssocs (blid b) (bpos b)
  invAssocs <- getsState $ getInvAssocs b
  let lootIsWeapon = isJust $ strongestSword cops floorAssocs
      weaponinInv = isJust $ strongestSword cops invAssocs
  return $! lootIsWeapon || weaponinInv
