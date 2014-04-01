-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getLevel, nUI, posOfContainer, posOfAid, actorConts, fightsAgainstSpawners
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class (Monad m, Functor m) => MonadStateRead m where
  getState  :: m State
  getsState :: (State -> a) -> m a

getLevel :: MonadStateRead m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadStateRead m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $! length $ filter (playerUI . gplayer) $ EM.elems factionD

posOfAid :: MonadStateRead m => ActorId -> m (LevelId, Point)
posOfAid aid = do
  b <- getsState $ getActorBody aid
  return (blid b, bpos b)

posOfContainer :: MonadStateRead m => Container -> m (LevelId, Point)
posOfContainer (CFloor lid p) = return (lid, p)
posOfContainer (CActor aid _) = posOfAid aid

actorInvs :: MonadStateRead m
          => ItemId -> Int -> ActorId -> m [(Int, ActorId)]
actorInvs iid k aid = do
  let takeFromInv :: Int -> [(ActorId, Actor)] -> [(Int, ActorId)]
      takeFromInv 0 _ = []
      takeFromInv _ [] = assert `failure` (iid, k, aid)
      takeFromInv n ((aid2, b2) : as) =
        case EM.lookup iid $ binv b2 of
          Nothing -> takeFromInv n as
          Just m -> let ck = min n m
                    in (ck, aid2) : takeFromInv (n - ck) as
  b <- getsState $ getActorBody aid
  as <- getsState $ fidActorNotProjAssocs (bfid b)
  return $ takeFromInv k $ (aid, b) : filter ((/= aid) . fst) as

actorConts :: MonadStateRead m
           => ItemId -> Int -> ActorId -> CStore
           -> m [(Int, Container)]
actorConts iid k aid cstore = case cstore of
  CGround -> return [(k, CActor aid CGround)]
  CEqp -> return [(k, CActor aid CEqp)]
  CInv -> do
    invs <- actorInvs iid k aid
    return $! map (\(n, aid2) -> (n, CActor aid2 CInv)) invs

-- TODO: make a field of Faction?
fightsAgainstSpawners :: MonadStateRead m => FactionId -> m Bool
fightsAgainstSpawners fid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  dungeon <- getsState sdungeon
  let escape = any lescape $ EM.elems dungeon
      isSpawner = isSpawnFact fact
  return $! escape && not isSpawner
