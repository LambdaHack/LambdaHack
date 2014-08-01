-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getLevel, nUI, posOfAid, factionCanEscape, factionLoots
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
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
  return $! length $ filter (fhasUI . gplayer) $ EM.elems factionD

posOfAid :: MonadStateRead m => ActorId -> m (LevelId, Point)
posOfAid aid = do
  b <- getsState $ getActorBody aid
  return (blid b, bpos b)

factionCanEscape :: MonadStateRead m => FactionId -> m Bool
factionCanEscape fid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  dungeon <- getsState sdungeon
  let escape = any lescape $ EM.elems dungeon
  return $! escape && fcanEscape (gplayer fact)

-- TODO: "treasure" is hardwired; tie this code with calculateTotal
factionLoots :: MonadStateRead m => FactionId -> m Bool
factionLoots fid = do
  dungeon <- getsState sdungeon
  let hasTreasure Level{litemFreq} =
        maybe False (> 0) $ lookup "treasure" litemFreq
      loots = any hasTreasure $ EM.elems dungeon
  canEscape <- factionCanEscape fid
  return $! canEscape && loots
