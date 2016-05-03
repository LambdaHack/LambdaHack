-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getLevel, nUI, posOfAid, factionCanEscape
  , getGameMode, isNoConfirmsGame, getEntryArena
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
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
  let escape = any (not . null . lescape) $ EM.elems dungeon
  return $! escape && fcanEscape (gplayer fact)

getGameMode :: MonadStateRead m => m ModeKind
getGameMode = do
  Kind.COps{comode=Kind.Ops{okind}} <- getsState scops
  t <- getsState sgameModeId
  return $! okind t

isNoConfirmsGame :: MonadStateRead m => m Bool
isNoConfirmsGame = do
  gameMode <- getGameMode
  return $! maybe False (> 0) $ lookup "no confirms" $ mfreq gameMode

getEntryArena :: MonadStateRead m => Faction -> m LevelId
getEntryArena fact = do
  dungeon <- getsState sdungeon
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "empty dungeon" `twith` dungeon
  return $! max minD $ min maxD $ toEnum $ fentryLevel $ gplayer fact
