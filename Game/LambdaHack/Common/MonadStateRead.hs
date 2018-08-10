{-# LANGUAGE TupleSections #-}
-- | Game state reading monad and basic operations.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getState, getLevel, nUI
  , getGameMode, isNoConfirmsGame, getEntryArena, pickWeaponM
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.ModeKind

-- | Monad for reading game state. A state monad with state modification
-- disallowed (another constraint is needed to permit that).
-- The basic server and client monads are like that, because server
-- and clients freely modify their internal session data, but don't modify
-- the main game state, except in very restricted and synchronized way.
class (Monad m, Functor m, Applicative m) => MonadStateRead m where
  getsState :: (State -> a) -> m a

getState :: MonadStateRead m => m State
getState = getsState id

getLevel :: MonadStateRead m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadStateRead m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $! length $ filter (fhasUI . gplayer) $ EM.elems factionD

getGameMode :: MonadStateRead m => m ModeKind
getGameMode = do
  COps{comode} <- getsState scops
  t <- getsState sgameModeId
  return $! okind comode t

isNoConfirmsGame :: MonadStateRead m => m Bool
isNoConfirmsGame = do
  gameMode <- getGameMode
  return $! maybe False (> 0) $ lookup "no confirms" $ mfreq gameMode

getEntryArena :: MonadStateRead m => Faction -> m LevelId
getEntryArena fact = do
  dungeon <- getsState sdungeon
  let (minD, maxD) = dungeonBounds dungeon
      f [] = 0
      f ((ln, _, _) : _) = ln
  return $! max minD $ min maxD $ toEnum $ f $ ginitial fact

pickWeaponM :: MonadStateRead m
            => Maybe DiscoveryBenefit
            -> [(ItemId, ItemFullKit)] -> Ability.Skills -> ActorId
            -> m [(Double, (ItemId, ItemFullKit))]
pickWeaponM mdiscoBenefit kitAss actorSk source = do
  sb <- getsState $ getActorBody source
  localTime <- getsState $ getLocalTime (blid sb)
  actorMaxSk <- getsState $ getActorMaxSkills source
  let calmE = calmEnough sb actorMaxSk
      forced = bproj sb
      permitted = permittedPrecious forced calmE
      preferredPrecious = either (const False) id . permitted
      permAssocs = filter (preferredPrecious . fst . snd) kitAss
      strongest = strongestMelee mdiscoBenefit localTime permAssocs
  return $! if | forced -> map (1,) kitAss
               | Ability.getSk Ability.SkMelee actorSk <= 0 -> []
               | otherwise -> strongest
