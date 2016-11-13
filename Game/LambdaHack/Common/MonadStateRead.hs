-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the main action type.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getLevel, nUI
  , getGameMode, isNoConfirmsGame, getEntryArena, pickWeaponM
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class (Monad m, Functor m, Applicative m) => MonadStateRead m where
  getState  :: m State
  getsState :: (State -> a) -> m a

getLevel :: MonadStateRead m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadStateRead m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $! length $ filter (fhasUI . gplayer) $ EM.elems factionD

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

pickWeaponM :: MonadStateRead m
            => [(ItemId, ItemFull)] -> Ability.Skills -> ActorAspect
            -> ActorId -> Bool
            -> m [(Int, (ItemId, ItemFull))]
pickWeaponM allAssocs actorSk actorAspect source effectBonus = do
  sb <- getsState $ getActorBody source
  localTime <- getsState $ getLocalTime (blid sb)
  let ar = actorAspect EM.! source
      calmE = calmEnough sb ar
      forced = assert (not $ bproj sb) False
      permitted = permittedPrecious calmE forced
      preferredPrecious = either (const False) id . permitted
      permAssocs = filter (preferredPrecious . snd) allAssocs
      strongest = strongestMelee effectBonus localTime permAssocs
  return $! if EM.findWithDefault 0 Ability.AbMelee actorSk <= 0
            then []
            else strongest
