-- | Game state reading monad and basic operations.
module Game.LambdaHack.Common.MonadStateRead
  ( MonadStateRead(..)
  , getState, getLevel
  , getGameMode, isNoConfirmsGame, getEntryArena, pickWeaponM, displayTaunt
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability

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

getGameMode :: MonadStateRead m => m ModeKind
getGameMode = do
  COps{comode} <- getsState scops
  t <- getsState sgameModeId
  return $! okind comode t

isNoConfirmsGame :: MonadStateRead m => m Bool
isNoConfirmsGame = do
  gameMode <- getGameMode
  return $! maybe False (> 0) $ lookup NO_CONFIRMS $ mfreq gameMode

getEntryArena :: MonadStateRead m => Faction -> m LevelId
getEntryArena fact = do
  dungeon <- getsState sdungeon
  let (minD, maxD) = dungeonBounds dungeon
      f [] = 0
      f ((ln, _, _) : _) = ln
  return $! max minD $ min maxD $ toEnum $ f $ ginitial fact

pickWeaponM :: MonadStateRead m
            => Bool -> Maybe DiscoveryBenefit
            -> [(ItemId, ItemFullKit)] -> Ability.Skills -> ActorId
            -> m [(Double, Int, ItemId, ItemFullKit)]
pickWeaponM ignoreCharges mdiscoBenefit kitAss actorSk source = do
  sb <- getsState $ getActorBody source
  localTime <- getsState $ getLocalTime (blid sb)
  actorMaxSk <- getsState $ getActorMaxSkills source
  let calmE = calmEnough sb actorMaxSk
      forced = bproj sb
      permitted = permittedPrecious forced calmE
      preferredPrecious = either (const False) id . permitted
      permAssocs = filter (preferredPrecious . fst . snd) kitAss
      strongest = strongestMelee ignoreCharges mdiscoBenefit
                                 localTime permAssocs
  return $! if | forced -> map (\(iid, itemFullKit) ->
                                  (1, 1, iid, itemFullKit)) kitAss
               | Ability.getSk Ability.SkMelee actorSk <= 0 -> []
               | otherwise -> strongest

displayTaunt :: MonadStateRead m
             => Bool -> (Rnd (Text, Text) -> m (Text, Text))
             -> ActorId -> m (Text, Text)
displayTaunt _voluntary rndToAction aid = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let canApply = Ability.getSk Ability.SkApply actorMaxSk > 2
                 && canHear
        -- if applies complex items, probably intelligent and can speak
      canHear = Ability.getSk Ability.SkHearing actorMaxSk > 0
                && canBrace
        -- if hears, probably also emits sound vocally;
        -- disabled even by ushanka and rightly so
      canBrace = Ability.getSk Ability.SkWait actorMaxSk >= 2
        -- not an insect, plant, geyser, faucet, fence, etc.
        -- so can emit sound by hitting something with body parts
                 || Ability.getSk Ability.SkApply actorMaxSk > 2
                      -- and neither an impatient intelligent actor
      braceUneasy = [ (2, ("something", "flail around"))
                    , (1, ("something", "toss blindly"))
                    , (1, ("something", "squirm dizzily")) ]
      braceEasy = [ (2, ("something", "stretch"))
                  , (1, ("something", "fidget"))
                  , (1, ("something", "fret")) ]
      uneasy = deltasSerious (bcalmDelta b) || not (calmEnough b actorMaxSk)
  if bwatch b `elem` [WSleep, WWake]
  then rndToAction $ frequency $ toFreq "SfxTaunt" $
    if uneasy
    then if | canApply -> (5, ("somebody", "yell"))
                          : (3, ("somebody", "bellow"))
                          : braceUneasy
            | canHear -> (5, ("somebody", "bellow"))
                         : (3, ("something", "hiss"))
                         : braceUneasy
            | canBrace -> braceUneasy
            | otherwise -> [(1, ("something", "drone enquiringly"))]
    else if | canApply -> (5, ("somebody", "yawn"))
                          : (3, ("somebody", "grunt"))
                          : braceEasy
            | canHear -> (5, ("somebody", "grunt"))
                         : (3, ("something", "wheeze"))
                         : braceEasy
            | canBrace -> braceEasy
            | otherwise -> [(1, ("something", "hum silently"))]
  else return $!
    if | canApply -> ("somebody", "holler a taunt")
       | canHear -> ("somebody", "growl menacingly")
       | canBrace -> ("something", "stomp repeatedly")
       | otherwise -> ("something", "buzz angrily")
