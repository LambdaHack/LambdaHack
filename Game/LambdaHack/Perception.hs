{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Perception(..), PerceptionVisible(..), PerActor
  , totalVisible, actorSeesLoc, nullPer, addPer, diffPer
  , FactionPers, Pers
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Actor
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Point

-- TOOD: if really needed, optimize by representing as a set of intervals.
newtype PerceptionVisible = PerceptionVisible
  { pvisible :: ES.EnumSet Point}
  deriving (Show, Eq)

type PerActor = EM.EnumMap ActorId PerceptionVisible

-- | The type representing the perception of a faction on a level.
-- The total visibility holds the sum of FOVs of all actors
-- of a given faction on the level and servers only as a speedup.
data Perception = Perception
  { perActor :: PerActor           -- ^ visible points for each actor
  , ptotal   :: PerceptionVisible  -- ^ sum over all actors
  }
  deriving (Show, Eq)

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = EM.EnumMap LevelId Perception

-- | Perception indexed by faction identifier.
type Pers = EM.EnumMap FactionId FactionPers

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> ES.EnumSet Point
totalVisible = pvisible . ptotal

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per aid pos =
  let set = maybe ES.empty pvisible $ EM.lookup aid $ perActor per
  in pos `ES.member` set

nullPer :: Perception -> Bool
nullPer per = EM.null (perActor per) && ES.null (pvisible (ptotal per))

addPer :: Perception -> Perception -> Perception
addPer per1 per2 =
  let f :: (PerceptionVisible -> PerceptionVisible -> PerceptionVisible)
      f pv1 pv2 = PerceptionVisible $ pvisible pv1 `ES.union` pvisible pv2
  in Perception
       { perActor = EM.unionWith f (perActor per1) (perActor per2)
       , ptotal = PerceptionVisible
                  $ pvisible (ptotal per1) `ES.union` pvisible (ptotal per2)
       }

diffPer :: Perception -> Perception -> Perception
diffPer per1 per2 =
  let f :: (PerceptionVisible -> PerceptionVisible -> Maybe PerceptionVisible)
      f pv1 pv2 =
        let diff = pvisible pv1 ES.\\ pvisible pv2
        in if ES.null diff then Nothing else Just $ PerceptionVisible diff
  in Perception
       { perActor = EM.differenceWith f (perActor per1) (perActor per2)
       , ptotal = PerceptionVisible
                  $ pvisible (ptotal per1) ES.\\ pvisible (ptotal per2)
       }
