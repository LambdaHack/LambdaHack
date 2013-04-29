{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Common.Perception
  ( Perception(..), PerceptionVisible(..), PerActor
  , totalVisible, smellVisible
  , actorSeesLoc, nullPer, addPer, diffPer, smellFromActors
  , FactionPers, Pers
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State

-- TOOD: if really needed, optimize by representing as a set of intervals.
newtype PerceptionVisible = PerceptionVisible
  { pvisible :: ES.EnumSet Point}
  deriving (Show, Eq, Binary)

type PerActor = EM.EnumMap ActorId PerceptionVisible

-- | The type representing the perception of a faction on a level.
-- The total visibility holds the sum of FOVs of all actors
-- of a given faction on the level and serves only as a speedup.
-- The fields are not strict because often not all are used.
data Perception = Perception
  { perActor :: PerActor           -- ^ visible points for each actor
  , ptotal   :: PerceptionVisible  -- ^ sum over all actors
  , psmell   :: PerceptionVisible  -- ^ sum over actors that can smell
  }
  deriving (Show, Eq, Generic)

instance Binary Perception

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = EM.EnumMap LevelId Perception

-- | Perception indexed by faction identifier.
type Pers = EM.EnumMap FactionId FactionPers

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> ES.EnumSet Point
totalVisible = pvisible . ptotal

-- | The set of tiles smelled by at least one hero.
smellVisible :: Perception -> ES.EnumSet Point
smellVisible = pvisible . psmell

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per aid pos =
  let set = maybe ES.empty pvisible $ EM.lookup aid $ perActor per
  in pos `ES.member` set

nullPer :: Perception -> Bool
nullPer per = ES.null (totalVisible per)

addPer :: Perception -> Perception -> Perception
addPer per1 per2 =
  let f :: (PerceptionVisible -> PerceptionVisible -> PerceptionVisible)
      f pv1 pv2 = PerceptionVisible $ pvisible pv1 `ES.union` pvisible pv2
  in Perception
       { perActor = EM.unionWith f (perActor per1) (perActor per2)
       , ptotal = PerceptionVisible
                  $ totalVisible per1 `ES.union` totalVisible per2
       , psmell = PerceptionVisible
                  $ smellVisible per1 `ES.union` smellVisible per2
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
                  $ totalVisible per1 ES.\\ totalVisible per2
       , psmell = PerceptionVisible
                  $ smellVisible per1 ES.\\ smellVisible per2
       }

smellFromActors :: Kind.COps -> State -> PerActor -> PerceptionVisible
smellFromActors Kind.COps{coactor=Kind.Ops{okind}} s perActor =
  let actorCanSmell aid =
        if EM.notMember aid $ sactorD s
        then -- If actor is just created, it can already be in Perception
             -- sent to the client, but not in the client's state.
             -- We assume the actor's nose doesn't work yet on first turn.
             False
        else let b = getActorBody aid s
             in asmell $ okind $ bkind b
      visSmell = filter (actorCanSmell . fst) $ EM.assocs perActor
      setSmell = map (pvisible . snd) visSmell
  in PerceptionVisible $ ES.unions setSmell
