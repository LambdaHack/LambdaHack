module ActorKind
  (ActorKind(..), ActorKindId, getKind, actorFrequency, heroKindId)
  where

import Geometry
import qualified Color
import Random
import Frequency
import qualified Kind

-- | Monster properties that are changing rarely and permanently.
data ActorKind = ActorKind
  { bhp     :: !RollDice     -- ^ encodes initial and maximal hp
  , bspeed  :: !Time         -- ^ natural speed
  , bsymbol :: !Char         -- ^ map symbol
  , bcolor  :: !Color.Color  -- ^ map color
  , bname   :: !String       -- ^ name
  , bsight  :: !Bool         -- ^ can it see?
  , bsmell  :: !Bool         -- ^ can it smell?
  , biq     :: !Int          -- ^ intelligence
  , bregen  :: !Int          -- ^ regeneration interval
  , bfreq   :: !Int          -- ^ dungeon frequency
  }
  deriving (Show, Eq, Ord)

type ActorKindId = Kind.Id ActorKind

ops :: Kind.Ops ActorKind
ops = Kind.buildOps content bfreq

actorFrequency :: Frequency ActorKindId
actorFrequency = Kind.frequency ops

getKind :: ActorKindId -> ActorKind
getKind = Kind.getKind ops

heroKindId :: ActorKindId
heroKindId = Kind.getId ops hero

content :: [ActorKind]
content = [hero, eye, fastEye, nose]

hero,            eye, fastEye, nose :: ActorKind

hero = ActorKind
  { bhp     = (50, 1)
  , bspeed  = 10
  , bsymbol = '@'
  , bname   = "hero"
  , bcolor  = Color.BrWhite  -- Heroes white, monsters colorful.
  , bsight  = True
  , bsmell  = False
  , biq     = 13  -- Can see secret doors, when he is under alien control.
  , bregen  = 1500
  , bfreq   = 0  -- Does not appear randomly throughout the dungeon.

  }

eye = ActorKind
  { bhp     = (1, 12)  -- falls in 1--4 unarmed rounds
  , bspeed  = 10
  , bsymbol = 'e'
  , bcolor  = Color.BrRed
  , bname   = "the reducible eye"
  , bsight  = True
  , bsmell  = False
  , biq     = 8
  , bregen  = 1500
  , bfreq   = 6
  }
fastEye = ActorKind
  { bhp     = (1, 6)  -- falls in 1--2 unarmed rounds
  , bspeed  = 4
  , bsymbol = 'e'
  , bcolor  = Color.BrBlue
  , bname   = "the super-fast eye"
  , bsight  = True
  , bsmell  = False
  , biq     = 12
  , bregen  = 1500
  , bfreq   = 1
  }
nose = ActorKind
  { bhp     = (6, 2)  -- 2--5 and in 1 round of the strongest sword
  , bspeed  = 11
  , bsymbol = 'n'
  , bcolor  = Color.Green
  , bname   = "the point-free nose"
  , bsight  = False
  , bsmell  = True
  , biq     = 0
  , bregen  = 1500
  , bfreq   = 2
  }
