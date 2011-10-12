module Content.ActorKind (ActorKind(..)) where

import Color
import qualified Content.Content
import qualified Geometry
import qualified Random

-- | Monster properties that are changing rarely and permanently.
data ActorKind = ActorKind
  { bhp     :: !Random.RollDice  -- ^ encodes initial and maximal hp
  , bspeed  :: !Geometry.Time    -- ^ natural speed
  , bsymbol :: !Char             -- ^ map symbol
  , bcolor  :: !Color            -- ^ map color
  , bname   :: !String           -- ^ name
  , bsight  :: !Bool             -- ^ can it see?
  , bsmell  :: !Bool             -- ^ can it smell?
  , biq     :: !Int              -- ^ intelligence
  , bregen  :: !Int              -- ^ regeneration interval
  , bfreq   :: !Int              -- ^ dungeon frequency
  }
  deriving (Show, Eq, Ord)

instance Content.Content.Content ActorKind where
  getFreq = bfreq
  content =
    [hero, eye, fastEye, nose]

hero,      eye, fastEye, nose :: ActorKind

hero = ActorKind
  { bhp     = (50, 1)
  , bspeed  = 10
  , bsymbol = '@'
  , bname   = "hero"
  , bcolor  = BrWhite  -- Heroes white, monsters colorful.
  , bsight  = True
  , bsmell  = False
  , biq     = 13  -- Can see secret doors, when he is under alien control.
  , bregen  = 1500
  , bfreq   = 0  -- Does not appear randomly in the dungeon.

  }

eye = ActorKind
  { bhp     = (1, 12)  -- falls in 1--4 unarmed rounds
  , bspeed  = 10
  , bsymbol = 'e'
  , bcolor  = BrRed
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
  , bcolor  = BrBlue
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
  , bcolor  = Green
  , bname   = "the point-free nose"
  , bsight  = False
  , bsmell  = True
  , biq     = 0
  , bregen  = 1500
  , bfreq   = 2
  }
