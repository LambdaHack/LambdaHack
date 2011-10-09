module ActorKind where

import Data.Binary

import Geometry
import qualified Color

-- | Monster properties that are changing rarely and permanently.
data ActorKind = ActorKind
  { bhpMin  :: !Int          -- ^ minimal initial hp
  , bhpMax  :: !Int          -- ^ maximal possible and initial hp
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
  deriving (Show, Eq)

instance Binary ActorKind where
  put (ActorKind nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq) =
    do
      put nhpMin
      put nhpMax
      put nsp
      put nsym
      put ncol
      put nnm
      put nsi
      put nsm
      put niq
      put nreg
      put nfreq
  get = do
    nhpMin <- get
    nhpMax <- get
    nsp    <- get
    nsym   <- get
    ncol   <- get
    nnm    <- get
    nsi    <- get
    nsm    <- get
    niq    <- get
    nreg   <- get
    nfreq  <- get
    return (ActorKind nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq)

-- | The list of kinds of monsters that appear randomly throughout the dungeon.
dungeonMonsters :: [ActorKind]
dungeonMonsters = [eye, fastEye, nose]

hero, eye, fastEye, nose :: ActorKind
hero = ActorKind
  { bhpMin  = 50,
    bhpMax  = 50,
    bspeed  = 10,
    bsymbol = '@',
    bname   = "hero",
    bcolor  = Color.BrWhite,  -- Heroes white, monsters colorful.
    bsight  = True,
    bsmell  = False,
    biq     = 13,  -- Can see secret doors under alien control.
    bregen  = 1500,
    bfreq   = 0
  }

eye = ActorKind
  { bhpMin  = 1,  -- falls in 1--4 unarmed rounds
    bhpMax  = 12,
    bspeed  = 10,
    bsymbol = 'e',
    bcolor  = Color.BrRed,
    bname   = "the reducible eye",
    bsight  = True,
    bsmell  = False,
    biq     = 8,
    bregen  = 1500,
    bfreq   = 6
  }
fastEye = ActorKind
  { bhpMin  = 1,  -- falls in 1--2 unarmed rounds
    bhpMax  = 6,
    bspeed  = 4,
    bsymbol = 'e',
    bcolor  = Color.BrBlue,
    bname   = "the super-fast eye",
    bsight  = True,
    bsmell  = False,
    biq     = 12,
    bregen  = 1500,
    bfreq   = 1
  }
nose = ActorKind
  { bhpMin  = 6,  -- 2--5 and in 1 round of the strongest sword
    bhpMax  = 13,
    bspeed  = 11,
    bsymbol = 'n',
    bcolor  = Color.Green,
    bname   = "the point-free nose",
    bsight  = False,
    bsmell  = True,
    biq     = 0,
    bregen  = 1500,
    bfreq   = 2
  }
