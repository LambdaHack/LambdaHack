module MovableKind where

import Data.Binary
import Control.Monad

import Geometry
import Random
import qualified Color

-- | Monster properties that are changing rarely and permanently.
data MovableKind = MovableKind
  { nhpMin  :: !Int,          -- ^ minimal initial hp
    nhpMax  :: !Int,          -- ^ maximal possible and initial hp
    nspeed  :: !Time,         -- ^ natural speed
    nsymbol :: !Char,         -- ^ map symbol
    ncolor  :: !Color.Color,  -- ^ map color
    nname   :: String,        -- ^ name
    nsight  :: !Bool,         -- ^ can it see?
    nsmell  :: !Bool,         -- ^ can it smell?
    niq     :: !Int,          -- ^ intelligence
    nregen  :: !Int,          -- ^ regeneration interval
    nfreq   :: !Int           -- ^ dungeon frequency
  }
  deriving (Show, Eq)

instance Binary MovableKind where
  put (MovableKind nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq) =
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
    return (MovableKind nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq)

-- | The list of kinds of monsters that appear randomly throughout the dungeon.
dungeonMonsters :: [MovableKind]
dungeonMonsters = [eye, fastEye, nose]

hero, eye, fastEye, nose :: MovableKind
hero = MovableKind
  { nhpMin  = 50,
    nhpMax  = 50,
    nspeed  = 10,
    nsymbol = '@',
    nname   = "you",
    ncolor  = Color.BrWhite,  -- Heroes white, monsters colorful.
    nsight  = True,
    nsmell  = False,
    niq     = 13,  -- Can see that secret doors under alien control.
    nregen  = 1500,
    nfreq   = 0
  }

eye = MovableKind
  { nhpMin  = 1,  -- falls in 1--4 unarmed rounds
    nhpMax  = 12,
    nspeed  = 10,
    nsymbol = 'e',
    ncolor  = Color.BrRed,
    nname   = "the reducible eye",
    nsight  = True,
    nsmell  = False,
    niq     = 10,
    nregen  = 1500,
    nfreq   = 6
  }
fastEye = MovableKind
  { nhpMin  = 1,  -- falls in 1--2 unarmed rounds
    nhpMax  = 6,
    nspeed  = 4,
    nsymbol = 'e',
    ncolor  = Color.BrBlue,
    nname   = "the super-fast eye",
    nsight  = True,
    nsmell  = False,
    niq     = 3,
    nregen  = 1500,
    nfreq   = 1
  }
nose = MovableKind
  { nhpMin  = 6,  -- 2--5 and in 1 round of the strongest sword
    nhpMax  = 13,
    nspeed  = 11,
    nsymbol = 'n',
    ncolor  = Color.Green,
    nname   = "the point-free nose",
    nsight  = False,
    nsmell  = True,
    niq     = 0,
    nregen  = 1500,
    nfreq   = 6
  }
