module Monster where

import Data.Binary
import Control.Monad

import Geometry
import Random
import qualified Display

data MovableType = MovableType
  { nhpMin   :: !Int,                -- ^ minimal initial hp
    nhpMax   :: !Int,                -- ^ maximal possible and initial hp
    nspeed   :: !Time,               -- ^ natural speed
    nsymbol  :: !Char,               -- ^ map symbol
    ncolor   :: !Display.AttrColor,  -- ^ map color
    nname    :: String,              -- ^ name
    nsight   :: !Bool,               -- ^ can it see?
    nsmell   :: !Bool,               -- ^ can it smell?
    niq      :: !Int,                -- ^ intelligence
    nregen   :: !Int,                -- ^ regeneration interval
    nfreq    :: !Int                 -- ^ dungeon frequency
  }
  deriving (Show, Eq)

hero, eye, fastEye, nose :: MovableType
hero = MovableType
  { nhpMin  = 50,
    nhpMax  = 50,
    nspeed  = 10,
    nsymbol = '@',
    nname   = "you",
    ncolor  = Display.bright_white,  -- Heroes white, monsters colorful.
    nsight  = True,
    nsmell  = False,
    niq     = 13,  -- Can see that secret doors under alien control.
    nregen  = 1500,
    nfreq   = 0
  }

eye = MovableType
  { nhpMin  = 1,  -- falls in 1--4 unarmed rounds
    nhpMax  = 12,
    nspeed  = 10,
    nsymbol = 'e',
    ncolor  = Display.bright_red,
    nname   = "the reducible eye",
    nsight  = True,
    nsmell  = False,
    niq     = 10,
    nregen  = 1500,
    nfreq   = 6
  }
fastEye = MovableType
  { nhpMin  = 1,  -- falls in 1--2 unarmed rounds
    nhpMax  = 6,
    nspeed  = 4,
    nsymbol = 'e',
    ncolor  = Display.bright_blue,
    nname   = "the super-fast eye",
    nsight  = True,
    nsmell  = False,
    niq     = 3,
    nregen  = 1500,
    nfreq   = 1
  }
nose = MovableType
  { nhpMin  = 6,  -- 2--5 and in 1 round of the strongest sword
    nhpMax  = 13,
    nspeed  = 11,
    nsymbol = 'n',
    ncolor  = Display.green,
    nname   = "the point-free nose",
    nsight  = False,
    nsmell  = True,
    niq     = 0,
    nregen  = 1500,
    nfreq   = 6
  }

-- | The list of types of monster that appear randomly throughout the dungeon.
roamingMts :: [MovableType]
roamingMts = [eye, fastEye, nose]

instance Binary MovableType where
  put (MovableType nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq) =
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
    return (MovableType nhpMin nhpMax nsp nsym ncol nnm nsi nsm niq nreg nfreq)
