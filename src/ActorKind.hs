module ActorKind
  (ActorKind(..), ActorKindId, getKind, actorFrequency, heroKindId)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Monad

import Geometry
import qualified Color
import Random

-- Some extra functions that need access to the internal representation:

actorFrequency :: Frequency ActorKindId
actorFrequency = Frequency [(bfreq ak, ActorKindId i) | (i, ak) <- kindAssocs]

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

newtype ActorKindId = ActorKindId Int
  deriving (Show, Eq, Ord)

instance Binary ActorKindId where
  put (ActorKindId i) = put i
  get = liftM ActorKindId get

heroKindId :: ActorKindId
heroKindId = ActorKindId $ -1

kindAssocs :: [(Int, ActorKind)]
kindAssocs = L.zip [0..] content

kindMap :: IM.IntMap ActorKind
kindMap = IM.fromDistinctAscList kindAssocs

getKind :: ActorKindId -> ActorKind
getKind ak@(ActorKindId i)
  | ak == heroKindId = hero
  | otherwise        = kindMap IM.! i

-- | Only monsters that appear randomly throughout the dungeon.
content :: [ActorKind]
content = [eye, fastEye, nose]

hero,      eye, fastEye, nose :: ActorKind

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
  , bfreq   = 0
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
