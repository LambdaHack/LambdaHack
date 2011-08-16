module Actor where

import Data.Binary
import Control.Monad

import Geometry
import Item
import ActorKind

-- | Monster properties that are changing a lot. If they are dublets
-- of properties form ActorKind, the intention is they may be modified
-- temporarily, but will return to the original value over time. E.g., HP.
data Actor = Actor
  { akind   :: !ActorKind, -- ^ kind of the actor; TODO: make this an index
    ahp     :: !Int,       -- ^ current hit pints
    adir    :: Maybe Dir,  -- ^ the direction of running
    atarget :: Target,     -- ^ the target for distance attacks and AI
    aloc    :: !Loc,       -- ^ current location
    aitems  :: [Item],     -- ^ inventory
    aletter :: !Char,      -- ^ next inventory letter
    atime   :: !Time }     -- ^ time of next action
  deriving Show

instance Binary Actor where
  put (Actor akind ahp adir atarget aloc aitems aletter atime) =
    do
      put akind
      put ahp
      put adir
      put atarget
      put aloc
      put aitems
      put aletter
      put atime
  get = do
          akind   <- get
          ahp     <- get
          adir    <- get
          atarget <- get
          aloc    <- get
          aitems  <- get
          aletter <- get
          atime   <- get
          return (Actor akind ahp adir atarget aloc aitems aletter atime)

data ActorId = AHero Int     -- ^ hero index (on the lheroes intmap)
             | AMonster Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq, Ord)

isAHero :: ActorId -> Bool
isAHero (AHero _) = True
isAHero (AMonster _) = False

isAMonster :: ActorId -> Bool
isAMonster = not . isAHero

instance Binary ActorId where
  put (AHero n)    = putWord8 0 >> put n
  put (AMonster n) = putWord8 1 >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM AHero get
            1 -> liftM AMonster get
            _ -> fail "no parse (ActorId)"

data Target =
    TEnemy ActorId Loc  -- ^ fire at the actor; last seen location
  | TLoc Loc            -- ^ fire at a given location
  | TCursor             -- ^ fire at the current position of the cursor; default
  deriving (Show, Eq)

instance Binary Target where
  put (TEnemy a ll) = putWord8 0 >> put a >> put ll
  put (TLoc loc) = putWord8 1 >> put loc
  put TCursor    = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM2 TEnemy get get
            1 -> liftM TLoc get
            2 -> return TCursor
            _ -> fail "no parse (Target)"
