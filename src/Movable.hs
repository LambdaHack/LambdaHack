module Movable where

import Data.Binary
import Control.Monad

import Geometry
import Item
import MovableKind

-- | Monster properties that are changing a lot. If they are dublets
-- of properties form MovableKind, the intention is they may be modified
-- temporarily, but will return to the original value over time. E.g., HP.
data Movable = Movable
  { mkind   :: !MovableKind,  -- ^ kind of the movable; TODO: make this Int
    mhp     :: !Int,       -- ^ current hit pints
    mdir    :: Maybe Dir,  -- ^ the direction of running
    mtarget :: Target,     -- ^ the target for distance attacks and AI
    mloc    :: !Loc,       -- ^ current location
    mitems  :: [Item],     -- ^ inventory
    mletter :: !Char,      -- ^ next inventory letter
    mtime   :: !Time }     -- ^ time of next action
  deriving Show

instance Binary Movable where
  put (Movable mk mhp md tgt ml minv mletter mtime) =
    do
      put mk
      put mhp
      put md
      put tgt
      put ml
      put minv
      put mletter
      put mtime
  get = do
          mk      <- get
          mhp     <- get
          md      <- get
          tgt     <- get
          ml      <- get
          minv    <- get
          mletter <- get
          mtime   <- get
          return (Movable mk mhp md tgt ml minv mletter mtime)

data Actor = AHero Int     -- ^ hero index (on the lheroes intmap)
           | AMonster Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq, Ord)

isAHero :: Actor -> Bool
isAHero (AHero _) = True
isAHero (AMonster _) = False

isAMonster :: Actor -> Bool
isAMonster = not . isAHero

instance Binary Actor where
  put (AHero n)    = putWord8 0 >> put n
  put (AMonster n) = putWord8 1 >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM AHero get
            1 -> liftM AMonster get
            _ -> fail "no parse (Actor)"

data Target =
    TEnemy Actor Loc -- ^ fire at the actor; last seen location
  | TLoc Loc         -- ^ fire at a given location
  | TCursor          -- ^ fire at the current position of the cursor; default
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
