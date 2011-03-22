module Movable where

import Data.Binary
import Control.Monad

import Geometry
import Item
import Monster

data Movable = Movable
                { mtype   :: !MovableType,
                  mhpmax  :: !Int,
                  mhp     :: !Int,
                  mdir    :: Maybe Dir,  -- for monsters: the dir the monster last moved; TODO: use target for this, instead and use mdir to signal the monster wants to switch position with a hero (if the monster is smart/big enough)
                                         -- for heroes: the dir the hero is running
                  mtarget :: Target,
                  mloc    :: !Loc,
                  mitems  :: [Item],     -- inventory
                  mletter :: !Char,      -- next inventory letter
                  mspeed  :: !Time,      -- speed (i.e., delay before next action)
                  mtime   :: !Time }     -- time of next action
  deriving Show

instance Binary Movable where
  put (Movable mt mhpm mhp md tgt ml minv mletter mspeed mtime) =
    do
      put mt
      put mhpm
      put mhp
      put md
      put tgt
      put ml
      put minv
      put mletter
      put mspeed
      put mtime
  get = do
          mt      <- get
          mhpm    <- get
          mhp     <- get
          md      <- get
          tgt     <- get
          ml      <- get
          minv    <- get
          mletter <- get
          mspeed  <- get
          mtime   <- get
          return (Movable mt mhpm mhp md tgt ml minv mletter mspeed mtime)

data Actor = AHero Int     -- ^ hero index (on the lheroes intmap)
           | AMonster Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq)

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
    TEnemy Actor  -- ^ fire at the actor (a monster or a hero)
  | TLoc Loc      -- ^ fire at a given location
  | TCursor       -- ^ fire at the current position of the cursor; the default
  deriving (Show, Eq)

instance Binary Target where
  put (TEnemy a) = putWord8 0 >> put a
  put (TLoc loc) = putWord8 1 >> put loc
  put TCursor    = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM TEnemy get
            1 -> liftM TLoc get
            2 -> return TCursor
            _ -> fail "no parse (Target)"
