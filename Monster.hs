module Monster where

import Data.Char
import Data.Binary
import Control.Monad

import Geometry
import Display
import Item

-- | Hit points of the player. TODO: Should not be hardcoded.
playerHP :: Int
playerHP = 20

-- | Time the player can be traced by monsters. TODO: Make configurable.
smellTimeout :: Time
smellTimeout = 100

-- | Initial player.
defaultPlayer :: Loc -> Player
defaultPlayer ploc =
  Monster Player playerHP Nothing ploc []

type Player = Monster

data Monster = Monster
                { mtype   :: MonsterType,
                  mhp     :: Int,
                  mdir    :: Maybe Dir, -- for monsters: the dir the monster last moved;
                                        -- for the player: the dir the player is running
                  mloc    :: Loc,
                  mitems  :: [Item] }   -- inventory
  deriving Show

instance Binary Monster where
  put (Monster mt mhp md ml minv) =
    do
      put mt
      put mhp
      put md
      put ml
      put minv
  get = liftM5 Monster get get get get get

data MonsterType =
    Player
  | Eye
  | Nose
  deriving (Show, Eq)

instance Binary MonsterType where
  put Player = putWord8 0 
  put Eye    = putWord8 1
  put Nose   = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> return Player 
            1 -> return Eye
            2 -> return Nose
            _ -> fail "no parse (MonsterType)" 

objectMonster :: MonsterType -> String
objectMonster Player = "you"
objectMonster Eye    = "the reducible eye"
objectMonster Nose   = "the point-free nose"

subjectMonster :: MonsterType -> String
subjectMonster x = let (s:r) = objectMonster x in toUpper s : r

verbMonster :: MonsterType -> String -> String
verbMonster Player v = v
verbMonster _      v = v ++ "s"

compoundVerbMonster :: MonsterType -> String -> String -> String
compoundVerbMonster Player v p = v ++ " " ++ p
compoundVerbMonster _      v p = v ++ "s " ++ p

viewMonster :: MonsterType -> (Char, Attr -> Attr)
viewMonster Player = ('@', setBG white . setFG black)
viewMonster Eye    = ('e', setFG red)
viewMonster Nose   = ('n', setFG green)
