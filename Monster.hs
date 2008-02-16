module Monster where

import Data.Char
import Data.Binary
import Control.Monad

import Geometry

data Monster = Monster
                { mtype :: MonsterType,
                  mhp   :: Int,
                  mdir  :: Maybe Dir, -- indicates a "running" player, too
                  mloc  :: Loc }
  deriving Show

instance Binary Monster where
  put (Monster mt mhp md ml) =
    do
      put mt
      put mhp
      put md
      put ml
  get = liftM4 Monster get get get get

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
