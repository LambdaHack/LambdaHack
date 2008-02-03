module Monster where

import Data.Char
import Data.Binary
import Control.Monad

import Geometry

data Monster = Monster
                { mtype :: MonsterType,
                  mhp   :: Int,
                  mloc  :: Loc }
  deriving Show

instance Binary Monster where
  put (Monster mt mhp ml) =
    do
      put mt
      put mhp
      put ml
  get = liftM3 Monster get get get

data MonsterType =
    Player
  | Eye
  deriving Show

instance Binary MonsterType where
  put Player = putWord8 0 
  put Eye    = putWord8 1
  get = do
          tag <- getWord8
          case tag of
            0 -> return Player 
            1 -> return Eye
            _ -> fail "no parse (MonsterType)" 

objectMonster :: MonsterType -> String
objectMonster Player = "you"
objectMonster Eye    = "the reducible eye"

subjectMonster :: MonsterType -> String
subjectMonster x = let (s:r) = objectMonster x in toUpper s : r

verbMonster :: MonsterType -> String -> String
verbMonster Player v = v
verbMonster _      v = v ++ "s"
