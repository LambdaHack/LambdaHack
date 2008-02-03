module Monster where

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
  put Eye = putWord8 0
  get = do
          tag <- getWord8
          case tag of
            0 -> return Eye
            _ -> fail "no parse (MonsterType)" 
