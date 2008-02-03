module Monster where

import Data.Binary
import Control.Monad

import Geometry

data Monster = Monster
                { mloc :: Loc }
  deriving Show

instance Binary Monster where
  put (Monster ml) = put ml
  get = liftM Monster get 
