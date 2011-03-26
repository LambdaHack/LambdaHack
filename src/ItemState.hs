module ItemState where

import Data.Set as S
import Data.Map as M
import Data.List as L

import Item
import State
import Movable
import MovableState

-- | Calculate loot's worth. TODO: refine significantly.
calculateTotal :: State -> Int
calculateTotal s =
  L.sum $ L.map price $ L.concatMap mitems (levelHeroList s)
    where
      price i = if iletter i == Just '$' then icount i else 10 * icount i
