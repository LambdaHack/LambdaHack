module ItemState where

import Data.Set as S
import Data.Map as M
import Data.List as L

import Item
import State
import Movable
import MovableState

identified :: Assocs -> Discoveries -> ItemType -> String -> String
identified a d i
  | i `S.member` d = case i of
                       Potion t -> potionType t
                       _        -> ("really strange " ++)
  | otherwise      = case M.lookup i a of
                       Just ap  -> appearance ap
                       _        -> ("really strange " ++)

-- | Calculate loot's worth. TODO: refine significantly.
calculateTotal :: State -> Int
calculateTotal s =
  L.sum $ L.map price $ L.concatMap mitems (levelHeroList s)
    where
      price i = if iletter i == Just '$' then icount i else 10 * icount i
