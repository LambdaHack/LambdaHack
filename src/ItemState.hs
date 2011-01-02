module ItemState where

import Data.Set as S
import Data.Map as M

import Item
import State

identified :: Assocs -> Discoveries -> ItemType -> String -> String
identified a d i
  | i `S.member` d = case i of
                       Potion t -> potionType t
                       _        -> ("really strange " ++)
  | otherwise      = case M.lookup i a of
                       Just ap  -> appearance ap
                       _        -> ("really strange " ++)
