module ItemState where

import Data.Set as S
import Data.Map as M

import Item
import State

objectItem :: State -> Int -> ItemType -> String
objectItem _ n Ring       = makeObject n id "ring"
objectItem _ n Scroll     = makeObject n id "scroll"
objectItem s n (Potion t) = makeObject n (identified (sassocs s) (sdiscoveries s) (Potion t)) "potion"
objectItem _ n Wand       = makeObject n id "wand"
objectItem _ n Amulet     = makeObject n id "amulet"
objectItem _ n Gem        = makeObject n id "gem"
objectItem _ n Gold       = makeObject n id "gold piece"

identified :: Assocs -> Discoveries -> ItemType -> String -> String
identified a d i
  | i `S.member` d = case i of
                       Potion t -> potionType t
                       _        -> ("really strange " ++)
  | otherwise      = case M.lookup i a of
                       Just ap  -> appearance ap
                       _        -> ("really strange " ++)

