module Grammar where

import Data.Char

import Item
import Monster
import State
import ItemState

-- | How to refer to a monster in object position of a sentence.
objectMonster :: MonsterType -> String
objectMonster Player  = "you"
objectMonster Eye     = "the reducible eye"
objectMonster FastEye = "the super-fast eye"
objectMonster Nose    = "the point-free nose"

-- | How to refer to a monster in subject position of a sentence.
subjectMonster :: MonsterType -> String
subjectMonster x = let (s:r) = objectMonster x in toUpper s : r

verbMonster :: MonsterType -> String -> String
verbMonster Player v = v
verbMonster _      v = v ++ "s"

compoundVerbMonster :: MonsterType -> String -> String -> String
compoundVerbMonster Player v p = v ++ " " ++ p
compoundVerbMonster _      v p = v ++ "s " ++ p

objectItem :: State -> Int -> ItemType -> String
objectItem _ n Ring       = makeObject n id "ring"
objectItem _ n Scroll     = makeObject n id "scroll"
objectItem s n (Potion t) = makeObject n (identified (sassocs s) (sdiscoveries s) (Potion t)) "potion"
objectItem _ n Wand       = makeObject n id "wand"
objectItem _ n Amulet     = makeObject n id "amulet"
objectItem _ n Gem        = makeObject n id "gem"
objectItem _ n Gold       = makeObject n id "gold piece"
objectItem _ n (Sword i)  = makeObject n id ("(+" ++ show i ++ ") sword")

