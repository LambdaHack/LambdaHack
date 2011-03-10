module Grammar where

import Data.Char

import Item
import Monster
import State
import ItemState

-- | How to refer to a movable in object position of a sentence.
objectMovable :: MovableType -> String
objectMovable (Hero _ s) = s
objectMovable Eye        = "the reducible eye"
objectMovable FastEye    = "the super-fast eye"
objectMovable Nose       = "the point-free nose"

-- | How to refer to a movable in subject position of a sentence.
subjectMovable :: MovableType -> String
subjectMovable x = let (s:r) = objectMovable x in toUpper s : r

verbMovable :: MovableType -> String -> String
verbMovable (Hero _ "you") v = v
verbMovable (Hero _ s)     v = v ++ "s"
verbMovable _              v = v ++ "s"

-- | Sentences such like "The dog barks".
subjectMovableVerb :: MovableType -> String -> String
subjectMovableVerb x v = subjectMovable x ++ " " ++ verbMovable x v

compoundVerbMovable :: MovableType -> String -> String -> String
compoundVerbMovable m v p = verbMovable m v ++ " " ++ p

objectItem :: State -> Int -> ItemType -> String
objectItem _ n Ring       = makeObject n id "ring"
objectItem _ n Scroll     = makeObject n id "scroll"
objectItem s n (Potion t) = makeObject n (identified (sassocs s) (sdiscoveries s) (Potion t)) "potion"
objectItem _ n Wand       = makeObject n id "wand"
objectItem _ n Amulet     = makeObject n id "amulet"
objectItem _ n Gem        = makeObject n id "gem"
objectItem _ n Gold       = makeObject n id "gold piece"
objectItem _ n (Sword i)  = makeObject n id ("(+" ++ show i ++ ") sword")

subjectVerbIObject :: State -> Movable -> String -> Item -> String -> String
subjectVerbIObject state m v o add =
  subjectMovable (mtype m) ++ " " ++
  verbMovable (mtype m) v ++ " " ++
  objectItem state (icount o) (itype o) ++ add ++ "."

subjectVerbMObject :: State -> Movable -> String -> Movable -> String -> String
subjectVerbMObject state m v o add =
  subjectMovable (mtype m) ++ " " ++
  verbMovable (mtype m) v ++ " " ++
  objectMovable (mtype o) ++ add ++ "."

subjectCompoundVerbIObject :: State -> Movable -> String -> String ->
                             Item -> String -> String
subjectCompoundVerbIObject state m v p o add =
  subjectMovable (mtype m) ++ " " ++
  compoundVerbMovable (mtype m) v p ++ " " ++
  objectItem state (icount o) (itype o) ++ add ++ "."
