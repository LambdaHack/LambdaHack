module Grammar where

import Data.Char
import Data.Set as S
import Data.List as L
import qualified Data.IntMap as IM

import Item
import Movable
import MovableKind
import State
import ItemState
import ItemKind
import Effect

-- | How to refer to a movable in object position of a sentence.
objectMovable :: MovableKind -> String
objectMovable mk = nname mk

-- | How to refer to a movable in subject position of a sentence.
subjectMovable :: MovableKind -> String
subjectMovable x = let (s:r) = objectMovable x in toUpper s : r

verbMovable :: MovableKind -> String -> String
verbMovable mk v = if nname mk == "you" then v else v ++ "s"

-- | Sentences such like "The dog barks".
subjectMovableVerb :: MovableKind -> String -> String
subjectMovableVerb x v = subjectMovable x ++ " " ++ verbMovable x v

compoundVerbMovable :: MovableKind -> String -> String -> String
compoundVerbMovable m v p = verbMovable m v ++ " " ++ p

subjectVerbIObject :: State -> Movable -> String -> Item -> String -> String
subjectVerbIObject state m v o add =
  subjectMovable (mkind m) ++ " " ++
  verbMovable (mkind m) v ++ " " ++
  objectItem state o ++ add ++ "."

subjectVerbMObject :: State -> Movable -> String -> Movable -> String -> String
subjectVerbMObject state m v o add =
  subjectMovable (mkind m) ++ " " ++
  verbMovable (mkind m) v ++ " " ++
  objectMovable (mkind o) ++ add ++ "."

subjectCompoundVerbIObject :: State -> Movable -> String -> String ->
                             Item -> String -> String
subjectCompoundVerbIObject state m v p o add =
  subjectMovable (mkind m) ++ " " ++
  compoundVerbMovable (mkind m) v p ++ " " ++
  objectItem state o ++ add ++ "."

makeObject :: Int -> (String -> String) -> String -> String
makeObject 1 adj obj = let b = adj obj
                       in  case b of
                             (c:_) | c `elem` "aeio" -> "an " ++ b
                             _                       -> "a " ++ b
makeObject n adj obj = show n ++ " " ++ adj (obj ++ "s")

objectItem :: State -> Item -> String
objectItem state o =
  let ik = ikind o
      kind = ItemKind.getIK ik
      identified = L.length (jflavour kind) == 1 ||
                   ik `S.member` sdiscoveries state
      eff = effectToName (jeffect kind)
      pwr = if ipower o == 0 then "" else " (+" ++ show (ipower o) ++ ")"
      adj name = if identified
                 then name ++ if jsecret kind == ""
                              then if eff == "" then pwr else " " ++ eff ++ pwr
                              else " " ++ jsecret kind ++ pwr
                 else let flavour = getFlavour (sassocs state) ik
                      in  flavourToName flavour ++ " " ++ name
  in  makeObject (icount o) adj (jname kind)
