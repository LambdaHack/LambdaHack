module Grammar where

import Data.Char
import Data.Set as S
import Data.List as L
import qualified Data.IntMap as IM

import Item
import Movable
import MovableKind
import State
import ItemKind
import Effect


suffixS :: String -> String
suffixS word = case last word of
                'y' -> init word ++ "ies"
                's' -> word ++ "es"
                'x' -> word ++ "es"
                _   -> word ++ "s"

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

-- | How to refer to a movable in object position of a sentence.
objectMovable :: MovableKind -> String
objectMovable mk = nname mk

-- | How to refer to a movable in subject position of a sentence.
subjectMovable :: MovableKind -> String
subjectMovable x = capitalize $ objectMovable x

verbMovable :: MovableKind -> String -> String
verbMovable mk v = if nname mk == "you" then v else suffixS v

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

subjectVerbMObject :: Movable -> String -> Movable -> String -> String
subjectVerbMObject m v o add =
  subjectMovable (mkind m) ++ " " ++
  verbMovable (mkind m) v ++ " " ++
  objectMovable (mkind o) ++ add ++ "."

subjCompoundVerbIObj :: State -> Movable -> String -> String ->
                        Item -> String -> String
subjCompoundVerbIObj state m v p o add =
  subjectMovable (mkind m) ++ " " ++
  compoundVerbMovable (mkind m) v p ++ " " ++
  objectItem state o ++ add ++ "."

makeObject :: Int -> (String -> String) -> String -> String
makeObject 1 adj obj = let b = adj obj
                       in  case b of
                             (c:_) | c `elem` "aeio" -> "an " ++ b
                             _                       -> "a " ++ b
makeObject n adj obj = show n ++ " " ++ adj (suffixS obj)

objectItem :: State -> Item -> String
objectItem state o =
  let ik = ikind o
      kind = ItemKind.getIK ik
      identified = L.length (jflavour kind) == 1 ||
                   ik `S.member` sdiscoveries state
      addSpace s = if s == "" then "" else " " ++ s
      eff = effectToName (jeffect kind)
      pwr = if ipower o == 0 then "" else "(+" ++ show (ipower o) ++ ")"
      adj name = if identified
                 then name ++ addSpace eff ++ addSpace pwr
                 else let flavour = getFlavour (sassocs state) ik
                      in  flavourToName flavour ++ " " ++ name
  in  makeObject (icount o) adj (jname kind)
