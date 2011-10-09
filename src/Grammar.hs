module Grammar where

import Data.Char
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

import Item
import Actor
import ActorKind
import State
import ItemKind
import Effect
import Flavour

suffixS :: String -> String
suffixS word = case last word of
                'y' -> init word ++ "ies"
                's' -> word ++ "es"
                'x' -> word ++ "es"
                _   -> word ++ "s"

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

-- | How to refer to an actor in object position of a sentence.
objectActor :: Actor -> String
objectActor a = fromMaybe (bname $ ActorKind.getKind $ akind a) (aname a)

-- | How to refer to an actor in subject position of a sentence.
subjectActor :: Actor -> String
subjectActor x = capitalize $ objectActor x

verbActor :: Actor -> String -> String
verbActor a v = if objectActor a == "you" then v else suffixS v

-- | Sentences such like "The dog barks".
subjectActorVerb :: Actor -> String -> String
subjectActorVerb x v = subjectActor x ++ " " ++ verbActor x v

compoundVerbActor :: Actor -> String -> String -> String
compoundVerbActor m v p = verbActor m v ++ " " ++ p

subjectVerbIObject :: State -> Actor -> String -> Item -> String -> String
subjectVerbIObject state m v o add =
  subjectActor m ++ " " ++
  verbActor m v ++ " " ++
  objectItem state o ++ add ++ "."

subjectVerbMObject :: Actor -> String -> Actor -> String -> String
subjectVerbMObject m v o add =
  subjectActor m ++ " " ++
  verbActor m v ++ " " ++
  objectActor o ++ add ++ "."

subjCompoundVerbIObj :: State -> Actor -> String -> String ->
                        Item -> String -> String
subjCompoundVerbIObj state m v p o add =
  subjectActor m ++ " " ++
  compoundVerbActor m v p ++ " " ++
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
      kind = ItemKind.getKind ik
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
