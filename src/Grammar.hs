module Game.LambdaHack.Grammar where

import Data.Char
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

import Game.LambdaHack.Item
import Game.LambdaHack.Actor
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.State
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import qualified Game.LambdaHack.Kind as Kind

suffixS :: String -> String
suffixS word = case last word of
                'y' -> init word ++ "ies"
                's' -> word ++ "es"
                'x' -> word ++ "es"
                _   -> word ++ "s"

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

makeObject :: Int -> (String -> String) -> String -> String
makeObject 1 adj obj = let b = adj obj
                       in  case b of
                             (c:_) | c `elem` "aeio" -> "an " ++ b
                             _                       -> "a " ++ b
makeObject n adj obj = show n ++ " " ++ adj (suffixS obj)

-- TODO: when there's more of the above, split and move to Utils/

-- | How to refer to an actor in object position of a sentence.
objectActor :: Actor -> String
objectActor a = fromMaybe (aname $ Kind.getKind $ bkind a) (bname a)

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

objectItem :: State -> Item -> String
objectItem state o =
  let ik = jkind o
      kind = Kind.getKind ik
      identified = L.length (iflavour kind) == 1 ||
                   ik `S.member` sdisco state
      addSpace s = if s == "" then "" else " " ++ s
      eff = effectToName (ieffect kind)
      pwr = if jpower o == 0 then "" else "(+" ++ show (jpower o) ++ ")"
      adj name = if identified
                 then name ++ addSpace eff ++ addSpace pwr
                 else let flavour = getFlavour (sflavour state) ik
                      in  flavourToName flavour ++ " " ++ name
  in  makeObject (jcount o) adj (iname kind)
