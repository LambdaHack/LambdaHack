-- | Construct English sentences from content.
module Game.LambdaHack.Grammar
  ( -- * Grammar types
    Verb, Object
    -- * General operations
  , capitalize, suffixS, addIndefinite
    -- * Objects from content
  , objectItemCheat, objectItem, objectActor, capActor
    -- * Sentences
  , actorVerb, actorVerbItem, actorVerbActor, actorVerbExtraItem
    -- * Scenery description
  , lookAt
  ) where

import Data.Char
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Point
import Game.LambdaHack.Item
import Game.LambdaHack.Actor
import Game.LambdaHack.Level
import Game.LambdaHack.State
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import qualified Game.LambdaHack.Kind as Kind

-- | The type of verbs.
type Verb = String

-- | The grammatical object type.
type Object = String

-- | Tests if a character is a vowel (@u@ is too hard, so is @eu@).
vowel :: Char -> Bool
vowel l = l `elem` "aeio"

-- | Adds the plural (@s@, @es@, @ies@) suffix to a word.
-- See http://en.wikipedia.org/wiki/English_plural.
suffixS :: String -> String
suffixS word = case L.reverse word of
                'h' : 'c' : _ -> word ++ "es"
                'h' : 's' : _ -> word ++ "es"
                'i' : 's' : _ -> word ++ "es"
                's' : _ -> word ++ "es"
                'z' : _ -> word ++ "es"
                'x' : _ -> word ++ "es"
                'j' : _ -> word ++ "es"
                'o' : l : _ | not (vowel l) -> init word ++ "es"
                'y' : l : _ | not (vowel l) -> init word ++ "ies"
                _ -> word ++ "s"

conjugate :: String -> Verb -> Verb
conjugate "you" "be" = "are"
conjugate "You" "be" = "are"
conjugate _     "be" = "is"
conjugate "you" verb = verb
conjugate "You" verb = verb
conjugate _     verb = suffixS verb

-- | Capitalize a string.
capitalize :: Object -> Object
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

-- | Add the indefinite article (@a@, @an@) to a word (@h@ is too hard).
addIndefinite :: Object -> Object
addIndefinite b = case b of
                    c : _ | vowel c -> "an " ++ b
                    _               -> "a "  ++ b

-- | Transform an object, adding a count and a plural suffix.
makeObject :: Int -> (Object -> Object) -> Object -> Object
makeObject 1 f obj = addIndefinite $ f obj
makeObject n f obj = show n ++ " " ++ f (suffixS obj)

-- TODO: when there's more of the above, split and move to Utils/

-- | How to refer to an item in object position of a sentence.
-- If cheating is allowed, full identity of the object is revealed
-- together with its flavour (e.g. at game over screen).
objectItemCheat :: Kind.Ops ItemKind -> Bool -> State -> Item -> Object
objectItemCheat coitem@Kind.Ops{okind} cheat state i =
  let ik = jkind i
      kind = okind ik
      identified = L.length (iflavour kind) == 1 ||
                   ik `S.member` sdisco state
      addSpace s = if s == "" then "" else " " ++ s
      eff = effectToSuffix (ieffect kind)
      pwr = if jpower i == 0 then "" else "(+" ++ show (jpower i) ++ ")"
      adj name =
        let known = name ++ addSpace eff ++ addSpace pwr
            flavour = getFlavour coitem (sflavour state) ik
            obscured = flavourToName flavour ++ " " ++ name
        in if identified
           then known
           else if cheat
                then flavourToName flavour ++ " " ++ known
                else obscured
  in makeObject (jcount i) adj (iname kind)

-- | How to refer to an item in object position of a sentence.
objectItem :: Kind.Ops ItemKind -> State -> Item -> Object
objectItem coitem = objectItemCheat coitem False

-- | How to refer to an actor in object position of a sentence.
objectActor :: Kind.Ops ActorKind -> Actor -> Object
objectActor Kind.Ops{oname} a =
  fromMaybe (oname $ bkind a) (bname a)

-- | Capitalized actor object.
capActor :: Kind.Ops ActorKind -> Actor -> Object
capActor coactor x = capitalize $ objectActor coactor x

-- | Sentences such as \"Dog barks loudly.\"
actorVerb :: Kind.Ops ActorKind -> Actor -> Verb  -> String-> String
actorVerb coactor a v extra =
  let cactor = capActor coactor a
      verb = conjugate cactor v
      ending | null extra = "."
             | otherwise  = " " ++ extra ++ "."
  in cactor ++ " " ++ verb ++ ending

-- | Sentences such as \"Dog quaffs a red potion fast.\"
actorVerbItem :: Kind.COps -> State -> Actor -> Verb -> Item -> String
                   -> String
actorVerbItem Kind.COps{coactor, coitem} state a v i extra =
  let ending | null extra = ""
             | otherwise  = " " ++ extra
  in actorVerb coactor a v $
       objectItem coitem state i ++ ending

-- | Sentences such as \"Dog bites goblin furiously.\"
actorVerbActor :: Kind.Ops ActorKind -> Actor -> Verb -> Actor -> String
                    -> String
actorVerbActor coactor a v b extra =
  let ending | null extra = ""
             | otherwise  = " " ++ extra
  in actorVerb coactor a v $
       objectActor coactor b ++ ending

-- | Sentences such as \"Dog gulps down a red potion fast.\"
actorVerbExtraItem :: Kind.COps -> State -> Actor -> Verb -> String
                        -> Item -> String -> String
actorVerbExtraItem Kind.COps{coactor, coitem} state a v extra1 i extra2 =
  assert (not $ null extra1) $
  let ending | null extra2 = ""
             | otherwise   = " " ++ extra2
  in actorVerb coactor a v $
       extra1 ++ " " ++ objectItem coitem state i ++ ending

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The detailed variant is for use in the targeting mode.
lookAt :: Kind.COps  -- ^ game content
       -> Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> State      -- ^ game state
       -> Level      -- ^ current level
       -> Point      -- ^ location to describe
       -> String     -- ^ an extra sentence to print
       -> String
lookAt Kind.COps{coitem, cotile=Kind.Ops{oname}} detailed canSee s lvl loc msg
  | detailed  =
    let tile = lvl `rememberAt` loc
        name = capitalize $ oname tile
    in name ++ ". " ++ msg ++ isd
  | otherwise = msg ++ isd
 where
  is  = lvl `rememberAtI` loc
  prefixSee = if canSee then "You see " else "You remember "
  isd = case is of
          []    -> ""
          [i]   -> prefixSee ++ objectItem coitem s i ++ "."
          [i,j] -> prefixSee ++ objectItem coitem s i ++ " and "
                             ++ objectItem coitem s j ++ "."
          _ | detailed -> "Objects:"
          _ -> "Objects here."
