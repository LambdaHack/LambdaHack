{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
-- | Construct English sentences from content.
module Game.LambdaHack.Grammar
  (-- * Grammar types
    Verb, Object
    -- * General operations
  , capitalize, pluralise, addIndefinite, conjugate, (<>), (<+>), showT
    -- * Objects from content
  , objectItemCheat, objectItem, objectActor, capActor
    -- * Sentences
  , actorVerb, actorVerbItem, actorVerbActor, actorVerbExtraItem
    -- * Scenery description
  , lookAt
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Miniutter.English (capitalize)
import qualified NLP.Miniutter.English as MU

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

default (Text)

-- | The type of verbs.
type Verb = Text

-- | The type of sentence objects.
type Object = Text

-- These two stolen from Eric:

-- | Identical to 'T.append'
(<>) :: Text -> Text -> Text
t1 <> t2 = t1 `T.append` t2

-- | Separated by space unless one of them is empty (in which case just
--   the non-empty one)
(<+>) :: Text -> Text -> Text
t1 <+> t2 | T.null t1 = t2
          | T.null t2 = t1
          | otherwise = t1 `T.append` " " `T.append` t2

-- | Show a value in Text format.
showT :: Show a => a -> Text
showT = T.pack . show

-- | Nouns with irregular plural spelling.
-- See http://en.wikipedia.org/wiki/English_plural.
irregularPlural :: M.Map Text Text
irregularPlural = M.fromList
  [ ("canto"  , "cantos")
  , ("homo "  , "homos")
  , ("photo"  , "photos")
  , ("zero"   , "zeros")
  , ("piano"  , "pianos")
  , ("portico", "porticos")
  , ("pro"    , "pros")
  , ("quarto" , "quartos")
  , ("kimono" , "kimonos")
  , ("calf"   , "calves")
  , ("leaf"   , "leaves")
  , ("knife"  , "knives")
  , ("life"   , "lives")
  , ("dwarf"  , "dwarves")
  , ("hoof"   , "hooves")
  , ("elf"    , "elves")
  , ("staff"  , "staves")
  , ("child"  , "children")
  , ("foot"   , "feet")
  , ("goose"  , "geese")
  , ("louse"  , "lice")
  , ("man"    , "men")
  , ("mouse"  , "mice")
  , ("tooth"  , "teeth")
  , ("woman"  , "women")
  ]

-- | The list of words with identical singular and plural form.
-- See http://en.wikipedia.org/wiki/English_plural.
noPlural :: S.Set Text
noPlural = S.fromList
  [ "buffalo"
  , "deer"
  , "moose"
  , "sheep"
  , "bison"
  , "salmon"
  , "pike"
  , "trout"
  , "swine"
  , "aircraft"
  , "watercraft"
  , "spacecraft"
  , "hovercraft"
  , "information"
  ]

-- | Tests if a character is a vowel (@u@ is too hard, so is @eu@).
vowel :: Char -> Bool
vowel l = l `elem` "aeio"

compound :: Bool -> (Text -> Text) -> Text -> Text
compound modifyFirst f phrase =
  let rev | modifyFirst = reverse
          | otherwise   = id
  in case rev $ T.words phrase of
       [] -> assert `failure` ("compound: no words" :: String)
       word : rest -> T.unwords $ rev $ f word : rest

-- | Adds the plural (@s@, @es@, @ies@) suffix to a word.
-- Used also for conjugation.
-- See http://en.wikipedia.org/wiki/English_plural.
suffixS :: Text -> Text
suffixS = compound False singleSuffixS

singleSuffixS :: Text -> Text
singleSuffixS word = case L.reverse (T.unpack word) of
  'h' : 'c' : _ -> word <> "es"
  'h' : 's' : _ -> word <> "es"
  'i' : 's' : _ -> word <> "es"
  's' : _ -> word <> "es"
  'z' : _ -> word <> "es"
  'x' : _ -> word <> "es"
  'j' : _ -> word <> "es"
  'o' : l : _ | not (vowel l) -> T.init word <> "es"
  'y' : l : _ | not (vowel l) -> T.init word <> "ies"
  _ -> word <> "s"

pluralise :: Text -> Text
pluralise = compound True singlePluralise

-- TODO: a suffix tree would be best, to catch ableman, seaman, etc.
singlePluralise :: Text -> Text
singlePluralise word =
  if word `S.member` noPlural
  then word
  else case M.lookup word irregularPlural of
    Just plural -> plural
    Nothing -> suffixS word

conjugate :: Text -> Text -> Text
conjugate "you" "be" = "are"
conjugate "You" "be" = "are"
conjugate _     "be" = "is"
conjugate "you" verb = verb
conjugate "You" verb = verb
conjugate _     verb = suffixS verb

-- | Add the indefinite article (@a@, @an@) to a word (@h@ is too hard).
addIndefinite :: Text -> Text
addIndefinite b = case T.uncons b of
                    Just (c, _) | vowel c -> "an " <> b
                    _                     -> "a "  <> b

-- | Transform an object, adding a count and a plural suffix.
makeObject :: Int -> (Text -> Text) -> Text -> Text
makeObject 1 f obj = addIndefinite $ f obj
makeObject n f obj = showT n <> " " <> f (pluralise obj)

-- TODO: when there's more of the above, split and move to Utils/

-- | How to refer to an item in object position of a sentence.
-- If cheating is allowed, full identity of the object is revealed
-- together with its flavour (e.g. at game over screen).
objectItemCheat :: Kind.Ops ItemKind -> Bool -> State -> Item -> Text
objectItemCheat coitem@Kind.Ops{okind} cheat state i =
  let ik = jkind i
      kind = okind ik
      identified = L.length (iflavour kind) == 1 ||
                   ik `S.member` sdisco state
      addSpace s = if s == "" then "" else " " <> s
      eff = T.pack $ effectToSuffix (ieffect kind)
      pwr = if jpower i == 0
            then ""
            else "(+" <> showT (jpower i) <> ")"
      adj name =
        let known = name <> addSpace eff <> addSpace pwr
            flavour = getFlavour coitem (sflavour state) ik
            obscured = T.pack (flavourToName flavour) <> " " <> name
        in if identified
           then known
           else if cheat
                then T.pack (flavourToName flavour) <> " " <> known
                else obscured
  in makeObject (jcount i) adj (T.pack $ iname kind)

-- | How to refer to an item in object position of a sentence.
objectItem :: Kind.Ops ItemKind -> State -> Item -> Text
objectItem coitem = objectItemCheat coitem False

-- | How to refer to an actor in object position of a sentence.
objectActor :: Kind.Ops ActorKind -> Actor -> Text
objectActor Kind.Ops{oname} a =
  fromMaybe (T.pack $ oname $ bkind a) (T.pack `fmap` bname a)

-- | Capitalized actor object.
capActor :: Kind.Ops ActorKind -> Actor -> Text
capActor coactor x = capitalize $ objectActor coactor x

-- | Sentences such as \"Dog barks loudly.\"
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text -> Text
actorVerb coactor a v extra =
  let cactor = capActor coactor a
      verb = conjugate cactor v
      ending | T.null extra = "."
             | otherwise  = " " <> extra <> "."
  in cactor <> " " <> verb <> ending

-- | Sentences such as \"Dog quaffs a red potion fast.\"
actorVerbItem :: Kind.COps -> State -> Actor -> Text -> Item -> Text
                   -> Text
actorVerbItem Kind.COps{coactor, coitem} state a v i extra =
  let ending | T.null extra = ""
             | otherwise  = " " <> extra
  in actorVerb coactor a v $
       objectItem coitem state i <> ending

-- | Sentences such as \"Dog bites goblin furiously.\"
actorVerbActor :: Kind.Ops ActorKind -> Actor -> Text -> Actor -> Text
                    -> Text
actorVerbActor coactor a v b extra =
  let ending | T.null extra = ""
             | otherwise  = " " <> extra
  in actorVerb coactor a v $
       objectActor coactor b <> ending

-- | Sentences such as \"Dog gulps down a red potion fast.\"
actorVerbExtraItem :: Kind.COps -> State -> Actor -> Text -> Text
                        -> Item -> Text -> Text
actorVerbExtraItem Kind.COps{coactor, coitem} state a v extra1 i extra2 =
  assert (not $ T.null extra1) $
  let ending | T.null extra2 = ""
             | otherwise   = " " <> extra2
  in actorVerb coactor a v $
       extra1 <> " " <> objectItem coitem state i <> ending

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The detailed variant is for use in the targeting mode.
lookAt :: Kind.COps  -- ^ game content
       -> Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> State      -- ^ game state
       -> Level      -- ^ current level
       -> Point      -- ^ location to describe
       -> Text       -- ^ an extra sentence to print
       -> Text
lookAt Kind.COps{coitem, cotile=Kind.Ops{oname}} detailed canSee s lvl loc msg
  | detailed  =
    let tile = lvl `rememberAt` loc
        name = capitalize $ T.pack $ oname tile
    in name <> ". " <> msg <> isd
  | otherwise = msg <> isd
 where
  is  = lvl `rememberAtI` loc
  prefixSee = if canSee then "You see " else "You remember "
  isd = case is of
          []    -> ""
          [i]   -> prefixSee <> objectItem coitem s i <> "."
          [i,j] -> prefixSee <> objectItem coitem s i <> " and "
                             <> objectItem coitem s j <> "."
          _ | detailed -> "Objects:"
          _ -> "Objects here."
