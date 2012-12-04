{-# LANGUAGE OverloadedStrings #-}
-- | Construct English sentences from content.
module Game.LambdaHack.Grammar
  ( -- * Grammar types and the main operations
    Verb, Object, makePhrase, makeClause
    -- * Objects from content
  , objectItemCheat, objectItem, objectActor
    -- * Sentences
  , actorVerb, actorVerbItem, actorVerbActor
    -- * Scenery description
  , lookAt
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Point
import Game.LambdaHack.Msg
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
type Verb = Text

-- | The type of sentence objects.
type Object = Text

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeClause :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrrp
makeClause = MU.makeClause MU.defIrrp

-- | Transform an object, adding a count and a plural suffix.
makeObject :: Int -> (Text -> Text) -> Text -> Text
makeObject n f obj = makePhrase [MU.NWs n $ MU.Text $ f obj]

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
      eff = effectToSuffix (ieffect kind)
      pwr = if jpower i == 0
            then ""
            else "(+" <> showT (jpower i) <> ")"
      adj name =
        let known = name <> addSpace eff <> addSpace pwr
            flavour = getFlavour coitem (sflavour state) ik
            obscured = flavourToName flavour <+> name
        in if identified
           then known
           else if cheat
                then flavourToName flavour <+> known
                else obscured
  in makeObject (jcount i) adj (iname kind)

-- | How to refer to an item in object position of a sentence.
objectItem :: Kind.Ops ItemKind -> State -> Item -> Text
objectItem coitem = objectItemCheat coitem False

-- | How to refer to an actor in object position of a sentence.
objectActor :: Kind.Ops ActorKind -> Actor -> Text
objectActor Kind.Ops{oname} a =
  fromMaybe (oname $ bkind a) (bname a)

-- | Sentences such as \"Dog barks loudly.\"
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text -> Text
actorVerb coactor a v extra = makeClause
  [ MU.SubjectVerb (MU.Text $ objectActor coactor a) (MU.Text v)
  , MU.Text extra ]

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
        name = makePhrase [MU.Capitalize (MU.Text $ oname tile)]
    in name <> "." <+> msg <> isd
  | otherwise = msg <> isd
 where
  is  = lvl `rememberAtI` loc
  prefixSee = if canSee then "You see " else "You remember "
  isd = case is of
          []    -> ""
          [i]   -> prefixSee <> objectItem coitem s i <> "."
          [i,j] -> prefixSee <> objectItem coitem s i <+> "and "
                             <> objectItem coitem s j <> "."
          _ | detailed -> "Objects:"
          _ -> "Objects here."
