{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of weapons, treasure, organs, blasts, etc.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), makeData
  , Aspect(..), Effect(..), DetectKind(..), TimerDice, ThrowMod(..)
  , boostItemKindList, forApplyEffect
  , strengthOnSmash, getDropOrgans, getMandatoryHideAsFromKind, isEffEscape
  , isEffEscapeOrAscend, timeoutAspect, onSmashEffect, damageUsefulness
  , verbMsgNoLonger, verbMsgLess, toVelocity, toLinger
  , timerNone, isTimerNone, foldTimer, toOrganBad, toOrganGood, toOrganNoTimer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , boostItemKind, validateSingle, validateAll, validateDups, validateDamage
  , hardwiredItemGroups
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import           Data.Hashable (Hashable)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified System.Random as R
import qualified System.Random.SplitMix32 as SM

import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | Item properties that are fixed for a given kind of items.
-- Of these, aspects and effects are jointly called item powers.
-- Note that this type is mutually recursive with 'Effect' and `Aspect`.
data ItemKind = ItemKind
  { isymbol  :: Char            -- ^ map symbol
  , iname    :: Text            -- ^ generic name; is pluralized if needed
  , ifreq    :: Freqs ItemKind  -- ^ frequency within groups
  , iflavour :: [Flavour]       -- ^ possible flavours
  , icount   :: Dice.Dice       -- ^ created in that quantity
  , irarity  :: Rarity          -- ^ rarity on given depths
  , iverbHit :: Text            -- ^ the verb for hitting
  , iweight  :: Int             -- ^ weight in grams
  , idamage  :: Dice.Dice       -- ^ basic kinetic damage
  , iaspects :: [Aspect]        -- ^ affect the actor continuously
  , ieffects :: [Effect]        -- ^ cause the effects when triggered
  , ikit     :: [(GroupName ItemKind, CStore)]
                                -- ^ accompanying organs and equipment
  , idesc    :: Text            -- ^ description
  }
  deriving (Show, Generic)  -- No Eq and Ord to make extending logically sound

-- | Aspects of items. Aspect @AddSkill@ is additive (starting at 0)
-- for all items wielded by an actor and it affects the actor.
-- The others affect only the item in question, not the actor carrying it,
-- and so are not additive in any sense.
data Aspect =
    Timeout Dice.Dice  -- ^ specifies the cooldown before an item may be
                       --   applied again; if a copy of an item is applied
                       --   manually (not via periodic activation),
                       --   all effects on a single copy of the item are
                       --   disabled until the copy recharges for the given
                       --   time expressed in game turns; all copies
                       --   recharge concurrently
  | AddSkill Ability.Skill Dice.Dice
                       -- ^ bonus to a skill; in content, avoid boosting
                       --   skills such as SkApply via permanent equipment,
                       --   to avoid micromanagement through swapping items
                       --   among party members before each skill use
  | SetFlag Ability.Flag
                       -- ^ item feature
  | ELabel Text        -- ^ extra label of the item; it's not pluralized
  | ToThrow ThrowMod   -- ^ parameters modifying a throw
  | HideAs (GroupName ItemKind)
                       -- ^ until identified, presents as this unique kind
  | EqpSlot Ability.EqpSlot
                       -- ^ AI and UI flag that leaks item intended use
  | Odds Dice.Dice [Aspect] [Aspect]
                       -- ^ if level-scaled dice roll > 50,
                       --   pick the former aspects, otherwise the latter
  deriving (Show, Eq, Generic)

-- | Effects of items. Can be invoked by the item wielder to affect
-- another actor or the wielder himself. Many occurences in the same item
-- are possible.
data Effect =
    Burn Dice.Dice     -- ^ burn with this damage
  | Explode (GroupName ItemKind)
                       -- ^ explode producing this group of blasts
  | RefillHP Int       -- ^ modify HP of the actor by this amount
  | RefillCalm Int     -- ^ modify Calm of the actor by this amount
  | Dominate           -- ^ change actor's allegiance
  | Impress            -- ^ make actor susceptible to domination
  | PutToSleep         -- ^ put actor to sleep, also calming him
  | Yell               -- ^ make the actor yell/yawn, waking him and others up
  | Summon (GroupName ItemKind) Dice.Dice
      -- ^ summon the given number of actors of this group
  | Ascend Bool           -- ^ ascend to another level of the dungeon
  | Escape                -- ^ escape from the dungeon
  | Paralyze Dice.Dice    -- ^ paralyze for this many game clips
  | ParalyzeInWater Dice.Dice
                          -- ^ paralyze for this many game clips due to water
  | InsertMove Dice.Dice  -- ^ give actor this many extra tenths of actor move
  | Teleport Dice.Dice    -- ^ teleport actor across rougly this distance
  | CreateItem CStore (GroupName ItemKind) TimerDice
      -- ^ create an item of the group and insert into the store with the given
      --   random timer
  | DropItem Int Int CStore (GroupName ItemKind)
      -- ^ make the actor drop items of the given group from the given store;
      --   the first integer says how many item kinds to drop, the second,
      --   how many copies of each kind to drop; for non-organs, beware of
      --   not dropping all, or cluttering store with rubbish becomes beneficial
  | PolyItem
      -- ^ get a suitable (i.e., numerous enough) non-unique common item stack
      --   on the floor and polymorph it to a stack of random common items,
      --   with current depth coefficient
  | RerollItem
      -- ^ get a suitable (i.e., with any random aspects) single item
      --   (even unique) on the floor and change the random bonuses
      --   of the items randomly, with maximal depth coefficient
  | DupItem
      -- ^ exactly duplicate a single non-unique, non-valuable item on the floor
  | Identify
      -- ^ find a suitable (i.e., not identified) item, starting from
      --   the floor, and identify it
  | Detect DetectKind Int -- ^ detect something on the map in the given radius
  | SendFlying ThrowMod   -- ^ send an actor flying (push or pull, depending)
  | PushActor ThrowMod    -- ^ push an actor
  | PullActor ThrowMod    -- ^ pull an actor
  | DropBestWeapon        -- ^ make the actor drop its best weapon
  | ApplyPerfume          -- ^ remove all smell on the level
  | OneOf [Effect]        -- ^ trigger one of the effects with equal probability
  | OnSmash Effect
      -- ^ trigger the effect when item smashed (not when applied nor meleed);
  | Composite [Effect]    -- ^ only fire next effect if previous fully activated
  | VerbNoLonger Text
      -- ^ a sentence with the actor causing the effect as subject and the given
      --   text as verb is emitted when the activation causes item to expire;
      --   no spam is emitted if a projectile
  | VerbMsg Text
      -- ^ a sentence with the actor causing the effect as subject and the given
      --   text as verb is emitted whenever the item is activated;
      --   no spam is emitted if a projectile
  deriving (Show, Eq, Generic)

data DetectKind =
    DetectAll
  | DetectActor
  | DetectLoot
  | DetectExit
  | DetectHidden
  | DetectEmbed
  deriving (Show, Eq, Generic)

-- | Specification of how to randomly roll a timer at item creation
-- to obtain a fixed timer for the item's lifetime.
data TimerDice =
    TimerNone
  | TimerGameTurn Dice.Dice
  | TimerActorTurn Dice.Dice
  deriving (Eq, Generic)

instance Show TimerDice where
  show TimerNone = "0"
  show (TimerGameTurn nDm) =
    show nDm ++ " " ++ if nDm == 1 then "turn" else "turns"
  show (TimerActorTurn nDm) =
    show nDm ++ " " ++ if nDm == 1 then "move" else "moves"

-- | Parameters modifying a throw of a projectile or flight of pushed actor.
-- Not additive and don't start at 0.
data ThrowMod = ThrowMod
  { throwVelocity :: Int  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: Int  -- ^ fly for this percentage of 2 turns
  , throwHP       :: Int  -- ^ start flight with this many HP
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Effect

instance Binary DetectKind

instance Binary TimerDice

instance Binary ThrowMod

instance Hashable ThrowMod

boostItemKindList :: SM.SMGen -> [ItemKind] -> [ItemKind]
boostItemKindList _ [] = []
boostItemKindList initialGen l =
  let (r, _) = R.randomR (0, length l - 1) initialGen
  in case splitAt r l of
    (pre, i : post) -> pre ++ boostItemKind i : post
    _               -> error $  "" `showFailure` l

boostItemKind :: ItemKind -> ItemKind
boostItemKind i =
  let mainlineLabel (label, _) =
        label `elem` ["common item", "curious item", "treasure"]
  in if any mainlineLabel (ifreq i)
     then i { ifreq = ("common item", 10000) : filter (not . mainlineLabel) (ifreq i)
            , iaspects = delete (SetFlag Ability.Unique) $ iaspects i
            }
     else i

-- | Whether the effect has a chance of exhibiting any potentially
-- noticeable behaviour, except when the item is destroyed.
-- We assume at least one of @OneOf@ effects must be noticeable.
forApplyEffect :: Effect -> Bool
forApplyEffect eff = case eff of
  OnSmash{} -> False
  Composite effs -> any forApplyEffect effs
  VerbNoLonger{} -> False
  VerbMsg{} -> False
  ParalyzeInWater{} -> False  -- barely noticeable, spams when resisted
  _ -> True

isEffEscape :: Effect -> Bool
isEffEscape Escape{} = True
isEffEscape (OneOf l) = any isEffEscape l
isEffEscape (Composite l) = any isEffEscape l
isEffEscape _ = False

isEffEscapeOrAscend :: Effect -> Bool
isEffEscapeOrAscend Ascend{} = True
isEffEscapeOrAscend Escape{} = True
isEffEscapeOrAscend (OneOf l) = any isEffEscapeOrAscend l
isEffEscapeOrAscend (Composite l) = any isEffEscapeOrAscend l
isEffEscapeOrAscend _ = False

timeoutAspect :: Aspect -> Bool
timeoutAspect Timeout{} = True
timeoutAspect _ = False

onSmashEffect :: Effect -> Bool
onSmashEffect OnSmash{} = True
onSmashEffect _ = False

strengthOnSmash :: ItemKind -> [Effect]
strengthOnSmash =
  let f (OnSmash eff) = [eff]
      f _ = []
  in concatMap f . ieffects

getDropOrgans :: ItemKind -> [GroupName ItemKind]
getDropOrgans =
  let f (DropItem _ _ COrgan grp) = [grp]
      f Impress = ["impressed"]
      f (OneOf l) = concatMap f l  -- even remote possibility accepted
      f (Composite l) = concatMap f l  -- not certain, but accepted
      f _ = []
  in concatMap f . ieffects

-- Anything under @Odds@ is ignored, because it's not mandatory.
getMandatoryHideAsFromKind :: ItemKind -> Maybe (GroupName ItemKind)
getMandatoryHideAsFromKind itemKind =
  let f (HideAs grp) = [grp]
      f _ = []
  in listToMaybe $ concatMap f (iaspects itemKind)

damageUsefulness :: ItemKind -> Double
damageUsefulness itemKind =
  let v = min 1000 (10 * Dice.meanDice (idamage itemKind))
  in assert (v >= 0) v

verbMsgNoLonger :: Text -> Effect
verbMsgNoLonger name = VerbNoLonger $ "be no longer" <+> name

verbMsgLess :: Text -> Effect
verbMsgLess name = VerbMsg $ "look less" <+> name

toVelocity :: Int -> Aspect
toVelocity n = ToThrow $ ThrowMod n 100 1

toLinger :: Int -> Aspect
toLinger n = ToThrow $ ThrowMod 100 n 1

timerNone :: TimerDice
timerNone = TimerNone

isTimerNone :: TimerDice -> Bool
isTimerNone tim = tim == TimerNone

foldTimer :: a -> (Dice.Dice -> a) -> (Dice.Dice -> a) -> TimerDice -> a
foldTimer a fgame factor tim = case tim of
  TimerNone -> a
  TimerGameTurn nDm -> fgame nDm
  TimerActorTurn nDm -> factor nDm

toOrganBad :: GroupName ItemKind -> Dice.Dice -> Effect
toOrganBad grp nDm =
  assert (Dice.infDice nDm > 0
          `blame` "dice at organ creation should always roll above zero"
          `swith` (grp, nDm))
  $ CreateItem COrgan grp (TimerGameTurn nDm)

toOrganGood :: GroupName ItemKind -> Dice.Dice -> Effect
toOrganGood grp nDm =
  assert (Dice.infDice nDm > 0
          `blame` "dice at organ creation should always roll above zero"
          `swith` (grp, nDm))
  $ CreateItem COrgan grp (TimerActorTurn nDm)

toOrganNoTimer :: GroupName ItemKind -> Effect
toOrganNoTimer grp = CreateItem COrgan grp TimerNone

-- | Catch invalid item kind definitions.
validateSingle :: ItemKind -> [Text]
validateSingle ik@ItemKind{..} =
  ["iname longer than 23" | T.length iname > 23]
  ++ ["icount < 0" | Dice.infDice icount < 0]
  ++ validateRarity irarity
  ++ validateDamage idamage
  -- Reject duplicate Timeout, because it's not additive.
  ++ (let ts = filter timeoutAspect iaspects
      in ["more than one Timeout specification" | length ts > 1])
  ++ [ "Conflicting Fragile and Durable"
     | SetFlag Ability.Fragile `elem` iaspects
       && SetFlag Ability.Durable `elem` iaspects ]
  ++ (let f :: Aspect -> Bool
          f EqpSlot{} = True
          f _ = False
          ts = filter f iaspects
      in [ "EqpSlot specified but not Equipable nor Meleeable"
         | length ts > 0 && SetFlag Ability.Equipable `notElem` iaspects
                         && SetFlag Ability.Meleeable `notElem` iaspects ])
  ++ [ "Redundant Equipable or Meleeable"
     | SetFlag Ability.Equipable `elem` iaspects
       && SetFlag Ability.Meleeable `elem` iaspects ]
  ++ [ "Conflicting Durable and Blast"
     | SetFlag Ability.Durable `elem` iaspects
       && SetFlag Ability.Blast `elem` iaspects ]
  ++ [ "Conflicting Durable and Condition"
     | SetFlag Ability.Durable `elem` iaspects
       && SetFlag Ability.Condition `elem` iaspects ]
  ++ [ "Conflicting Blast and Condition"
     | SetFlag Ability.Blast `elem` iaspects
       && SetFlag Ability.Condition `elem` iaspects ]
  ++ (let f :: Aspect -> Bool
          f ELabel{} = True
          f _ = False
          ts = filter f iaspects
      in ["more than one ELabel specification" | length ts > 1])
  ++ (let f :: Aspect -> Bool
          f ToThrow{} = True
          f _ = False
          ts = filter f iaspects
      in ["more than one ToThrow specification" | length ts > 1])
  ++ (let f :: Aspect -> Bool
          f HideAs{} = True
          f _ = False
          ts = filter f iaspects
      in ["more than one HideAs specification" | length ts > 1])
  ++ concatMap (validateDups ik) (map SetFlag [minBound .. maxBound])
  ++ (let f :: Effect -> Bool
          f VerbMsg{} = True
          f _ = False
      in validateOnlyOne ieffects "VerbMsg" f)  -- may be duplicated if nested
  ++ (let f :: Effect -> Bool
          f VerbNoLonger{} = True
          f _ = False
      in validateOnlyOne ieffects "VerbNoLonger" f)  -- may be duped if nested
  ++ (validateNotNested ieffects "OnSmash" onSmashEffect)
       -- duplicates permitted

-- We only check there are no duplicates at top level. If it may be nested,
-- it may presumably be duplicated inside the nesting as well.
validateOnlyOne :: [Effect] -> Text -> (Effect -> Bool) -> [Text]
validateOnlyOne effs t f =
  let ts = filter f effs
  in ["more than one" <+> t <+> "specification" | length ts > 1]

-- We check it's not nested one nor more levels.
validateNotNested :: [Effect] -> Text -> (Effect -> Bool) -> [Text]
validateNotNested effs t f =
  let g (OneOf l) = any f l || any g l
      g (OnSmash effect) = f effect || g effect
      g (Composite l) = any f l || any g l
      g _ = False
      ts = filter g effs
  in [ "effect" <+> t <+> "should be specified at top level, not nested"
     | length ts > 0 ]

validateDups :: ItemKind -> Aspect -> [Text]
validateDups ItemKind{..} feat =
  let ts = filter (== feat) iaspects
  in ["more than one" <+> tshow feat <+> "specification" | length ts > 1]

validateDamage :: Dice.Dice -> [Text]
validateDamage dice = [ "potentially negative dice:" <+> tshow dice
                      | Dice.infDice dice < 0]

-- | Validate all item kinds.
validateAll :: [ItemKind] -> ContentData ItemKind -> [Text]
validateAll content coitem =
  let missingKitGroups = [ cgroup
                      | k <- content
                      , (cgroup, _) <- ikit k
                      , not $ omemberGroup coitem cgroup ]
      f :: Aspect -> Bool
      f HideAs{} = True
      f _ = False
      wrongHideAsGroups =
        [ cgroup
        | k <- content
        , let (cgroup, notSingleton) = case find f (iaspects k) of
                Just (HideAs grp) | not $ oisSingletonGroup coitem grp ->
                  (grp, True)
                _ -> (undefined, False)
        , notSingleton
        ]
      g :: Effect -> Maybe (GroupName ItemKind)
      g (Explode grp) = Just grp
      g (Summon grp _) = Just grp
      g (CreateItem _ grp _) = Just grp
      g (DropItem _ _ _ grp) = Just grp
      g _ = Nothing
      missingEffectGroups =
        [ (iname k, absGroups)
        | k <- content
        , let grps = mapMaybe g $ ieffects k
              absGroups = filter (not . omemberGroup coitem) grps
        , not $ null absGroups
        ]
      missingHardwiredGroups =
        filter (not . omemberGroup coitem) hardwiredItemGroups
  in [ "no ikit groups in content:" <+> tshow missingKitGroups
     | not $ null missingKitGroups ]
     ++ [ "HideAs groups not singletons:" <+> tshow wrongHideAsGroups
        | not $ null wrongHideAsGroups ]
     ++ [ "mentioned groups not in content:" <+> tshow missingEffectGroups
        | not $ null missingEffectGroups ]
     ++ [ "hardwired groups not in content:" <+> tshow missingHardwiredGroups
        | not $ null missingHardwiredGroups ]

hardwiredItemGroups :: [GroupName ItemKind]
hardwiredItemGroups =
  -- From Preferences.hs:
  ["condition", "common item"]
    -- the others are optional:
    -- "curious item", "treasure", "any scroll", "any vial",
    -- "potion", "explosive", "any jewelry"
  -- Assorted:
  ++ ["bonus HP", "braced", "asleep", "impressed", "currency", "mobile"]

makeData :: [ItemKind] -> ContentData ItemKind
makeData = makeContentData "ItemKind" iname ifreq validateSingle validateAll
