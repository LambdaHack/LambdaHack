{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of weapons, treasure, organs, blasts, etc.
module Game.LambdaHack.Content.ItemKind
  ( pattern CONDITION, pattern COMMON_ITEM, pattern S_BONUS_HP, pattern S_BRACED, pattern S_ASLEEP, pattern S_IMPRESSED, pattern S_CURRENCY, pattern MOBILE
  , pattern CRAWL_ITEM, pattern TREASURE, pattern ANY_SCROLL, pattern ANY_GLASS, pattern ANY_POTION, pattern ANY_FLASK, pattern EXPLOSIVE, pattern ANY_JEWELRY, pattern S_SINGLE_SPARK, pattern S_SPARK, pattern S_FRAGRANCE
  , pattern HORROR, pattern VALUABLE, pattern UNREPORTED_INVENTORY, pattern AQUATIC
  , ItemKind(..), makeData
  , Aspect(..), Effect(..), Condition(..), DetectKind(..)
  , TimerDice, ThrowMod(..)
  , boostItemKindList, forApplyEffect, forDamageEffect, isDamagingKind
  , strengthOnCombine, strengthOnSmash, getDropOrgans
  , getMandatoryPresentAsFromKind, isEffEscape, isEffEscapeOrAscend
  , timeoutAspect, orEffect, onSmashEffect, onCombineEffect, alwaysDudEffect
  , damageUsefulness, verbMsgNoLonger, verbMsgLess, toVelocity, toLinger
  , timerNone, isTimerNone, foldTimer, toOrganBad, toOrganGood, toOrganNoTimer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , boostItemKind, onSmashOrCombineEffect
  , validateSingle, validateAll, validateDups, validateDamage
  , mandatoryGroups, mandatoryGroupsSingleton
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude
import qualified Game.LambdaHack.Content.RuleKind as RK

import           Data.Binary
import           Data.Hashable (Hashable)
import qualified Data.Ini as Ini
import qualified Data.Text as T
import           Data.Version
import           GHC.Generics (Generic)
import qualified System.Random.SplitMix32 as SM

import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random (nextRandom)
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- * Mandatory item groups

mandatoryGroupsSingleton :: [GroupName ItemKind]
mandatoryGroupsSingleton =
       [S_BONUS_HP, S_BRACED, S_ASLEEP, S_IMPRESSED, S_CURRENCY]

pattern S_BONUS_HP, S_BRACED, S_ASLEEP, S_IMPRESSED, S_CURRENCY :: GroupName ItemKind

mandatoryGroups :: [GroupName ItemKind]
mandatoryGroups =
       [CONDITION, COMMON_ITEM, MOBILE]

pattern CONDITION, COMMON_ITEM, MOBILE :: GroupName ItemKind

-- From Preferences.hs

pattern CONDITION = GroupName "condition"
pattern COMMON_ITEM = GroupName "common item"

-- Assorted

pattern S_BONUS_HP = GroupName "bonus HP"
pattern S_BRACED = GroupName "braced"
pattern S_ASLEEP = GroupName "asleep"
pattern S_IMPRESSED = GroupName "impressed"
pattern S_CURRENCY = GroupName "currency"
pattern MOBILE = GroupName "mobile"

-- * Optional item groups

pattern S_SINGLE_SPARK, S_SPARK, S_FRAGRANCE, CRAWL_ITEM, TREASURE, ANY_SCROLL, ANY_GLASS, ANY_POTION, ANY_FLASK, EXPLOSIVE, ANY_JEWELRY, VALUABLE, UNREPORTED_INVENTORY, AQUATIC, HORROR :: GroupName ItemKind

-- Used in Preferences.hs

pattern S_SINGLE_SPARK = GroupName "single spark"
pattern S_SPARK = GroupName "spark"
pattern S_FRAGRANCE = GroupName "fragrance"

pattern CRAWL_ITEM = GroupName "curious item"
  -- to be used only in long scenarios, such as multi-level dungeon crawl;
  -- may be a powerful or a mundate item, unlike @TREASURE@ item
pattern TREASURE = GroupName "treasure"
  -- particularly powerful items, but may appear in any scenario
pattern ANY_SCROLL = GroupName "scroll"
pattern ANY_GLASS = GroupName "glass"
pattern ANY_POTION = GroupName "potion"
pattern ANY_FLASK = GroupName "flask"
pattern EXPLOSIVE = GroupName "explosive"
pattern ANY_JEWELRY = GroupName "jewelry"

-- * Used elsewhere

pattern VALUABLE = GroupName "valuable"
pattern UNREPORTED_INVENTORY = GroupName "unreported inventory"
pattern AQUATIC = GroupName "aquatic"

pattern HORROR = GroupName "horror"

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
  deriving Show  -- No Eq and Ord to make extending logically sound

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
  | PresentAs (GroupName ItemKind)
                       -- ^ until identified, presents as this unique kind
  | EqpSlot Ability.EqpSlot
                       -- ^ AI and UI flag that leaks item intended use
  | Odds Dice.Dice [Aspect] [Aspect]
                       -- ^ if level-scaled dice roll > 50,
                       --   pick the former aspects, otherwise the latter
  deriving (Show, Eq)

-- | Effects of items. Can be invoked by the item wielder to affect
-- another actor or the wielder himself.
--
-- Various effects of an item kind are all groupped in one list,
-- at the cost of conditionals, sequences, etc., to ensure brevity
-- and simplicity of content definitions. Most effects fire regardless
-- of activation kind (the only exceptions are @OnSmash@ and @OnCombine@
-- effects) so the deviations, handled via the conditionals, are rare
-- and the definitions remain simple. Whether an item can be activated
-- in any particular way, OTOH, is specified via simple flags elsewhere,
-- again, by default, assuming that most activations are possible for all.
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
  | CreateItem (Maybe Int) CStore (GroupName ItemKind) TimerDice
      -- ^ create an item of the group and insert into the store with the given
      --   random timer; it cardinality not specified, roll it
  | DestroyItem Int Int CStore (GroupName ItemKind)
      -- ^ destroy some items of the group from the store; see below about Ints
  | ConsumeItems [(Int, GroupName ItemKind)] [(Int, GroupName ItemKind)]
      -- ^ @ConsumeItems toUse toDestroy@ uses items matching @toUse@
      --   (destroys non-durable, without invoking OnSmash effects;
      --   applies normal effects of durable, without destroying them;
      --   the same behaviour as when transforming terrain using items)
      --   and destroys items matching @toDestroy@, invoking no effects,
      --   regardless of durability;
      --   the items are taken from @CGround@ (but not from @CEqp@),
      --   preferring non-durable (since durable can harm when used
      --   and may be more vauable when destroyed); if not all required items
      --   are present, no item are destroyed; if an item belongs to many groups
      --   in the sum of @toUse@ and @toDestroy@, it counts for all
      --   (otherwise, some orders of destroying would succeed,
      --   while others would not); even if item durable, as many copies
      --   are needed as specified, not just one applied many times;
      --   items are first destroyed and then, if any copies left, applied
  | DropItem Int Int CStore (GroupName ItemKind)
      -- ^ make the actor drop items of the given group from the given store;
      --   the first integer says how many item kinds to drop, the second,
      --   how many copies of each kind to drop;
      --   for non-organs, beware of not dropping all kinds, or cluttering
      --   store with rubbish becomes beneficial
  | Recharge Int Dice.Dice
      -- ^ reduce the cooldown period of this number of discharged items
      --   in the victim's equipment and organs by this dice of game clips;
      --   if the result is negative, set to 0, instantly recharging the item;
      --   starts with weapons with highest raw damage in equipment,
      --   then among organs, then non-weapons in equipment and among organs;
      --   beware of exploiting for healing periodic items
  | Discharge Int Dice.Dice
      -- ^ increase the cooldown period of this number of fully recharged items
      --   in the victim's equipment and organs by this dice of game clips;
      --   starts with weapons with highest raw damage in equipment,
      --   then among organs, then non-weapons in equipment and among organs;
      --   beware of exploiting for hunger inducing and similar organs
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
  | ApplyPerfume          -- ^ remove all smell on the level
  | AtMostOneOf [Effect]  -- ^ try to trigger a single random effect of the list
  | OneOf [Effect]        -- ^ trigger, with equal probability,
                          --   one of the effects that don't end with @UseDud@
  | OnSmash Effect
      -- ^ trigger the effect when item smashed (not when applied nor meleed)
  | OnCombine Effect
      -- ^ trigger the effect only when the actor explicitly desires
      --   to combine items or otherwise subtly tinker with an
      --   item or a tile, e.g., craft items from other items in a workshop;
      --   in particular, don't trigger the effects when entering a tile;
      --   trigger exclusively the effects when activating walkable terrain
  | OnUser Effect  -- ^ apply the effect to the user, not the victim
  | NopEffect                -- ^ nothing happens, @UseDud@, no description
  | AndEffect Effect Effect  -- ^ only fire second effect if first activated
  | OrEffect Effect Effect   -- ^ only fire second effect if first not activated
  | SeqEffect [Effect]       -- ^ fire all effects in order; always suceed
  | When Condition Effect    -- ^ if condition not met, fail without a message;
                             --   better avoided, since AI can't value it well
  | Unless Condition Effect  -- ^ if condition met, fail without a message;
                             --   better avoided, since AI can't value it well
  | IfThenElse Condition Effect Effect
                             -- ^ conditional effect;
                             --   better avoided, since AI can't value it well
  | VerbNoLonger Text Text
      -- ^ a sentence with the actor causing the effect as subject, the given
      --   texts as the verb and the ending of the sentence (that may be
      --   ignored when the message is cited, e.g., as heard by someone)
      --   that is emitted when an activation causes an item to expire;
      --   no spam is emitted if a projectile; the ending is appended
      --   without a space in-between
  | VerbMsg Text Text
      -- ^ as @VerbNoLonger@ but that is emitted whenever the item is activated;
  | VerbMsgFail Text Text
      -- ^ as @VerbMsg@, but a failed effect (returns @UseId@)
  deriving (Show, Eq)

data Condition =
    HpLeq Int
  | HpGeq Int
  | CalmLeq Int
  | CalmGeq Int
  | TriggeredBy Ability.ActivationFlag
  deriving (Show, Eq)

data DetectKind =
    DetectAll
  | DetectActor
  | DetectLoot
  | DetectExit
  | DetectHidden
  | DetectEmbed
  | DetectStash
  deriving (Show, Eq)

-- | Specification of how to randomly roll a timer at item creation
-- to obtain a fixed timer for the item's lifetime.
data TimerDice =
    TimerNone
  | TimerGameTurn Dice.Dice
  | TimerActorTurn Dice.Dice
  deriving Eq

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

instance Binary ThrowMod

instance Hashable ThrowMod

boostItemKindList :: SM.SMGen -> [ItemKind] -> [ItemKind]
boostItemKindList _ [] = []
boostItemKindList initialGen l =
  let (r, _) = nextRandom (length l - 1) initialGen
  in case splitAt r l of
    (pre, i : post) -> pre ++ boostItemKind i : post
    _               -> error $ "" `showFailure` l

boostItemKind :: ItemKind -> ItemKind
boostItemKind i =
  let mainlineLabel (label, _) =
        label `elem` [COMMON_ITEM, CRAWL_ITEM, TREASURE]
  in if any mainlineLabel (ifreq i)
     then i { ifreq = (COMMON_ITEM, 10000) : filter (not . mainlineLabel)
                                                    (ifreq i)
            , iaspects = delete (SetFlag Ability.Unique) $ iaspects i
            }
     else i

-- | Whether the effect has a chance of exhibiting any potentially
-- noticeable behaviour, except when the item is destroyed or combined.
-- We assume at least one of @OneOf@ effects must be noticeable.
forApplyEffect :: Effect -> Bool
forApplyEffect eff = case eff of
  OnSmash{} -> False
  OnCombine{} -> False
  OnUser eff1 -> forApplyEffect eff1
  NopEffect -> False
  AndEffect eff1 eff2 -> forApplyEffect eff1 || forApplyEffect eff2
  OrEffect eff1 eff2 -> forApplyEffect eff1 || forApplyEffect eff2
  SeqEffect effs -> or $ map forApplyEffect effs
  When _ eff1 -> forApplyEffect eff1
  Unless _ eff1 -> forApplyEffect eff1
  IfThenElse _ eff1 eff2 -> forApplyEffect eff1 || forApplyEffect eff2
  VerbNoLonger{} -> False
  VerbMsg{} -> False
  VerbMsgFail{} -> False
  ParalyzeInWater{} -> False  -- barely noticeable, spams when resisted
  _ -> True

-- | Whether a non-nested effect always applies raw damage.
forDamageEffect :: Effect -> Bool
forDamageEffect eff = case eff of
  Burn{} -> True
  RefillHP n | n < 0 -> True
  _ -> False

-- | Whether an item is damaging. Such items may trigger embedded items
-- and may collide with bursting items mid-air.
isDamagingKind :: ItemKind -> Bool
isDamagingKind itemKind = Dice.infDice (idamage itemKind) > 0
                          || any forDamageEffect (ieffects itemKind)

isEffEscape :: Effect -> Bool
isEffEscape Escape{} = True
isEffEscape (AtMostOneOf l) = any isEffEscape l
isEffEscape (OneOf l) = any isEffEscape l
isEffEscape (OnCombine eff) = isEffEscape eff
isEffEscape (OnUser eff) = isEffEscape eff
isEffEscape (AndEffect eff1 eff2) = isEffEscape eff1 || isEffEscape eff2
isEffEscape (OrEffect eff1 eff2) = isEffEscape eff1 || isEffEscape eff2
isEffEscape (SeqEffect effs) = or $ map isEffEscape effs
isEffEscape (When _ eff) = isEffEscape eff
isEffEscape (Unless _ eff) = isEffEscape eff
isEffEscape (IfThenElse _ eff1 eff2) = isEffEscape eff1 || isEffEscape eff2
isEffEscape _ = False

isEffEscapeOrAscend :: Effect -> Bool
isEffEscapeOrAscend Ascend{} = True
isEffEscapeOrAscend Escape{} = True
isEffEscapeOrAscend (AtMostOneOf l) = any isEffEscapeOrAscend l
isEffEscapeOrAscend (OneOf l) = any isEffEscapeOrAscend l
isEffEscapeOrAscend (OnCombine eff) = isEffEscapeOrAscend eff
isEffEscapeOrAscend (OnUser eff) = isEffEscapeOrAscend eff
isEffEscapeOrAscend (AndEffect eff1 eff2) =
  isEffEscapeOrAscend eff1 || isEffEscapeOrAscend eff2
isEffEscapeOrAscend (OrEffect eff1 eff2) =
  isEffEscapeOrAscend eff1 || isEffEscapeOrAscend eff2
isEffEscapeOrAscend (SeqEffect effs) =
  or $ map isEffEscapeOrAscend effs
isEffEscapeOrAscend (When _ eff) = isEffEscapeOrAscend eff
isEffEscapeOrAscend (Unless _ eff) = isEffEscapeOrAscend eff
isEffEscapeOrAscend (IfThenElse _ eff1 eff2) =
  isEffEscapeOrAscend eff1 || isEffEscapeOrAscend eff2
isEffEscapeOrAscend _ = False

timeoutAspect :: Aspect -> Bool
timeoutAspect Timeout{} = True
timeoutAspect _ = False

orEffect :: Effect -> Bool
orEffect OrEffect{} = True
orEffect _ = False

onSmashEffect :: Effect -> Bool
onSmashEffect OnSmash{} = True
onSmashEffect _ = False

onCombineEffect :: Effect -> Bool
onCombineEffect OnCombine{} = True
onCombineEffect _ = False

onSmashOrCombineEffect :: Effect -> Bool
onSmashOrCombineEffect OnSmash{} = True
onSmashOrCombineEffect OnCombine{} = True
onSmashOrCombineEffect _ = False

alwaysDudEffect :: Effect -> Bool
alwaysDudEffect OnSmash{} = True
alwaysDudEffect OnCombine{} = True
alwaysDudEffect NopEffect = True
alwaysDudEffect _ = False

strengthOnSmash :: ItemKind -> [Effect]
strengthOnSmash =
  let f (OnSmash eff) = [eff]
      f _ = []
  in concatMap f . ieffects

strengthOnCombine :: ItemKind -> [Effect]
strengthOnCombine =
  let f (OnCombine eff) = [eff]
      f _ = []
  in concatMap f . ieffects

getDropOrgans :: ItemKind -> [GroupName ItemKind]
getDropOrgans =
  let f (DestroyItem _ _ COrgan grp) = [grp]
      f (DropItem _ _ COrgan grp) = [grp]
      f Impress = [S_IMPRESSED]
      f (AtMostOneOf l) = concatMap f l  -- even remote possibility accepted
      f (OneOf l) = concatMap f l  -- even remote possibility accepted
      f (OnUser eff) = f eff  -- no OnCombine, because checked for potions, etc.
      f (AndEffect eff1 eff2) = f eff1 ++ f eff2  -- not certain, but accepted
      f (OrEffect eff1 eff2) = f eff1 ++ f eff2  -- not certain, but accepted
      f (SeqEffect effs) = concatMap f effs
      f (When _ eff) = f eff
      f (Unless _ eff) = f eff
      f (IfThenElse _ eff1 eff2) = f eff1 ++ f eff2
      f _ = []
  in concatMap f . ieffects

-- Anything under @Odds@ is ignored, because it's not mandatory.
getMandatoryPresentAsFromKind :: ItemKind -> Maybe (GroupName ItemKind)
getMandatoryPresentAsFromKind itemKind =
  let f (PresentAs grp) = [grp]
      f _ = []
  in listToMaybe $ concatMap f (iaspects itemKind)

damageUsefulness :: ItemKind -> Double
damageUsefulness itemKind =
  let v = min 1000 (10 * Dice.meanDice (idamage itemKind))
  in assert (v >= 0) v

verbMsgNoLonger :: Text -> Effect
verbMsgNoLonger name = VerbNoLonger ("be no longer" <+> name) "."

verbMsgLess :: Text -> Effect
verbMsgLess name = VerbMsg ("appear less" <+> name) "."

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
toOrganBad grp nDm = CreateItem Nothing COrgan grp (TimerGameTurn nDm)

toOrganGood :: GroupName ItemKind -> Dice.Dice -> Effect
toOrganGood grp nDm = CreateItem Nothing COrgan grp (TimerActorTurn nDm)

toOrganNoTimer :: GroupName ItemKind -> Effect
toOrganNoTimer grp = CreateItem Nothing COrgan grp TimerNone

-- | Catch invalid item kind definitions.
validateSingle :: RK.RuleContent -> ItemKind -> [Text]
validateSingle ruleContent
               ik@ItemKind{..} =
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
          equipable = SetFlag Ability.Equipable `elem` iaspects
          meleeable = SetFlag Ability.Meleeable `elem` iaspects
          likelyTemplate = case ifreq of
            [(grp, 1)] -> "unknown" `T.isSuffixOf` fromGroupName grp
            _ -> False
          likelyException = isymbol `elem` [RK.rsymbolFood ruleContent, RK.rsymbolNecklace ruleContent, RK.rsymbolWand ruleContent] || likelyTemplate 
      in [ "EqpSlot specified but not Equipable nor Meleeable"
         | length ts == 1 && not equipable && not meleeable ]
         ++ [ "EqpSlot not specified but Equipable or Meleeable and not a likely organ or necklace or template"
            | not likelyException
              && length ts == 0 && (equipable || meleeable) ]
         ++ [ "More than one EqpSlot specified"
            | length ts > 1 ] )
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
          f PresentAs{} = True
          f _ = False
          ts = filter f iaspects
      in ["more than one PresentAs specification" | length ts > 1])
  ++ concatMap (validateDups ik) (map SetFlag [minBound .. maxBound])
  ++ (let f :: Effect -> Bool
          f VerbNoLonger{} = True
          f _ = False
      in validateOnlyOne ieffects "VerbNoLonger" f)  -- may be duped if nested
  ++ (let f :: Effect -> Bool
          f VerbMsg{} = True
          f _ = False
      in validateOnlyOne ieffects "VerbMsg" f)  -- may be duplicated if nested
  ++ (let f :: Effect -> Bool
          f VerbMsgFail{} = True
          f _ = False
      in validateOnlyOne ieffects "VerbMsgFail" f)  -- may be duped if nested
  ++ validateNotNested ieffects "OnSmash or OnCombine" onSmashOrCombineEffect
       -- but duplicates permitted
  ++ let nonPositiveBurn :: Effect -> Bool
         nonPositiveBurn (Burn d) = Dice.infDice d <= 0
         nonPositiveBurn _ = False
         containingNonPositiveBurn =
           filter (checkSubEffectProp nonPositiveBurn) ieffects
     in [ "effects with non-positive Burn:" <+> tshow containingNonPositiveBurn
        | not $ null containingNonPositiveBurn ]
  ++ let emptyOneOf :: Effect -> Bool
         emptyOneOf (AtMostOneOf []) = True
         emptyOneOf (OneOf []) = True
         emptyOneOf _ = False
         containingEmptyOneOf = filter (checkSubEffectProp emptyOneOf) ieffects
     in [ "effects with empty AtMostOneOf or OneOf:"
          <+> tshow containingEmptyOneOf
        | not $ null containingEmptyOneOf ]
  ++ (let nonPositiveEffect :: Effect -> Bool
          nonPositiveEffect (CreateItem (Just n) _ _ _) | n <= 0 = True
          nonPositiveEffect (DestroyItem n k _ _) | n <= 0 || k <= 0 = True
          nonPositiveEffect (ConsumeItems tools raw)
            | any ((<= 0) . fst) (tools ++ raw) = True
          nonPositiveEffect (DropItem n k _ _) | n <= 0 || k <= 0 = True
          nonPositiveEffect (Detect _ n) | n <= 0 = True
          nonPositiveEffect _ = False
          containingNonPositiveEffect =
            filter (checkSubEffectProp nonPositiveEffect) ieffects
      in [ "effects with forbidden non-positive parameters:"
           <+> tshow containingNonPositiveEffect
         | not $ null containingNonPositiveEffect ])
  ++ (let nonPositiveEffect :: Effect -> Bool
          nonPositiveEffect (Summon _ d) | Dice.infDice d <= 0 = True
          nonPositiveEffect (Paralyze d) | Dice.infDice d <= 0 = True
          nonPositiveEffect (ParalyzeInWater d) | Dice.infDice d <= 0 = True
          nonPositiveEffect (InsertMove d) | Dice.infDice d <= 0 = True
          nonPositiveEffect (Teleport d) | Dice.infDice d <= 0 = True
          nonPositiveEffect (CreateItem _ _ _ (TimerGameTurn d))
            | Dice.infDice d <= 0 = True
          nonPositiveEffect (CreateItem _ _ _ (TimerActorTurn d))
            | Dice.infDice d <= 0 = True
          nonPositiveEffect (Recharge n d)
            | n <= 0 || Dice.infDice d <= 0 = True
          nonPositiveEffect (Discharge n d)
            | n <= 0 || Dice.infDice d <= 0 = True
          nonPositiveEffect _ = False
          containingNonPositiveEffect =
            filter (checkSubEffectProp nonPositiveEffect) ieffects
      in [ "effects with forbidden potentially non-positive or negative number or dice:"
           <+> tshow containingNonPositiveEffect
         | not $ null containingNonPositiveEffect ])

-- We only check there are no duplicates at top level. If it may be nested,
-- it may presumably be duplicated inside the nesting as well.
validateOnlyOne :: [Effect] -> Text -> (Effect -> Bool) -> [Text]
validateOnlyOne effs t f =
  let ts = filter f effs
  in ["more than one" <+> t <+> "specification" | length ts > 1]

-- We check it's not nested one nor more levels.
validateNotNested :: [Effect] -> Text -> (Effect -> Bool) -> [Text]
validateNotNested effs t f =
  let g (AtMostOneOf l) = any h l
      g (OneOf l) = any h l
      g (OnSmash effect) = h effect
      g (OnCombine effect) = h effect
      g (OnUser effect) = h effect
      g (AndEffect eff1 eff2) = h eff1 || h eff2
      g (OrEffect eff1 eff2) = h eff1 || h eff2
      g (SeqEffect effs2) = or $ map h effs2
      g (When _ effect) = h effect
      g (Unless _ effect) = h effect
      g (IfThenElse _ eff1 eff2) = h eff1 || h eff2
      g _ = False
      h effect = f effect || g effect
      ts = filter g effs
  in [ "effect" <+> t <+> "should be specified at top level, not nested"
     | length ts > 0 ]

checkSubEffectProp :: (Effect -> Bool) -> Effect -> Bool
checkSubEffectProp f eff =
  let g (AtMostOneOf l) = any h l
      g (OneOf l) = any h l
      g (OnSmash effect) = h effect
      g (OnCombine effect) = h effect
      g (OnUser effect) = h effect
      g (AndEffect eff1 eff2) = h eff1 || h eff2
      g (OrEffect eff1 eff2) = h eff1 || h eff2
      g (SeqEffect effs) = or $ map h effs
      g (When _ effect) = h effect
      g (Unless _ effect) = h effect
      g (IfThenElse _ eff1 eff2) = h eff1 || h eff2
      g _ = False
      h effect = f effect || g effect
  in h eff

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
  let f :: Aspect -> Bool
      f PresentAs{} = True
      f _ = False
      wrongPresentAsGroups =
        [ cgroup
        | k <- content
        , let (cgroup, notSingleton) = case find f (iaspects k) of
                Just (PresentAs grp) | not $ oisSingletonGroup coitem grp ->
                  (grp, True)
                _ -> (undefined, False)
        , notSingleton
        ]
  in [ "PresentAs groups not singletons:" <+> tshow wrongPresentAsGroups
     | not $ null wrongPresentAsGroups ]

makeData :: [ItemKind] -> [GroupName ItemKind] -> [GroupName ItemKind] -> RK.RuleContent
         -> ContentData ItemKind
makeData content groupNamesSingleton groupNames ruleContent =
  let allGroupNamesTooLong = filter ((> 23) . T.length . fromGroupName)
                             $ groupNamesSingleton ++ groupNames
  in assert (null allGroupNamesTooLong
             `blame` "ItemKind: some item group names too long"
             `swith` allGroupNamesTooLong) $
     makeContentData "ItemKind" iname ifreq (validateSingle ruleContent) validateAll content
                     (mandatoryGroupsSingleton ++ groupNamesSingleton)
                     (mandatoryGroups ++ groupNames)
