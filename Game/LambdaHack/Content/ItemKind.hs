{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable #-}
-- | The type of kinds of weapons, treasure, organs, blasts and actors.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..)
  , Effect(..), TimerDice(..)
  , Aspect(..), ThrowMod(..)
  , Feature(..), EqpSlot(..)
  , forApplyEffect, forIdEffect
  , toDmg, toVelocity, toLinger, toOrganGameTurn, toOrganActorTurn, toOrganNone
  , validateSingleItemKind, validateAllItemKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import Data.Hashable (Hashable)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol  :: !Char              -- ^ map symbol
  , iname    :: !Text              -- ^ generic name
  , ifreq    :: !(Freqs ItemKind)  -- ^ frequency within groups
  , iflavour :: ![Flavour]         -- ^ possible flavours
  , icount   :: !Dice.Dice         -- ^ created in that quantity
  , irarity  :: !Rarity            -- ^ rarity on given depths
  , iverbHit :: !MU.Part           -- ^ the verb&noun for applying and hit
  , iweight  :: !Int               -- ^ weight in grams
  , idamage  :: ![(Int, Dice.Dice)]  -- ^ frequency of basic impact damage
  , iaspects :: ![Aspect]          -- ^ keep the aspect continuously
  , ieffects :: ![Effect]          -- ^ cause the effect when triggered
  , ifeature :: ![Feature]         -- ^ public properties
  , idesc    :: !Text              -- ^ description
  , ikit     :: ![(GroupName ItemKind, CStore)]
                                   -- ^ accompanying organs and items
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | Effects of items. Can be invoked by the item wielder to affect
-- another actor or the wielder himself. Many occurences in the same item
-- are possible. Constructors are sorted vs increasing impact/danger.
data Effect =
    -- Ordinary effects.
    ELabel !Text        -- ^ secret (learned as effect) label of the item
  | EqpSlot !EqpSlot    -- ^ AI and UI flag that leaks item properties
  | Burn !Dice.Dice
  | Explode !(GroupName ItemKind)
      -- ^ explode, producing this group of blasts
  | RefillHP !Int
  | RefillCalm !Int
  | Dominate
  | Impress
  | Summon !(GroupName ItemKind) !Dice.Dice
  | Ascend !Bool
  | Escape
  | Paralyze !Dice.Dice
  | InsertMove !Dice.Dice
  | Teleport !Dice.Dice
  | CreateItem !CStore !(GroupName ItemKind) !TimerDice
      -- ^ create an item of the group and insert into the store with the given
      --   random timer
  | DropItem !Int !Int !CStore !(GroupName ItemKind)
  | PolyItem
  | Identify
  | Detect !Int
  | DetectActor !Int
  | DetectItem !Int
  | DetectExit !Int
  | DetectHidden !Int
  | SendFlying !ThrowMod
  | PushActor !ThrowMod
  | PullActor !ThrowMod
  | DropBestWeapon
  | ActivateInv !Char   -- ^ symbol @' '@ means all
  | ApplyPerfume
    -- Exotic effects follow.
  | OneOf ![Effect]
  | OnSmash !Effect     -- ^ trigger if item smashed (not applied nor meleed)
  | Recharging !Effect  -- ^ this effect inactive until timeout passes
  | Temporary !Text
      -- ^ the item is temporary, vanishes at even void Periodic activation,
      --   unless Durable and not Fragile, and shows message with
      --   this verb at last copy activation or at each activation
      --   unless Durable and Fragile
  | Unique              -- ^ at most one copy can ever be generated
  | Periodic            -- ^ in eqp, triggered as often as @Timeout@ permits
  deriving (Show, Eq, Ord, Generic)

instance NFData Effect

forApplyEffect :: Effect -> Bool
forApplyEffect eff = case eff of
  ELabel{} -> False
  EqpSlot{} -> False
  OnSmash{} -> False
  Temporary{} -> False
  Unique -> False
  Periodic -> False
  _ -> True

forIdEffect :: Effect -> Bool
forIdEffect eff = case eff of
  ELabel{} -> False
  EqpSlot{} -> False
  OnSmash{} -> False
  Explode{} -> False  -- tentative; needed for rings to auto-identify
  Unique -> False
  Periodic -> False
  _ -> True

data TimerDice =
    TimerNone
  | TimerGameTurn !Dice.Dice
  | TimerActorTurn !Dice.Dice
  deriving (Eq, Ord, Generic)

instance Show TimerDice where
  show TimerNone = "0"
  show (TimerGameTurn nDm) =
    show nDm ++ " " ++ if nDm == 1 then "turn" else "turns"
  show (TimerActorTurn nDm) =
    show nDm ++ " " ++ if nDm == 1 then "move" else "moves"

instance NFData TimerDice

-- | Aspects of items. Those that are named @Add*@ are additive
-- (starting at 0) for all items wielded by an actor and they affect the actor.
data Aspect =
    Timeout !Dice.Dice         -- ^ some effects disabled until item recharges
  | AddHurtMelee !Dice.Dice    -- ^ percentage damage bonus in melee
  | AddArmorMelee !Dice.Dice   -- ^ percentage armor bonus against melee
  | AddArmorRanged !Dice.Dice  -- ^ percentage armor bonus against ranged
  | AddMaxHP !Dice.Dice        -- ^ maximal hp
  | AddMaxCalm !Dice.Dice      -- ^ maximal calm
  | AddSpeed !Dice.Dice        -- ^ speed in m/10s (not of a projectile!)
  | AddSight !Dice.Dice        -- ^ FOV radius, where 1 means a single tile
  | AddSmell !Dice.Dice        -- ^ smell radius, where 1 means a single tile
  | AddShine !Dice.Dice        -- ^ shine radius, where 1 means a single tile
  | AddNocto !Dice.Dice        -- ^ noctovision radius, where 1 is single tile
  | AddAggression !Dice.Dice   -- ^ aggression, especially closing in for melee
  | AddAbility !Ability.Ability !Dice.Dice  -- ^ bonus to an ability
  deriving (Show, Eq, Ord, Generic)

-- | Parameters modifying a throw of a projectile or flight of pushed actor.
-- Not additive and don't start at 0.
data ThrowMod = ThrowMod
  { throwVelocity :: !Int  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: !Int  -- ^ fly for this percentage of 2 turns
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData ThrowMod

-- | Features of item. Affect only the item in question, not the actor,
-- and so not additive in any sense.
data Feature =
    Fragile            -- ^ drop and break at target tile, even if no hit
  | Lobable            -- ^ drop at target tile, even if no hit
  | Durable            -- ^ don't break even when hitting or applying
  | ToThrow !ThrowMod  -- ^ parameters modifying a throw
  | Identified         -- ^ the item starts identified
  | Applicable         -- ^ AI and UI flag: consider applying
  | Equipable          -- ^ AI and UI flag: consider equipping (independent of
                       -- ^ 'EqpSlot', e.g., in case of mixed blessings)
  | Meleeable          -- ^ AI and UI flag: consider meleeing with
  | Precious           -- ^ AI and UI flag: don't risk identifying by use
                       --   also, can't throw or apply if not calm enough;
  | Tactic !Tactic     -- ^ overrides actor's tactic
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotMiscBonus
  | EqpSlotAddHurtMelee
  | EqpSlotAddArmorMelee
  | EqpSlotAddArmorRanged
  | EqpSlotAddMaxHP
  | EqpSlotAddSpeed
  | EqpSlotAddSight
  | EqpSlotLightSource
  | EqpSlotWeapon
  | EqpSlotMiscAbility
  | EqpSlotAbMove
  | EqpSlotAbMelee
  | EqpSlotAbDisplace
  | EqpSlotAbAlter
  | EqpSlotAbProject
  | EqpSlotAbApply
  -- Do not use in content:
  | EqpSlotAddMaxCalm
  | EqpSlotAddSmell
  | EqpSlotAddNocto
  | EqpSlotAddAggression
  | EqpSlotAbWait
  | EqpSlotAbMoveItem
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData EqpSlot

instance Hashable Effect

instance Hashable TimerDice

instance Hashable Aspect

instance Hashable ThrowMod

instance Hashable Feature

instance Hashable EqpSlot

instance Binary Effect

instance Binary TimerDice

instance Binary Aspect

instance Binary ThrowMod

instance Binary Feature

instance Binary EqpSlot

toDmg :: Dice.Dice -> [(Int, Dice.Dice)]
toDmg dmg = [(1, dmg)]

toVelocity :: Int -> Feature
toVelocity n = ToThrow $ ThrowMod n 100

toLinger :: Int -> Feature
toLinger n = ToThrow $ ThrowMod 100 n

toOrganGameTurn :: GroupName ItemKind -> Dice.Dice -> Effect
toOrganGameTurn grp nDm = CreateItem COrgan grp (TimerGameTurn nDm)

toOrganActorTurn :: GroupName ItemKind -> Dice.Dice -> Effect
toOrganActorTurn grp nDm = CreateItem COrgan grp (TimerActorTurn nDm)

toOrganNone :: GroupName ItemKind -> Effect
toOrganNone grp = CreateItem COrgan grp TimerNone

-- | Catch invalid item kind definitions.
validateSingleItemKind :: ItemKind -> [Text]
validateSingleItemKind ItemKind{..} =
  [ "iname longer than 23" | T.length iname > 23 ]
  ++ validateRarity irarity
  -- Reject duplicate Timeout, because it's not additive.
  ++ let timeoutAspect :: Aspect -> Bool
         timeoutAspect Timeout{} = True
         timeoutAspect _ = False
         ts = filter timeoutAspect iaspects
     in ["more than one Timeout specification" | length ts > 1]

-- | Validate all item kinds.
validateAllItemKind :: [ItemKind] -> [Text]
validateAllItemKind content =
  let kindFreq :: S.Set (GroupName ItemKind)  -- cf. Kind.kindFreq
      kindFreq = let tuples = [ cgroup
                              | k <- content
                              , (cgroup, n) <- ifreq k
                              , n > 0 ]
                 in S.fromList tuples
      missingGroups = [ cgroup
                      | k <- content
                      , (cgroup, _) <- ikit k
                      , S.notMember cgroup kindFreq ]
      errorMsg = case missingGroups of
        [] -> []
        _ -> ["no groups" <+> tshow missingGroups
              <+> "among content that has groups"
              <+> tshow (S.elems kindFreq)]
  in errorMsg
