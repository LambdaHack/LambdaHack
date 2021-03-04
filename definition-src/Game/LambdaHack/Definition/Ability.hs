{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Abilities of items, actors and factions.
module Game.LambdaHack.Definition.Ability
  ( Skill(..), Skills, Flag(..), Flags(..), Doctrine(..), EqpSlot(..)
  , getSk, addSk, checkFl, skillsToList
  , zeroSkills, addSkills, sumScaledSkills
  , nameDoctrine, describeDoctrine, doctrineSkills
  , blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , scaleSkills
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

-- | Actor and faction skills. They are a subset of actor aspects.
-- See 'Game.LambdaHack.Client.UI.EffectDescription.skillDesc'
-- for documentation.
data Skill =
  -- Stats, that is skills affecting permitted actions.
    SkMove
  | SkMelee
  | SkDisplace
  | SkAlter
  | SkWait
  | SkMoveItem
  | SkProject
  | SkApply
  -- Assorted skills.
  | SkSwimming
  | SkFlying
  | SkHurtMelee
  | SkArmorMelee
  | SkArmorRanged
  | SkMaxHP
  | SkMaxCalm
  | SkSpeed
  | SkSight  -- ^ FOV radius, where 1 means a single tile FOV area
  | SkSmell
  | SkShine
  | SkNocto
  | SkHearing
  | SkAggression
  | SkOdor
  | SkDeflectRanged  -- ^ intended to reflect how many items granting complete
                     --   invulnerability are among organs and equipment;
                     --   this is not strength of deflection nor duration, etc.
  | SkDeflectMelee   -- ^ see above
  deriving (Show, Eq, Enum, Bounded, Generic)

-- | Strength of particular skills. This is cumulative from actor
-- organs and equipment and so pertain to an actor as well as to items.
--
-- This representation is sparse, so better than a record when there are more
-- item kinds (with few skills) than actors (with many skills),
-- especially if the number of skills grows as the engine is developed.
-- It's also easier to code and maintain.
--
-- The tree is by construction sparse, so the derived equality is semantical.
newtype Skills = Skills {skills :: EM.EnumMap Skill Int}
  deriving (Show, Eq, Ord, Hashable, Binary)

-- | Item flag aspects.
data Flag =
    Fragile       -- ^ as a projectile, break at target tile, even if no hit;
                  --   also, at each periodic activation a copy is destroyed
                  --   and all other copies require full cooldown (timeout)
  | Lobable       -- ^ drop at target tile, even if no hit
  | Durable       -- ^ don't break even when hitting or applying
  | Equipable     -- ^ AI and UI flag: consider equipping (may or may not
                  --   have 'EqpSlot', e.g., if the benefit is periodic)
  | Meleeable     -- ^ AI and UI flag: consider meleeing with
  | Benign        -- ^ AI and UI flag: the item is not meant to harm
  | Precious      -- ^ AI and UI flag: don't risk identifying by use;
                  --   also, can't throw or apply if not calm enough;
                  --   also may be used for UI flavour or AI hints
  | Blast         -- ^ the item is an explosion blast particle
  | Condition     -- ^ item is a condition (buff or de-buff) of an actor
                  --   and is displayed as such and activated on drop;
                  --   this differs from belonging to the @CONDITION@ group,
                  --   which doesn't guarantee any behaviour or display,
                  --   but governs removal by items that drop @CONDITION@
  | Unique        -- ^ at most one copy can ever be generated
  | MetaGame      -- ^ once identified, the item is known until savefile deleted
  | MinorEffects  -- ^ override: the effects on this item are considered
                  --   minor and so not causing identification on use,
                  --   and so this item will identify on pick-up
  | -- The flags below specify all conditions under which the item activates,
    -- charges permitting, in addition to universal conditions, which are
    -- hitting with the item and explicitly applying the item by an actor
    -- (item destruction and combining only pertain to select effects).
    Periodic      -- ^ at most one of any copies without cooldown (timeout)
                  --   activates each turn; the cooldown required after
                  --   activation is specified in @Timeout@ (or is zero);
                  --   the initial cooldown can also be specified
                  --   as @TimerDice@ in @CreateItem@ effect; uniquely, this
                  --   activation never destroys a copy, unless item is fragile;
                  --   all this happens only for items in equipment or organs;
                  --   kinetic damage is not applied
  | UnderRanged   -- ^ activates when non-projectile actor with this item
                  --   as equipment or organ is under ranged attack;
                  --   kinetic damage is not applied
  | UnderMelee    -- ^ activates when non-projectile actor with this item
                  --   as equipment or organ is under melee attack;
                  --   kinetic damage is not applied
  deriving (Show, Eq, Enum, Bounded, Generic)

newtype Flags = Flags {flags :: ES.EnumSet Flag}
  deriving (Show, Eq, Ord, Hashable, Binary)

-- | Doctrine of non-leader actors. Apart of determining AI operation,
-- each doctrine implies a skill modifier, that is added to the non-leader
-- skills defined in @fskillsOther@ field of @Player@.
data Doctrine =
    TExplore  -- ^ if enemy nearby, attack, if no items, etc., explore unknown
  | TFollow   -- ^ always follow leader's target or his position if no target
  | TFollowNoItems   -- ^ follow but don't do any item management nor use
  | TMeleeAndRanged  -- ^ only melee and do ranged combat
  | TMeleeAdjacent   -- ^ only melee (or wait)
  | TBlock    -- ^ always only wait, even if enemy in melee range
  | TRoam     -- ^ if enemy nearby, attack, if no items, etc., roam randomly
  | TPatrol   -- ^ find an open and uncrowded area, patrol it according
              --   to sight radius and fallback temporarily to @TRoam@
              --   when enemy is seen by the faction and is within
              --   the actor's sight radius
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Binary Doctrine

instance Hashable Doctrine

-- | AI and UI hints about the role of the item.
data EqpSlot =
    EqpSlotMove
  | EqpSlotMelee
  | EqpSlotDisplace
  | EqpSlotAlter
  | EqpSlotWait
  | EqpSlotMoveItem
  | EqpSlotProject
  | EqpSlotApply
  | EqpSlotSwimming
  | EqpSlotFlying
  | EqpSlotHurtMelee
  | EqpSlotArmorMelee
  | EqpSlotArmorRanged
  | EqpSlotMaxHP
  | EqpSlotSpeed
  | EqpSlotSight
  | EqpSlotShine
  | EqpSlotMiscBonus
  | EqpSlotWeaponFast
  | EqpSlotWeaponBig
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Skill where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary Flag where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary EqpSlot where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Skill

instance Hashable Flag

instance Hashable EqpSlot

getSk :: Skill -> Skills -> Int
{-# INLINE getSk #-}
getSk sk (Skills skills) = EM.findWithDefault 0 sk skills

addSk :: Skill -> Int -> Skills -> Skills
addSk sk n = addSkills (Skills $ EM.singleton sk n)

checkFl :: Flag -> Flags -> Bool
{-# INLINE checkFl #-}
checkFl flag (Flags flags) = flag `ES.member` flags

skillsToList :: Skills -> [(Skill, Int)]
skillsToList (Skills sk) = EM.assocs sk

zeroSkills :: Skills
zeroSkills = Skills EM.empty

-- This avoids costly compaction (required for Eq) even in case of adding
-- empty skills, etc. This function is used a lot.
addSkills :: Skills -> Skills -> Skills
addSkills (Skills sk1) (Skills sk2) =
  let combine _ s1 s2 = case s1 + s2 of
        0 -> Nothing
        s -> Just s
  in Skills $ EM.mergeWithKey combine id id sk1 sk2

scaleSkills :: Skills -> Int -> Skills
scaleSkills _ 0 = zeroSkills
scaleSkills (Skills sk) n = Skills $ EM.map (n *) sk

sumScaledSkills :: [(Skills, Int)] -> Skills
sumScaledSkills = foldr addSkills zeroSkills . map (uncurry scaleSkills)

nameDoctrine :: Doctrine -> Text
nameDoctrine TExplore        = "explore"
nameDoctrine TFollow         = "follow freely"
nameDoctrine TFollowNoItems  = "follow only"
nameDoctrine TMeleeAndRanged = "fight only"
nameDoctrine TMeleeAdjacent  = "melee only"
nameDoctrine TBlock          = "block only"
nameDoctrine TRoam           = "roam freely"
nameDoctrine TPatrol         = "patrol area"

describeDoctrine :: Doctrine -> Text
describeDoctrine TExplore = "investigate unknown positions, chase targets"
describeDoctrine TFollow = "follow pointman's target or position, grab items"
describeDoctrine TFollowNoItems =
  "follow pointman's target or position, ignore items"
describeDoctrine TMeleeAndRanged =
  "engage in both melee and ranged combat, don't move"
describeDoctrine TMeleeAdjacent = "engage exclusively in melee, don't move"
describeDoctrine TBlock = "block and wait, don't move"
describeDoctrine TRoam = "move freely, chase targets"
describeDoctrine TPatrol = "find and patrol an area (WIP)"

doctrineSkills :: Doctrine -> Skills
doctrineSkills TExplore = zeroSkills
doctrineSkills TFollow = zeroSkills
doctrineSkills TFollowNoItems = ignoreItems
doctrineSkills TMeleeAndRanged = meleeAndRanged
doctrineSkills TMeleeAdjacent = meleeAdjacent
doctrineSkills TBlock = blockOnly
doctrineSkills TRoam = zeroSkills
doctrineSkills TPatrol = zeroSkills

minusTen, blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems :: Skills

-- To make sure only a lot of weak items can override move-only-leader, etc.
minusTen = Skills $ EM.fromDistinctAscList
                  $ zip [SkMove .. SkApply] (repeat (-10))

blockOnly = Skills $ EM.delete SkWait $ skills minusTen

meleeAdjacent = Skills $ EM.delete SkMelee $ skills blockOnly

-- Melee and reaction fire.
meleeAndRanged = Skills $ EM.delete SkProject $ skills meleeAdjacent

ignoreItems = Skills $ EM.fromList
                     $ zip [SkMoveItem, SkProject, SkApply] (repeat (-10))
