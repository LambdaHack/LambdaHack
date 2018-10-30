{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Abilities of items, actors and factions.
module Game.LambdaHack.Common.Ability
  ( Skill(..), Skills, Flag(..), Flags(..), Tactic(..), EqpSlot(..)
  , getSk, addSk, checkFl, skillsToList
  , zeroSkills, addSkills, sumScaledSkills
  , nameTactic, describeTactic, tacticSkills
  , blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , compactSkills, scaleSkills
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

-- | Actor and faction skills. They are a subset of actor aspects.
-- See 'skillDesc' for documentation.
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
  | SkSight  -- ^ FOV radius, where 1 means a single tile FOV
  | SkSmell
  | SkShine
  | SkNocto
  | SkHearing
  | SkAggression
  | SkOdor
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

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
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Item flag aspects.
data Flag =
    Fragile       -- ^ drop and break at target tile, even if no hit
  | Lobable       -- ^ drop at target tile, even if no hit
  | Durable       -- ^ don't break even when hitting or applying
  | Equipable     -- ^ AI and UI flag: consider equipping (may or may not
                  --   have 'EqpSlot', e.g., if the benefit is periodic)
  | Meleeable     -- ^ AI and UI flag: consider meleeing with
  | Precious      -- ^ AI and UI flag: don't risk identifying by use;
                  --   also, can't throw or apply if not calm enough
  | Blast         -- ^ the item is an explosion blast particle
  | Unique        -- ^ at most one copy can ever be generated
  | Periodic      -- ^ in eqp, triggered as often as @Timeout@ permits
  | MinorEffects  -- ^ override: the effects on this item are considered
                  --   minor and so not causing identification on use,
                  --   and so this item will identify on pick-up
  deriving (Show, Eq, Ord, Generic, Enum)

newtype Flags = Flags {flags :: ES.EnumSet Flag}
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Tactic of non-leader actors. Apart of determining AI operation,
-- each tactic implies a skill modifier, that is added to the non-leader skills
-- defined in 'fskillsOther' field of 'Player'.
data Tactic =
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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Tactic

instance NFData Tactic

instance Hashable Tactic

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
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData Skill

instance NFData Flag

instance NFData EqpSlot

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

compactSkills :: EM.EnumMap Skill Int -> EM.EnumMap Skill Int
compactSkills = EM.filter (/= 0)

addSkills :: Skills -> Skills -> Skills
addSkills (Skills sk1) (Skills sk2) =
  Skills $ compactSkills $ EM.unionWith (+) sk1 sk2

scaleSkills :: Int -> EM.EnumMap Skill Int -> EM.EnumMap Skill Int
scaleSkills n = EM.map (n *)

sumScaledSkills :: [(Skills, Int)] -> Skills
sumScaledSkills l = Skills $ compactSkills $ EM.unionsWith (+)
                           $ map (\(Skills sk, k) -> scaleSkills k sk) l

nameTactic :: Tactic -> Text
nameTactic TExplore        = "explore"
nameTactic TFollow         = "follow freely"
nameTactic TFollowNoItems  = "follow only"
nameTactic TMeleeAndRanged = "fight only"
nameTactic TMeleeAdjacent  = "melee only"
nameTactic TBlock          = "block only"
nameTactic TRoam           = "roam freely"
nameTactic TPatrol         = "patrol area"

describeTactic :: Tactic -> Text
describeTactic TExplore = "investigate unknown positions, chase targets"
describeTactic TFollow = "follow leader's target or position, grab items"
describeTactic TFollowNoItems =
  "follow leader's target or position, ignore items"
describeTactic TMeleeAndRanged =
  "engage in both melee and ranged combat, don't move"
describeTactic TMeleeAdjacent = "engage exclusively in melee, don't move"
describeTactic TBlock = "block and wait, don't move"
describeTactic TRoam = "move freely, chase targets"
describeTactic TPatrol = "find and patrol an area (WIP)"

tacticSkills :: Tactic -> Skills
tacticSkills TExplore = zeroSkills
tacticSkills TFollow = zeroSkills
tacticSkills TFollowNoItems = ignoreItems
tacticSkills TMeleeAndRanged = meleeAndRanged
tacticSkills TMeleeAdjacent = meleeAdjacent
tacticSkills TBlock = blockOnly
tacticSkills TRoam = zeroSkills
tacticSkills TPatrol = zeroSkills

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
