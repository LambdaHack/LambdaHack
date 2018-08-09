{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Skill(..), Skills, Flag(..), Flags(..), EqpSlot(..)
  , getSk, addSk, checkFl, skillsToList
  , zeroSkills, unitSkills, addSkills, sumScaledSkills
  , tacticSkills, blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems
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

import Game.LambdaHack.Common.Misc

-- | Actor and faction abilities.
data Skill =
  -- Basic abilities affecting permitted actions.
    SkMove
  | SkMelee
  | SkDisplace
  | SkAlter
  | SkWait
  | SkMoveItem
  | SkProject
  | SkApply
  -- Assorted abilities.
  | SkHurtMelee    -- ^ percentage damage bonus in melee
  | SkArmorMelee   -- ^ percentage armor bonus against melee
  | SkArmorRanged  -- ^ percentage armor bonus against ranged
  | SkMaxHP        -- ^ maximal hp
  | SkMaxCalm      -- ^ maximal calm
  | SkSpeed        -- ^ speed in m/10s (not when pushed or pulled)
  | SkSight        -- ^ FOV radius, where 1 means a single tile FOV
  | SkSmell        -- ^ smell radius
  | SkShine        -- ^ shine radius
  | SkNocto        -- ^ noctovision radius
  | SkAggression   -- ^ aggression, e.g., when closing in for melee
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | Skill level in particular abilities.
--
-- This representation is sparse, so better than a record when there are more
-- item kinds (with few abilities) than actors (with many abilities),
-- especially if the number of abilities grows as the engine is developed.
-- It's also easier to code and maintain.
--
-- The tree is by construction sparse, so equality is semantical.
newtype Skills = Skills {skills :: EM.EnumMap Skill Int}
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Item features.
data Flag =
    Fragile       -- ^ drop and break at target tile, even if no hit
  | Lobable       -- ^ drop at target tile, even if no hit
  | Durable       -- ^ don't break even when hitting or applying
  | Equipable     -- ^ AI and UI flag: consider equipping (independent of
                  --   'EqpSlot', e.g., in case of mixed blessings)
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

-- | AI and UI hints about the role of the item.
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
addSk sk n skills = addSkills (Skills $ EM.singleton sk n) skills

checkFl :: Flag -> Flags -> Bool
{-# INLINE checkFl #-}
checkFl flag (Flags flags) = flag `ES.member` flags

skillsToList :: Skills -> [(Skill, Int)]
skillsToList (Skills sk) = EM.assocs sk

zeroSkills :: Skills
zeroSkills = Skills EM.empty

unitSkills :: Skills
unitSkills =
  Skills $ EM.fromDistinctAscList $ zip [SkMove .. SkApply] (repeat 1)

compactSkills :: EM.EnumMap Skill Int -> EM.EnumMap Skill Int
compactSkills = EM.filter (/= 0)

addSkills :: Skills -> Skills -> Skills
addSkills (Skills sk1) (Skills sk2) =
  Skills $ compactSkills $ EM.unionWith (+) sk1 sk2

scaleSkills :: Int -> EM.EnumMap Skill Int -> EM.EnumMap Skill Int
scaleSkills n skills = EM.map (n *) skills

sumScaledSkills :: [(Skills, Int)] -> Skills
sumScaledSkills l = Skills $ compactSkills $ EM.unionsWith (+)
                           $ map (\(Skills sk, k) -> scaleSkills k sk) l

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
