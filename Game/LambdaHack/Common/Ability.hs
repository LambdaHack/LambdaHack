{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..), Skills, Feature(..), Flags(..), EqpSlot(..)
  , getAb, addAb, checkFl, skillsToList
  , zeroSkills, unitSkills, addSkills, sumScaledAbility
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
data Ability =
  -- Basic abilities affecting permitted actions.
    AbMove
  | AbMelee
  | AbDisplace
  | AbAlter
  | AbWait
  | AbMoveItem
  | AbProject
  | AbApply
  -- Assorted abilities.
  | AbHurtMelee    -- ^ percentage damage bonus in melee
  | AbArmorMelee   -- ^ percentage armor bonus against melee
  | AbArmorRanged  -- ^ percentage armor bonus against ranged
  | AbMaxHP        -- ^ maximal hp
  | AbMaxCalm      -- ^ maximal calm
  | AbSpeed        -- ^ speed in m/10s (not when pushed or pulled)
  | AbSight        -- ^ FOV radius, where 1 means a single tile FOV
  | AbSmell        -- ^ smell radius
  | AbShine        -- ^ shine radius
  | AbNocto        -- ^ noctovision radius
  | AbAggression   -- ^ aggression, e.g., when closing in for melee
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | Skill level in particular abilities.
--
-- This representation is sparse, so better than a record when there are more
-- item kinds (with few abilities) than actors (with many abilities),
-- especially if the number of abilities grows as the engine is developed.
-- It's also easier to code and maintain.
--
-- The tree is by construction sparse, so equality is semantical.
newtype Skills = Skills {skills :: EM.EnumMap Ability Int}
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Item features.
data Feature =
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

newtype Flags = Flags {flags :: ES.EnumSet Feature}
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

instance NFData Ability

instance NFData Feature

instance NFData EqpSlot

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary Feature where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary EqpSlot where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Ability

instance Hashable Feature

instance Hashable EqpSlot

getAb :: Ability -> Skills -> Int
{-# INLINE getAb #-}
getAb ab (Skills sk) = EM.findWithDefault 0 ab sk

addAb :: Ability -> Int -> Skills -> Skills
addAb ab n sk = addSkills (Skills $ EM.singleton ab n) sk

checkFl :: Feature -> Flags -> Bool
{-# INLINE checkFl #-}
checkFl flag (Flags flags) = flag `ES.member` flags

skillsToList :: Skills -> [(Ability, Int)]
skillsToList (Skills sk) = EM.assocs sk

zeroSkills :: Skills
zeroSkills = Skills EM.empty

unitSkills :: Skills
unitSkills =
  Skills $ EM.fromDistinctAscList $ zip [AbMove .. AbApply] (repeat 1)

compactSkills :: EM.EnumMap Ability Int -> EM.EnumMap Ability Int
compactSkills = EM.filter (/= 0)

addSkills :: Skills -> Skills -> Skills
addSkills (Skills sk1) (Skills sk2) =
  Skills $ compactSkills $ EM.unionWith (+) sk1 sk2

scaleSkills :: Int -> EM.EnumMap Ability Int -> EM.EnumMap Ability Int
scaleSkills n sk = EM.map (n *) sk

sumScaledAbility :: [(Skills, Int)] -> Skills
sumScaledAbility l = Skills $ compactSkills $ EM.unionsWith (+)
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
                  $ zip [AbMove .. AbApply] (repeat (-10))

blockOnly = Skills $ EM.delete AbWait $ skills minusTen

meleeAdjacent = Skills $ EM.delete AbMelee $ skills blockOnly

-- Melee and reaction fire.
meleeAndRanged = Skills $ EM.delete AbProject $ skills meleeAdjacent

ignoreItems = Skills $ EM.fromList
                     $ zip [AbMoveItem, AbProject, AbApply] (repeat (-10))
