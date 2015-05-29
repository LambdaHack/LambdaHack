{-# LANGUAGE DeriveGeneric #-}
-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..), Skills
  , zeroSkills, unitSkills, addSkills, scaleSkills
  , blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Actor and faction abilities corresponding to client-server requests.
data Ability =
    AbMove
  | AbMelee
  | AbDisplace
  | AbAlter
  | AbWait
  | AbMoveItem
  | AbProject
  | AbApply
  | AbTrigger
  deriving (Read, Eq, Ord, Generic, Enum, Bounded)

-- skill level in particular abilities.
type Skills = EM.EnumMap Ability Int

zeroSkills :: Skills
zeroSkills = EM.empty

unitSkills :: Skills
unitSkills = EM.fromDistinctAscList $ zip [minBound..maxBound] (repeat 1)

addSkills :: Skills -> Skills -> Skills
addSkills = EM.unionWith (+)

scaleSkills :: Int -> Skills -> Skills
scaleSkills n = EM.map (n *)

minusTen, blockOnly, meleeAdjacent, meleeAndRanged, ignoreItems :: Skills

-- To make sure only a lot of weak items can override move-only-leader, etc.
minusTen = EM.fromList $ zip [minBound..maxBound] [-10, -10..]

blockOnly = EM.delete AbWait minusTen

meleeAdjacent = EM.delete AbMelee blockOnly

-- Melee and reaction fire.
meleeAndRanged = EM.delete AbProject meleeAdjacent

ignoreItems = EM.fromList $ zip [AbMoveItem, AbProject, AbApply] [-10, -10..]

instance Show Ability where
  show AbMove = "move"
  show AbMelee = "melee"
  show AbDisplace = "displace"
  show AbAlter = "alter tile"
  show AbWait = "wait"
  show AbMoveItem = "manage items"
  show AbProject = "fling"
  show AbApply = "apply"
  show AbTrigger = "trigger floor"

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Ability
