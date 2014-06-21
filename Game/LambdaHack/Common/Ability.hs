{-# LANGUAGE DeriveGeneric #-}
-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..), Skills, addSkills, unitSkills
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

addSkills :: Skills -> Skills -> Skills
addSkills = EM.unionWith (+)

unitSkills :: Skills
unitSkills = EM.fromDistinctAscList $ zip [minBound..maxBound] [1..]

instance Show Ability where
  show AbMove = "move"
  show AbMelee = "melee"
  show AbDisplace = "displace"
  show AbAlter = "alter tile"
  show AbWait = "wait"
  show AbMoveItem = "manage items"
  show AbProject = "fling"
  show AbApply = "activate"
  show AbTrigger = "trigger tile"

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Ability
