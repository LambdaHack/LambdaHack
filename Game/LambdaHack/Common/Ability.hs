{-# LANGUAGE DeriveGeneric #-}
-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..)
  ) where

import Data.Binary
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
