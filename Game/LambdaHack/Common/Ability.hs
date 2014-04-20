-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..)
  ) where

import Data.Binary

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
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8
