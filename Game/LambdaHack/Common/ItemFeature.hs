{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Random

-- | All possible item features.
data Feature =
    Cause !(Effect RollDeep)  -- ^ causes the effect when triggered
  | ChangeTo !Text            -- ^ changes to this item kind group when altered
  deriving (Show, Eq, Ord, Generic)

instance Binary Feature
