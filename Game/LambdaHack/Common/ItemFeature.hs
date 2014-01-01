{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Effect

-- | All possible item features.
data Feature =
    Cause !(Effect Int)  -- ^ causes the effect when triggered
  | ChangeTo !Text       -- ^ changes to this item kind group when altered
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Feature
