{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..), featureToSuff
  ) where

import Data.Binary
import qualified Data.Hashable as Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Effect as Effect

-- | All possible item features.
data Feature =
    ChangeTo !Text             -- ^ change to this item kind group when altered
  | Fragile                    -- ^ break even when not hitting an enemy
  | ToThrow !Int               -- ^ percentage bonus to throw speed
  | Linger !Int                -- ^ fly for this percentage of 2 turns
  | Consumable                 -- ^ can't be turned off, is consumed by use
  | Light !Int                 -- ^ item shines with the given radius
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Feature

instance Binary Feature

featureToSuff :: Feature -> Text
featureToSuff feat =
  case feat of
    ChangeTo{} -> ""
    Fragile -> ""
    ToThrow{} -> ""
    Linger{} -> ""
    Consumable -> ""
    Light p -> Effect.affixPower p
