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
  | ToThrow !(Effect.ThrowMod Int)  -- ^ parameters modifying a throw
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
    Consumable -> ""
    Light p -> Effect.affixBonus p
