{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..), EqpSlot(..), featureToSuff
  ) where

import Data.Binary
import qualified Data.Hashable as Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Effect as Effect

-- | All possible item features.
data Feature =
    ChangeTo !Text                  -- ^ change to this group when altered
  | Fragile                         -- ^ break even when not hitting an enemy
  | Durable                         -- ^ don't break even hitting or applying
  | ToThrow !(Effect.ThrowMod Int)  -- ^ parameters modifying a throw
  | Applicable                      -- ^ can't be turned off, is consumed by use
  | Light !Int                      -- ^ item shines with the given radius
  | EqpSlot !EqpSlot !Text          -- ^ the equipment slot the item fits into
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotArmorMelee
  | EqpSlotSightRadius
  | EqpSlotSmellRadius
  | EqpSlotLight
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Feature

instance Hashable.Hashable EqpSlot

instance Binary Feature

instance Binary EqpSlot

featureToSuff :: Feature -> Text
featureToSuff feat =
  case feat of
    ChangeTo{} -> ""
    Fragile -> ""
    Durable -> ""
    ToThrow{} -> ""
    Applicable -> ""
    Light p -> Effect.affixBonus p
    EqpSlot{} -> ""
