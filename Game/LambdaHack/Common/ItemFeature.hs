{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..), EqpSlot(..), featureToSuff
  ) where

import Data.Binary
import Data.Hashable (Hashable)
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
  | EqpSlot !EqpSlot !Text          -- ^ the slot, counts towards the eqp limit
  | Identified                      -- ^ any such item starts identified
  | Precious                        -- ^ precious; don't risk identifying by use
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotPeriodic
  | EqpSlotAddMaxHP
  | EqpSlotAddMaxCalm
  | EqpSlotAddSpeed
  | EqpSlotAbility
  | EqpSlotArmorMelee
  | EqpSlotSightRadius
  | EqpSlotSmellRadius
  | EqpSlotAddLight
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Generic)

instance Hashable Feature

instance Hashable EqpSlot

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
    EqpSlot{} -> ""
    Identified -> ""
    Precious -> ""
