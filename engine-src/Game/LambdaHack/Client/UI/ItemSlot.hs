{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( MenuSlot, natSlots, oddSlot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary

newtype MenuSlot = MenuSlot Int
  deriving (Show, Eq, Ord, Binary, Enum)

natSlots :: [MenuSlot]
{-# INLINE natSlots #-}
natSlots = [MenuSlot 0 ..]

oddSlot :: MenuSlot
oddSlot = MenuSlot (-1)
