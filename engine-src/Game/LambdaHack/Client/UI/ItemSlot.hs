{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar, natSlots, oddSlot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary

newtype SlotChar = SlotChar Int
  deriving (Show, Eq, Ord, Binary, Enum)

natSlots :: [SlotChar]
{-# INLINE natSlots #-}
natSlots = [SlotChar 0 ..]

oddSlot :: SlotChar
oddSlot = SlotChar (-1)
