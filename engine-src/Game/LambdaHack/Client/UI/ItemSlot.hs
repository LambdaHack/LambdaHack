{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar, ItemRoles(..), SingleItemRoles
  , natSlots, oddSlot, assignSlot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Types
import Game.LambdaHack.Definition.Defs

-- | Slot label. Usually just a character. Sometimes with a numerical prefix.
newtype SlotChar = SlotChar Int
  deriving (Show, Eq, Ord, Binary, Enum)

type SingleItemRoles = EM.EnumMap SlotChar ItemId

-- | A collection of mappings from slot labels to item identifiers.
newtype ItemRoles = ItemRoles (EM.EnumMap SLore SingleItemRoles)
  deriving (Show, Binary)

natSlots :: [SlotChar]
{-# INLINE natSlots #-}
natSlots = [SlotChar 0 ..]

oddSlot :: SlotChar
oddSlot = SlotChar (-1)

-- | Assigns a slot to an item, e.g., for inclusion in equipment of a hero.
-- At first, e.g., when item is spotted on the floor, the slot is
-- not user-friendly. After any player's item manipulation action,
-- slots are sorted and a fully human-readable slot is then assigned.
-- Only then the slot can be viewed by the player.
assignSlot :: SingleItemRoles -> SlotChar
assignSlot lSlots =
  let maxPrefix = case EM.maxViewWithKey lSlots of
        Just ((SlotChar n, _), _) -> n
        Nothing -> 0
  in SlotChar $ maxPrefix + 1
