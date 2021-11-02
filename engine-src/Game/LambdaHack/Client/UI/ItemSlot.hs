{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar, ItemSlots(..), SingleItemSlots
  , natSlots, oddSlot, assignSlot, sortSlotMap
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Defs

-- | Slot label. Usually just a character. Sometimes with a numerical prefix.
newtype SlotChar = SlotChar Int
  deriving (Show, Eq, Ord, Binary, Enum)

type SingleItemSlots = EM.EnumMap SlotChar ItemId

-- | A collection of mappings from slot labels to item identifiers.
newtype ItemSlots = ItemSlots (EM.EnumMap SLore SingleItemSlots)
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
assignSlot :: SingleItemSlots -> SlotChar
assignSlot lSlots =
  let maxPrefix = case EM.maxViewWithKey lSlots of
        Just ((SlotChar n, _), _) -> n
        Nothing -> 0
  in SlotChar $ maxPrefix + 1

sortSlotMap :: (ItemId -> ItemFull) -> SingleItemSlots -> SingleItemSlots
sortSlotMap itemToF em =
  -- If appearance and aspects the same, keep the order from before sort.
  let kindAndAppearance iid =
        let ItemFull{itemBase=Item{..}, ..} = itemToF iid
        in ( not itemSuspect, itemKindId, itemDisco
           , IK.isymbol itemKind, IK.iname itemKind
           , jflavour, jfid )
      sortItemIds = sortOn kindAndAppearance
  in EM.fromDistinctAscList $ zip natSlots $ sortItemIds $ EM.elems em
