{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar(..), ItemSlots(..), SingleItemSlots
  , allSlots, intSlots, slotLabel
  , assignSlot, sortSlotMap, mergeItemSlots
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import           Data.Bits (unsafeShiftL, unsafeShiftR)
import           Data.Char
import qualified Data.EnumMap.Strict as EM
import           Data.Function
import qualified Data.Text as T

import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Defs

-- | Slot label. Usually just a character. Sometimes with a numerical prefix.
data SlotChar = SlotChar {slotPrefix :: Int, slotChar :: Char}
  deriving (Show, Eq)

instance Ord SlotChar where
  compare = comparing fromEnum

instance Binary SlotChar where
  put = put . fromEnum
  get = fmap toEnum get

instance Enum SlotChar where
  fromEnum (SlotChar n c) =
    unsafeShiftL n 8 + ord c + (if isUpper c then 100 else 0)
  toEnum e =
    let n = unsafeShiftR e 8
        c0 = e - unsafeShiftL n 8
        c100 = c0 - if c0 > 150 then 100 else 0
    in SlotChar n (chr c100)

type SingleItemSlots = EM.EnumMap SlotChar ItemId

-- | A collection of mappings from slot labels to item identifiers.
newtype ItemSlots = ItemSlots (EM.EnumMap SLore SingleItemSlots)
  deriving (Show, Binary)

allChars :: [Char]
allChars = ['a'..'z'] ++ ['A'..'Z']

allSlots :: [SlotChar]
allSlots = concatMap (\n -> map (SlotChar n) allChars) [0..]

intSlots :: [SlotChar]
intSlots = map (flip SlotChar 'a') [0..]

slotLabel :: SlotChar -> Text
slotLabel x =
  T.snoc (if slotPrefix x == 0 then T.empty else tshow $ slotPrefix x)
         (slotChar x)
  <> ")"

-- | Assigns a slot to an item, e.g., for inclusion in equipment of a hero.
-- At first, e.g., when item is spotted on the floor, the slot is
-- not user-friendly. After any player's item manipulation action,
-- slots are sorted and a fully human-readable slot is then assigned.
-- Only then the slot can be viewed by the player.
assignSlot :: SingleItemSlots -> SlotChar
assignSlot lSlots =
  let maxPrefix = case EM.maxViewWithKey lSlots of
        Just ((lm, _), _) -> slotPrefix lm
        Nothing -> 0
  in SlotChar (maxPrefix + 1) 'x'

-- If appearance and aspects the same, keep the order from before sort.
compareItemFull :: ItemFull -> ItemFull -> Ordering
compareItemFull itemFull1 itemFull2 =
  let kindAndAppearance ItemFull{itemBase=Item{..}, ..} =
        ( not itemSuspect, itemKindId, itemDisco
        , IK.isymbol itemKind, IK.iname itemKind
        , jflavour, jfid )
  in comparing kindAndAppearance itemFull1 itemFull2

sortSlotMap :: (ItemId -> ItemFull) -> SingleItemSlots -> SingleItemSlots
sortSlotMap itemToF em =
  let f iid = (iid, itemToF iid)
      sortItemIds l = map fst $ sortBy (compareItemFull `on` snd)
                      $ map f l
  in EM.fromDistinctAscList $ zip allSlots $ sortItemIds $ EM.elems em

mergeItemSlots :: (ItemId -> ItemFull) -> [SingleItemSlots] -> SingleItemSlots
mergeItemSlots itemToF ems =
  let renumberSlot n SlotChar{slotPrefix, slotChar} =
        SlotChar{slotPrefix = slotPrefix + n * 1000000, slotChar}
      renumberMap n = EM.mapKeys (renumberSlot n)
      rms = zipWith renumberMap [0..] ems
      em = EM.unionsWith (\_ _ -> error "mergeItemSlots: duplicate keys") rms
  in sortSlotMap itemToF em
