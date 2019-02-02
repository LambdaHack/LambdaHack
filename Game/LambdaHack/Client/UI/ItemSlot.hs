{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar(..), ItemSlots(..), SingleItemSlots
  , allSlots, intSlots, slotLabel
  , assignSlot, partyItemSet, sortSlotMap, mergeItemSlots
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import           Data.Bits (unsafeShiftL, unsafeShiftR)
import           Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import           Data.Ord (comparing)
import qualified Data.Text as T

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Container
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Content.ItemKind as IK

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

-- | Assigns a slot to an item, e.g., for inclusion in the inventory of a hero.
assignSlot :: ES.EnumSet ItemId -> SLore -> ItemSlots -> SlotChar
assignSlot partySet slore (ItemSlots itemSlots) =
  head $ freeLowPrefix ++ free
 where
  lSlots = itemSlots EM.! slore
  maxPrefix = case EM.maxViewWithKey lSlots of
    Just ((lm, _), _) -> slotPrefix lm
    Nothing -> 0
  slotsUpTo k = concatMap (\n -> map (SlotChar n) allChars) [0..k]
  f l = maybe True (`ES.notMember` partySet) $ EM.lookup l lSlots
  free = filter f $ slotsUpTo (maxPrefix + 1)  -- suffices
  g l = l {slotPrefix = maxPrefix} `EM.notMember` lSlots
  freeLowPrefix = filter g free

partyItemSet :: SLore -> FactionId -> Maybe Actor -> State -> ES.EnumSet ItemId
partyItemSet slore fid mbody s =
  let onPersons = combinedFromLore slore fid s
      onGround = maybe EM.empty  -- consider floor only under the acting actor
                   (\b -> getFloorBag (blid b) (bpos b) s)
                   mbody
  in ES.unions $ map EM.keysSet $ onPersons : [onGround | slore == SItem]

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
