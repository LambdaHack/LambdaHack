{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar(..), ItemSlots(..)
  , allSlots, intSlots, slotLabel, assignSlot, partyItemSet
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import           Data.Bits (unsafeShiftL, unsafeShiftR)
import           Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Ord (comparing)
import qualified Data.Text as T

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.State

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

-- | A collection of mappings from slot labels to item identifiers.
newtype ItemSlots = ItemSlots (EM.EnumMap SLore (EM.EnumMap SlotChar ItemId))
  deriving (Show, Binary)

allChars :: [Char]
allChars = ['a'..'z'] ++ ['A'..'Z']

allCharsLength :: Int
allCharsLength = length allChars

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
  head $ fresh ++ free
 where
  lSlots = itemSlots EM.! slore
  f l = maybe True (`ES.notMember` partySet) $ EM.lookup l lSlots
  free = filter f allSlots
  g l = l `EM.notMember` lSlots
  maxPrefix = case EM.maxViewWithKey lSlots of
    Just ((lm, _), _) -> slotPrefix lm
    Nothing -> 0
  -- Fill all empty slots up to half max prefix, then prefer lower prefixes
  -- (even if slot not empty) as long as the item not held by the party.
  fresh = filter g $ take ((maxPrefix `div` 2) * allCharsLength) allSlots

partyItemSet :: SLore -> FactionId -> Maybe Actor -> State -> ES.EnumSet ItemId
partyItemSet slore fid mbody s =
  let onPersons = combinedFromLore slore fid s
      onGround = maybe EM.empty  -- consider floor only under the acting actor
                   (\b -> getFloorBag (blid b) (bpos b) s)
                   mbody
  in ES.unions $ map EM.keysSet $ onPersons : [onGround | slore == SItem]
