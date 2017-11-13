-- | Item slots for UI and AI item collections.
module Game.LambdaHack.Client.UI.ItemSlot
  ( SlotChar(..), ItemSlots(..)
  , allSlots, allZeroSlots, intSlots, slotLabel, assignSlot
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

-- | Mapping from slot labels to item identifiers.
data ItemSlots = ItemSlots
  { itemSlots  :: EM.EnumMap SlotChar ItemId
  , organSlots :: EM.EnumMap SlotChar ItemId }
  deriving Show

instance Binary ItemSlots where
  put (ItemSlots is os) = put is >> put os
  get = ItemSlots <$> get <*> get

allSlots :: Int -> [SlotChar]
allSlots n = map (SlotChar n) $ ['a'..'z'] ++ ['A'..'Z']

allZeroSlots :: [SlotChar]
allZeroSlots = allSlots 0

intSlots :: [SlotChar]
intSlots = map (flip SlotChar 'a') [0..]

slotLabel :: SlotChar -> Text
slotLabel x =
  T.snoc (if slotPrefix x == 0 then T.empty else tshow $ slotPrefix x)
         (slotChar x)
  <> ")"

-- | Assigns a slot to an item, for inclusion in the inventory
-- of a hero. Tries to to use the requested slot, if any.
assignSlot :: CStore -> Item -> FactionId -> Maybe Actor -> ItemSlots
           -> SlotChar -> State
           -> SlotChar
assignSlot store item fid mbody (ItemSlots itemSlots organSlots) lastSlot s =
  assert (maybe True (\b -> bfid b == fid) mbody)
  $ if jsymbol item == '$'
    then SlotChar 0 '$'
    else head $ fresh ++ free
 where
  offset = maybe 0 (+1) (elemIndex lastSlot allZeroSlots)
  onlyOrgans = store == COrgan
  len0 = length allZeroSlots
  candidatesZero = take len0 $ drop offset $ cycle allZeroSlots
  candidates = candidatesZero ++ concat [allSlots n | n <- [1..]]
  onPerson = sharedAllOwnedFid onlyOrgans fid s
  onGround = maybe EM.empty  -- consider floor only under the acting actor
                   (\b -> getFloorBag (blid b) (bpos b) s)
                   mbody
  inBags = ES.unions $ map EM.keysSet $ onPerson : [ onGround | not onlyOrgans]
  lSlots = if onlyOrgans  then organSlots else itemSlots
  f l = maybe True (`ES.notMember` inBags) $ EM.lookup l lSlots
  free = filter f candidates
  g l = l `EM.notMember` lSlots
  fresh = filter g $ take ((slotPrefix lastSlot + 1) * len0) candidates
