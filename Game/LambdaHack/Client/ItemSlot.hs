-- | Item slots for UI and AI item collections.
-- TODO: document
module Game.LambdaHack.Client.ItemSlot
  ( ItemSlots, SlotChar(..)
  , allSlots, allZeroSlots, slotLabel, slotRange, assignSlot
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (shiftL, shiftR)
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.State

data SlotChar = SlotChar {slotPrefix :: Int, slotChar :: Char}
  deriving (Show, Eq)

instance Ord SlotChar where
  compare = comparing fromEnum

instance Binary SlotChar where
  put = put . fromEnum
  get = fmap toEnum get

instance Enum SlotChar where
  fromEnum (SlotChar n c) =
    ord c + (if isUpper c then 100 else 0) + shiftL n 8
  toEnum e =
    let n = shiftR e 8
        c0 = e - shiftL n 8
        c100 = c0 - if c0 > 150 then 100 else 0
    in SlotChar n (chr c100)

type ItemSlots = ( EM.EnumMap SlotChar ItemId
                 , EM.EnumMap SlotChar ItemId )

slotRange :: [SlotChar] -> Text
slotRange ls =
  sectionBy (sort ls) Nothing
 where
  succSlot c d = ord (slotChar d) - ord (slotChar c) == 1
  succ2Slot c d = ord (slotChar d) - ord (slotChar c) == 2

  sectionBy []     Nothing       = T.empty
  sectionBy []     (Just (c, d)) = finish (c,d)
  sectionBy (x:xs) Nothing       = sectionBy xs (Just (x, x))
  sectionBy (x:xs) (Just (c, d))
    | succSlot d x               = sectionBy xs (Just (c, x))
    | otherwise                  = finish (c,d) <> sectionBy xs (Just (x, x))

  finish (c, d) | c == d         = T.pack [slotChar c]
                | succSlot c d   = T.pack [slotChar c, slotChar d]
                | succ2Slot c d  = T.pack [ slotChar c
                                          , chr (1 + ord (slotChar c))
                                          , slotChar d ]
                | otherwise      = T.pack [slotChar c, '-', slotChar d]

allSlots :: Int -> [SlotChar]
allSlots n = map (SlotChar n) $ ['a'..'z'] ++ ['A'..'Z']

allZeroSlots :: [SlotChar]
allZeroSlots = allSlots 0

-- | Assigns a slot to an item, for inclusion in the inventory or equipment
-- of a hero. Tries to to use the requested slot, if any.
assignSlot :: CStore -> Item -> FactionId -> Maybe Actor -> ItemSlots
           -> SlotChar -> State
           -> SlotChar
assignSlot store item fid mbody (itemSlots, organSlots) lastSlot s =
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
                   (\b -> getCBag (CFloor (blid b) (bpos b)) s)
                   mbody
  inBags = ES.unions $ map EM.keysSet $ onPerson : [ onGround | not onlyOrgans]
  lSlots = if onlyOrgans  then organSlots else itemSlots
  f l = maybe True (`ES.notMember` inBags) $ EM.lookup l lSlots
  free = filter f candidates
  g l = l `EM.notMember` lSlots
  fresh = filter g $ take ((slotPrefix lastSlot + 1) * len0) candidates

slotLabel :: SlotChar -> Text
slotLabel x =
  T.snoc (if slotPrefix x == 0 then T.empty else tshow $ slotPrefix x)
         (slotChar x)
  <> ")"
