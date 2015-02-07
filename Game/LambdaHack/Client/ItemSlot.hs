{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
-- TODO: document
module Game.LambdaHack.Client.ItemSlot
  ( ItemSlots, SlotChar(..)
  , allSlots, slotLabel, slotRange, assignSlot
  ) where

import Data.Binary
import Data.Bits (shiftL, shiftR)
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

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

slotRange :: Int -> [SlotChar] -> Text
slotRange n ls =
  sectionBy (sort $ filter hasPrefix ls) Nothing
 where
  hasPrefix x = slotPrefix x == n
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
assignSlot c item fid mbody (itemSlots, organSlots) lastSlot s =
  if jsymbol item == '$'
  then SlotChar 0 '$'
  else head free
 where
  candidatesZero = take (length allZeroSlots)
                   $ drop (1 + fromMaybe 0 (elemIndex lastSlot allZeroSlots))
                   $ cycle allZeroSlots
  candidates = candidatesZero ++ concat [allSlots n | n <- [1..]]
  onPerson = maybe (sharedAllOwnedFid True fid s)
                   (\body -> sharedAllOwned True body s)
                   mbody
  onGroud = maybe EM.empty
                  (\b -> getCBag (CFloor (blid b) (bpos b)) s)
                  mbody
  inBags = ES.unions $ map EM.keysSet [onPerson, onGroud]
  lSlots = case c of
    COrgan -> organSlots
    _ -> itemSlots
  f l = maybe True (`ES.notMember` inBags) $ EM.lookup l lSlots
  free = filter f candidates

slotLabel :: SlotChar -> MU.Part
slotLabel x = MU.String
              $ (if slotPrefix x == 0 then [] else show $ slotPrefix x)
                ++ [slotChar x]
