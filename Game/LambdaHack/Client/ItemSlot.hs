{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI item collections.
-- TODO: document
module Game.LambdaHack.Client.ItemSlot
  ( ItemSlots, SlotChar(..)
  , allSlots, slotLabel, slotRange, assignSlot
  ) where

import Data.Binary
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State

newtype SlotChar = SlotChar {slotChar :: Char}
  deriving (Show, Eq, Enum, Binary)

instance Ord SlotChar where
  compare (SlotChar x) (SlotChar y) =
    compare (isUpper x, toLower x) (isUpper y, toLower y)

type ItemSlots = EM.EnumMap SlotChar ItemId

cmpSlot :: SlotChar -> SlotChar -> Ordering
cmpSlot (SlotChar x) (SlotChar y) =
  compare (isUpper x, toLower x) (isUpper y, toLower y)

slotRange :: [SlotChar] -> Text
slotRange ls =
  sectionBy (sortBy cmpSlot ls) Nothing
 where
  succSlot c d = ord (slotChar d) - ord (slotChar c) == 1

  sectionBy []     Nothing       = T.empty
  sectionBy []     (Just (c, d)) = finish (c,d)
  sectionBy (x:xs) Nothing       = sectionBy xs (Just (x, x))
  sectionBy (x:xs) (Just (c, d))
    | succSlot d x               = sectionBy xs (Just (c, x))
    | otherwise                  = finish (c,d) <> sectionBy xs (Just (x, x))

  finish (c, d) | c == d         = T.pack [slotChar c]
                | succSlot c d   = T.pack [slotChar c, slotChar d]
                | otherwise      = T.pack [slotChar c, '-', slotChar d]

allSlots :: [SlotChar]
allSlots = map SlotChar $ ['a'..'z'] ++ ['A'..'Z']

-- | Assigns a slot to an item, for inclusion in the inventory or equipment
-- of a hero. Tries to to use the requested slot, if any.
assignSlot :: Item -> Actor -> ItemSlots -> SlotChar
           -> State
           -> Maybe SlotChar
assignSlot item body slots lastSlot s =
  if jsymbol item == '$' && dollarChar `elem` allowed
  then Just dollarChar
  else listToMaybe free
 where
  candidates = take (length allSlots)
               $ drop (1 + fromJust (elemIndex lastSlot allSlots))
               $ cycle allSlots
  onPerson = sharedAllOwned body s
  onGroud = sdungeon s EM.! blid body `atI` bpos body
  inBags = ES.unions $ map EM.keysSet [onPerson, onGroud]
  f l = maybe True (`ES.notMember` inBags) $ EM.lookup l slots
  free = filter f candidates
  allowed = dollarChar : free
  dollarChar = SlotChar '$'

slotLabel :: SlotChar -> MU.Part
slotLabel c = MU.Text $ T.pack $ slotChar c : " -"
