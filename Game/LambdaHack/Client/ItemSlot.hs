{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Item slots for UI and AI inventories.
-- TODO: document
module Game.LambdaHack.Client.ItemSlot
  ( ItemSlots, SlotChar(..)
  , allSlots, assignSlot, slotLabel, slotRange
  , actorContainer, actorContainerB
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.State

newtype SlotChar = SlotChar {slotChar :: Char}
  deriving (Show, Eq, Enum)

instance Ord SlotChar where
  compare (SlotChar x) (SlotChar y) =
    compare (isUpper x, toLower x) (isUpper y, toLower y)

instance Binary SlotChar where
  put (SlotChar x) = put x
  get = fmap SlotChar get

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

-- | Assigns a slot to an item, for inclusion in the inventory
-- of a hero. Tries to to use the requested slot, if any.
assignSlot :: ItemId -> Maybe SlotChar -> Actor -> ItemSlots -> SlotChar
           -> State
           -> Maybe SlotChar
assignSlot iid r body slots freeSlot s =
  case lookup iid $ map swap $ EM.assocs slots of
    Just l -> Just l
    Nothing -> case r of
      Just l | l `elem` allowed -> Just l
      _ -> listToMaybe free
 where
  candidates = take (length allSlots)
               $ drop (fromJust (elemIndex freeSlot allSlots))
               $ cycle allSlots
  inBag = EM.keysSet $ sharedAllOwned body s
  f l = maybe True (`ES.notMember` inBag) $ EM.lookup l slots
  free = filter f candidates
  allowed = SlotChar '$' : free

actorContainer :: ActorId -> ItemSlots -> ItemId -> SlotChar
actorContainer aid slotChars iid =
  case find ((== iid) . snd) $ EM.assocs slotChars of
    Just (l, _) -> l
    Nothing -> assert `failure` "item not in inventory"
                      `twith` (aid, slotChars, iid)

actorContainerB :: Actor -> ItemSlots -> SlotChar
                -> ItemId -> Item -> State
                -> Maybe SlotChar
actorContainerB body slots freeSlot iid item s =
  case find ((== iid) . snd) $ EM.assocs slots of
    Just (l, _) -> Just l
    Nothing ->
      let l = if jsymbol item == '$' then Just $ SlotChar '$' else Nothing
      in case assignSlot iid l body slots freeSlot s of
        Just l2 -> Just l2
        Nothing -> Nothing

slotLabel :: SlotChar -> MU.Part
slotLabel c = MU.Text $ T.pack $ slotChar c : " -"
