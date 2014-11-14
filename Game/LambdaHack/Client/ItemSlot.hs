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
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.State

newtype SlotChar = SlotChar {slotChar :: Char}
  deriving (Show, Eq, Binary)

instance Ord SlotChar where
  compare x y = compare (fromEnum x) (fromEnum y)

instance Enum SlotChar where
  fromEnum (SlotChar x) = fromEnum x + if isUpper x then 1000 else 0
  toEnum e = SlotChar $ toEnum $ e - (if e > 1000 then 1000 else 0)

type ItemSlots = (EM.EnumMap SlotChar ItemId, IM.IntMap ItemId)

slotRange :: [SlotChar] -> Text
slotRange ls =
  sectionBy (sort ls) Nothing
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
assignSlot :: Item -> FactionId -> Maybe Actor -> ItemSlots -> SlotChar
           -> State
           -> Either SlotChar Int
assignSlot item fid mbody (letterSlots, numberSlots) lastSlot s =
  if jsymbol item == '$'
  then Left $ SlotChar '$'
  else case free of
    freeChar : _ -> Left freeChar
    [] -> Right $ head freeNumbers
 where
  candidates = take (length allSlots)
               $ drop (1 + fromJust (elemIndex lastSlot allSlots))
               $ cycle allSlots
  onPerson = maybe (sharedAllOwnedFid True fid s)
                   (\body -> sharedAllOwned True body s)
                   mbody
  onGroud = maybe EM.empty
                  (\b -> getCBag (CFloor (blid b) (bpos b)) s)
                  mbody
  inBags = ES.unions $ map EM.keysSet [onPerson, onGroud]
  f l = maybe True (`ES.notMember` inBags) $ EM.lookup l letterSlots
  free = filter f candidates
  g l = maybe True (`ES.notMember` inBags) $ IM.lookup l numberSlots
  freeNumbers = filter g [0..]

slotLabel :: Either SlotChar Int -> MU.Part
slotLabel (Left c) = MU.String [slotChar c]
slotLabel Right{} = "0"
