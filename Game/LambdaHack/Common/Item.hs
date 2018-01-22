{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type and operations
    ItemId, Item(..)
  , itemPrice, isMelee, isTmpCondition, isBlast
  , goesIntoEqp, goesIntoInv, goesIntoSha
    -- * Item discovery types and operations
  , ItemKindIx, ItemDisco(..), ItemFull(..)
  , DiscoveryKind, DiscoveryAspect, ItemIxMap, Benefit(..), DiscoveryBenefit
  , itemNoDisco, itemToFull6, aspectRecordFull
    -- * Inventory management types
  , ItemTimer, ItemQuant, ItemBag, ItemDict
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import qualified Data.Ix as Ix
import           GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | Game items in actor possesion or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: ItemKindIx    -- ^ index pointing to the kind of the item
  , jlid     :: LevelId       -- ^ lowest level the item was created at
  , jfid     :: Maybe FactionId
                              -- ^ the faction that created the item, if any
  , jsymbol  :: Char          -- ^ map symbol
  , jname    :: Text          -- ^ generic name
  , jflavour :: Flavour       -- ^ flavour
  , jfeature :: [IK.Feature]  -- ^ public properties
  , jweight  :: Int           -- ^ weight in grams, obvious enough
  , jdamage  :: Dice.Dice     -- ^ impact damage of this particular weapon
  }
  deriving (Show, Eq, Generic)

instance Hashable Item

instance Binary Item

-- | Price an item, taking count into consideration.
itemPrice :: (Item, Int) -> Int
itemPrice (item, jcount) =
  case jsymbol item of
    _ | jname item == "gem" -> jcount * 100  -- hack
    '$' -> jcount
    '*' -> jcount * 100
    _   -> 0

isMelee :: Item -> Bool
isMelee item = IK.Meleeable `elem` jfeature item

isTmpCondition :: Item -> Bool
isTmpCondition item = IK.Fragile `elem` jfeature item
                      && IK.Durable `elem` jfeature item

isBlast :: Item -> Bool
isBlast item = IK.Blast `elem` jfeature item

goesIntoEqp :: Item -> Bool
goesIntoEqp item = IK.Equipable `elem` jfeature item
                   || IK.Meleeable `elem` jfeature item

goesIntoInv :: Item -> Bool
goesIntoInv item = IK.Precious `notElem` jfeature item
                   && not (goesIntoEqp item)

goesIntoSha :: Item -> Bool
goesIntoSha item = IK.Precious `elem` jfeature item
                   && not (goesIntoEqp item)

-- | The map of item ids to item aspects.
-- The full map is known by the server.
type DiscoveryAspect = EM.EnumMap ItemId IA.AspectRecord

-- | An index of the kind identifier of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
-- The indexes and kind identifiers are 1-1.
newtype ItemKindIx = ItemKindIx Int
  deriving (Show, Eq, Ord, Enum, Ix.Ix, Hashable, Binary)

-- | The secret part of the information about an item. If a faction
-- knows the aspects of the item (the @kmConst@ flag is set or
-- the @itemAspect@ field is @Left@), this is a complete secret information.
-- Items that don't need second identification may be identified or not and both
-- cases are OK (their display flavour will differ and that may be the point).
data ItemDisco = ItemDisco
  { itemKindId :: ContentId IK.ItemKind
  , itemKind   :: IK.ItemKind
  , itemAspect :: Either IA.AspectRecord IA.KindMean
  }
  deriving Show

-- No speedup from making fields non-strict.
-- | Full information about an item.
data ItemFull = ItemFull
  { itemBase  :: Item
  , itemK     :: Int
  , itemTimer :: ItemTimer
  , itemDisco :: Maybe ItemDisco
  }
  deriving Show

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is 1-1.
type DiscoveryKind = EM.EnumMap ItemKindIx (ContentId IK.ItemKind)

-- | The map of item kind indexes to identifiers of items that have that kind.
-- Used to update data about items when their kinds become known, e.g.,
-- AI item use benefit data.
type ItemIxMap = EM.EnumMap ItemKindIx (ES.EnumSet ItemId)

-- | Fields are intentionally kept non-strict, because they are recomputed
-- often, but not used every time. The fields are, in order:
-- 1. whether the item should be kept in equipment (not in pack nor stash)
-- 2. the total benefit from picking the item up (to use or to put in equipment)
-- 3. the benefit of applying the item to self
-- 4. the (usually negative) benefit of hitting a foe in meleeing with the item
-- 5. the (usually negative) benefit of flinging an item at an opponent
data Benefit = Benefit
  { benInEqp  :: ~Bool
  , benPickup :: ~Double
  , benApply  :: ~Double
  , benMelee  :: ~Double
  , benFling  :: ~Double
  }
  deriving (Show, Generic)

instance Binary Benefit

type DiscoveryBenefit = EM.EnumMap ItemId Benefit

itemNoDisco :: (Item, Int) -> ItemFull
itemNoDisco (itemBase, itemK) =
  ItemFull {itemBase, itemK, itemTimer = [], itemDisco=Nothing}

itemToFull6 :: COps -> DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
            -> ItemQuant
            -> ItemFull
itemToFull6 COps{coitem, coItemSpeedup}
            discoKind discoAspect iid itemBase (itemK, itemTimer) =
  let itemDisco = case EM.lookup (jkindIx itemBase) discoKind of
        Nothing -> Nothing
        Just itemKindId ->
            let km = IK.getKindMean itemKindId coItemSpeedup
                itemAspect = case EM.lookup iid discoAspect of
                  Just ia -> Left ia
                  Nothing -> Right km
            in Just ItemDisco{ itemKindId
                             , itemKind = okind coitem itemKindId
                             , itemAspect }
  in ItemFull {..}

aspectRecordFull :: ItemFull -> IA.AspectRecord
aspectRecordFull itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAspect} -> either id IA.kmMean itemAspect
    Nothing -> IA.emptyAspectRecord

type ItemTimer = [Time]

-- | Number of items in a bag, together with recharging timer, in case of
-- items that need recharging, exists only temporarily or auto-activate
-- at regular intervals.
type ItemQuant = (Int, ItemTimer)

-- | A bag of items, e.g., one of the stores of an actor or the items
-- on a particular floor position or embedded in a particular map tile.
type ItemBag = EM.EnumMap ItemId ItemQuant

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item
