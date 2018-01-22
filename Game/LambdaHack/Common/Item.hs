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
  , -- * Assorted operations
    strengthEffect, strengthOnSmash, strengthDropOrgan
  , strengthEqpSlot, strengthToThrow, strongestSlot
  , computeTrajectory, itemTrajectory, totalRange
  , hasCharge, damageUsefulness, strongestMelee
  , unknownMeleeBonus, tmpMeleeBonus
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , unknownAspect
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import qualified Data.Ix as Ix
import qualified Data.Ord as Ord
import           GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
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

strengthEffect :: (IK.Effect -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemKind=IK.ItemKind{ieffects}} ->
      concatMap f ieffects
    Nothing -> []

strengthOnSmash :: ItemFull -> [IK.Effect]
strengthOnSmash =
  let p (IK.OnSmash eff) = [eff]
      p _ = []
  in strengthEffect p

strengthDropOrgan :: ItemFull -> [GroupName IK.ItemKind]
strengthDropOrgan =
  let p (IK.DropItem _ _ COrgan grp) = [grp]
      p (IK.Recharging (IK.DropItem _ _ COrgan grp)) = [grp]
      p (IK.OneOf l) = concatMap p l
      p (IK.Composite l) = concatMap p l
      p _ = []
  in strengthEffect p

strengthEqpSlot :: ItemFull -> Maybe IA.EqpSlot
strengthEqpSlot item =
  let p (IK.EqpSlot eqpSlot) = [eqpSlot]
      p _ = []
  in case strengthEffect p item of
    [] -> Nothing
    [x] -> Just x
    xs -> error $ "" `showFailure` (xs, item)

strengthToThrow :: Item -> IK.ThrowMod
strengthToThrow item =
  let p (IK.ToThrow tmod) = [tmod]
      p _ = []
  in case concatMap p (jfeature item) of
    [] -> IK.ThrowMod 100 100
    [x] -> x
    xs -> error $ "" `showFailure` (xs, item)

-- This ignores items that don't go into equipment, as determined in @inEqp@.
-- They are removed from equipment elsewhere via @harmful@.
strongestSlot :: DiscoveryBenefit -> IA.EqpSlot -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot discoBenefit eqpSlot is =
  let f (iid, itemFull) =
        let rawDmg = damageUsefulness $ itemBase itemFull
            (bInEqp, bPickup) = case EM.lookup iid discoBenefit of
               Just Benefit{benInEqp, benPickup} -> (benInEqp, benPickup)
               Nothing -> (goesIntoEqp $ itemBase itemFull, rawDmg)
        in if not bInEqp
           then Nothing
           else Just $
             let ben = if eqpSlot == IA.EqpSlotWeapon
                       -- For equipping/unequipping a weapon we take into
                       -- account not only melee power, but also aspects, etc.
                       then ceiling bPickup
                       else IA.prEqpSlot eqpSlot $ aspectRecordFull itemFull
             in (ben, (iid, itemFull))
  in sortBy (flip $ Ord.comparing fst) $ mapMaybe f is

itemTrajectory :: Item -> [Point] -> ([Vector], (Speed, Int))
itemTrajectory item path =
  let IK.ThrowMod{..} = strengthToThrow item
  in computeTrajectory (jweight item) throwVelocity throwLinger path

totalRange :: Item -> Int
totalRange item = snd $ snd $ itemTrajectory item []

hasCharge :: Time -> ItemFull -> Bool
hasCharge localTime itemFull@ItemFull{..} =
  let timeout = IA.aTimeout $ aspectRecordFull itemFull
      timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
      charging startT = timeShift startT timeoutTurns > localTime
      it1 = filter charging itemTimer
  in length it1 < itemK

damageUsefulness :: Item -> Double
damageUsefulness item = let v = min 1000 (10 * Dice.meanDice (jdamage item))
                        in assert (v >= 0) v

strongestMelee :: Maybe DiscoveryBenefit -> Time -> [(ItemId, ItemFull)]
               -> [(Double, (ItemId, ItemFull))]
strongestMelee _ _ [] = []
strongestMelee mdiscoBenefit localTime is =
  -- For simplicity we assume, if weapon not recharged, all important effects,
  -- good and bad, are disabled and only raw damage remains.
  let f (iid, itemFull@ItemFull{itemBase}) =
        let rawDmg = (damageUsefulness itemBase, (iid, itemFull))
            knownOrConstantAspects = case itemDisco itemFull of
              Just ItemDisco{itemAspect} ->
                either (const True) IA.kmConst itemAspect
              Nothing -> False
            unIDedBonus | knownOrConstantAspects = 0
                        | otherwise = 1000  -- exceptionally strong weapon
        in case mdiscoBenefit of
          Just discoBenefit -> case EM.lookup iid discoBenefit of
            Just Benefit{benMelee} ->
              -- For fighting, as opposed to equipping, we value weapon
              -- only for its raw damage and harming effects.
              let dmg = if hasCharge localTime itemFull
                        then (- benMelee, (iid, itemFull))
                        else rawDmg
              in first (+ unIDedBonus) dmg
            Nothing -> first (+ 1000) rawDmg -- not even kind known
          Nothing -> rawDmg  -- not interested about ID
  -- We can't filter out weapons that are not harmful to victim
  -- (@benMelee >= 0), because actors use them if nothing else available,
  -- e.g., geysers, bees. This is intended and fun.
  in sortBy (flip $ Ord.comparing fst) $ map f is

unknownAspect :: (IA.Aspect -> [Dice.Dice]) -> ItemFull -> Bool
unknownAspect f itemFull =
  case itemDisco itemFull of
    Nothing -> True  -- not even kind is known, so assume aspect affects melee
    Just ItemDisco{ itemAspect=Right IA.KindMean{kmConst}
                  , itemKind=IK.ItemKind{iaspects} } ->
      let unknown x = let (minD, maxD) = Dice.minmaxDice x
                      in minD /= maxD
      in not kmConst && or (concatMap (map unknown . f) iaspects)
    Just{} -> False  -- all known

unknownMeleeBonus :: [ItemFull] -> Bool
unknownMeleeBonus =
  let p (IA.AddHurtMelee k) = [k]
      p _ = []
      f itemFull b = b || unknownAspect p itemFull
  in foldr f False

tmpMeleeBonus :: [ItemFull] -> Int
tmpMeleeBonus is =
  let f itemFull k =
        itemK itemFull * IA.aHurtMelee (aspectRecordFull itemFull) + k
  in foldr f 0 $ filter (isTmpCondition . itemBase) is
