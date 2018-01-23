{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type and operations
    ItemId, Item(..), ItemIdentity(..)
  , isMelee, isTmpCondition, isBlast
  , goesIntoEqp, goesIntoInv, goesIntoSha
    -- * Item discovery types and operations
  , ItemKindIx, ItemDisco(..), ItemFull(..)
  , DiscoveryKind, DiscoveryAspect, ItemIxMap, Benefit(..), DiscoveryBenefit
  , itemNoDisco, itemToFull7, aspectRecordFull
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
  { jkind    :: ItemIdentity  -- ^ the kind of the item, or an indiretion
  , jlid     :: LevelId       -- ^ lowest level the item was created at
  , jfid     :: Maybe FactionId
                              -- ^ the faction that created the item, if any
  , jflavour :: Flavour       -- ^ flavour, always the real one, not hidden;
                              --   people may not recognize shape, but they
                              --   remember colour and old vs fancy look
  }
  deriving (Show, Eq, Generic)

instance Hashable Item

instance Binary Item

-- | Either the explicit obvious kind of the item or the kind it's hidden under,
-- with the details covered under the index indirection.
data ItemIdentity =
    IdentityObvious (ContentId IK.ItemKind)
  | IdentityCovered ItemKindIx (ContentId IK.ItemKind)
  deriving (Show, Eq, Generic)

instance Hashable ItemIdentity

instance Binary ItemIdentity

isMelee :: ItemFull -> Bool
isMelee itemFull = IK.Meleeable `elem` IK.ifeature (itemKind itemFull)

isTmpCondition :: ItemFull -> Bool
isTmpCondition itemFull = IK.Fragile `elem` IK.ifeature (itemKind itemFull)
                          && IK.Durable `elem` IK.ifeature (itemKind itemFull)

isBlast :: ItemFull -> Bool
isBlast itemFull = IK.Blast `elem` IK.ifeature (itemKind itemFull)

goesIntoEqp :: ItemFull -> Bool
goesIntoEqp itemFull = IK.Equipable `elem` IK.ifeature (itemKind itemFull)
                       || IK.Meleeable `elem` IK.ifeature (itemKind itemFull)

goesIntoInv :: ItemFull -> Bool
goesIntoInv itemFull = IK.Precious `notElem` IK.ifeature (itemKind itemFull)
                       && not (goesIntoEqp itemFull)

goesIntoSha :: ItemFull -> Bool
goesIntoSha itemFull = IK.Precious `elem` IK.ifeature (itemKind itemFull)
                       && not (goesIntoEqp itemFull)

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
--
-- The @itemAspect@ accessor it to be used unconditionally only on the server
-- where it's guaranteed to be safe.
data ItemDisco =
    ItemDiscoMean IA.KindMean
  | ItemDiscoFull {itemAspect :: IA.AspectRecord}
 deriving Show

-- No speedup from making fields non-strict.
-- | Full information about an item.
data ItemFull = ItemFull
  { itemBase    :: Item
  , itemK       :: Int
  , itemTimer   :: ItemTimer
  , itemKindId  :: ContentId IK.ItemKind
  , itemKind    :: IK.ItemKind
  , itemDisco   :: ItemDisco
  , itemSuspect :: Bool
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

itemNoDisco :: COps -> (Item, Int) -> ItemFull
itemNoDisco COps{coitem, coItemSpeedup} (itemBase, itemK) =
  let itemKindId = case jkind itemBase of
        IdentityObvious ik -> ik
        IdentityCovered _ ik -> ik
      itemKind = okind coitem itemKindId
      itemAspectMean = IK.getKindMean itemKindId coItemSpeedup
      itemDisco = ItemDiscoMean itemAspectMean
      itemSuspect = True
  in ItemFull {itemTimer = [], ..}

itemToFull7 :: COps -> DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
            -> ItemQuant
            -> ItemFull
itemToFull7 COps{coitem, coItemSpeedup}
            discoKind discoAspect iid itemBase (itemK, itemTimer) =
  let (itemKindId, itemSuspect) = case jkind itemBase of
        IdentityObvious ik -> (ik, False)
        IdentityCovered ix ik ->
          maybe (ik, True) (\ki -> (ki, False)) $ ix `EM.lookup` discoKind
      itemKind = okind coitem itemKindId
      km = IK.getKindMean itemKindId coItemSpeedup
      -- If the kind is not identified, we know nothing about the real
      -- aspects, so we at least assume they are variable.
      itemAspectMean | itemSuspect = km {IA.kmConst = False}
                     | otherwise = km
      itemDisco = case EM.lookup iid discoAspect of
        Just itemAspect -> ItemDiscoFull itemAspect
        Nothing -> ItemDiscoMean itemAspectMean
  in ItemFull {..}

aspectRecordFull :: ItemFull -> IA.AspectRecord
aspectRecordFull itemFull =
  case itemDisco itemFull of
    ItemDiscoMean itemAspectMean -> IA.kmMean itemAspectMean
    ItemDiscoFull itemAspect -> itemAspect

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
strengthEffect f itemFull = concatMap f $ IK.ieffects $ itemKind itemFull

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

strengthToThrow :: ItemFull -> IK.ThrowMod
strengthToThrow itemFull =
  let p (IK.ToThrow tmod) = [tmod]
      p _ = []
  in case concatMap p (IK.ifeature $ itemKind itemFull) of
    [] -> IK.ThrowMod 100 100
    [x] -> x
    xs -> error $ "" `showFailure` (xs, itemFull)

-- This ignores items that don't go into equipment, as determined in @inEqp@.
-- They are removed from equipment elsewhere via @harmful@.
strongestSlot :: DiscoveryBenefit -> IA.EqpSlot -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot discoBenefit eqpSlot is =
  let f (iid, itemFull) =
        let rawDmg = damageUsefulness itemFull
            (bInEqp, bPickup) = case EM.lookup iid discoBenefit of
               Just Benefit{benInEqp, benPickup} -> (benInEqp, benPickup)
               Nothing -> (goesIntoEqp itemFull, rawDmg)
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

itemTrajectory :: ItemFull -> [Point] -> ([Vector], (Speed, Int))
itemTrajectory itemFull@ItemFull{itemKind} path =
  let IK.ThrowMod{..} = strengthToThrow itemFull
  in computeTrajectory (IK.iweight itemKind) throwVelocity throwLinger path

totalRange :: ItemFull -> Int
totalRange itemFull = snd $ snd $ itemTrajectory itemFull []

hasCharge :: Time -> ItemFull -> Bool
hasCharge localTime itemFull@ItemFull{..} =
  let timeout = IA.aTimeout $ aspectRecordFull itemFull
      timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
      charging startT = timeShift startT timeoutTurns > localTime
      it1 = filter charging itemTimer
  in length it1 < itemK

damageUsefulness :: ItemFull -> Double
damageUsefulness ItemFull{itemKind} =
  let v = min 1000 (10 * Dice.meanDice (IK.idamage itemKind))
  in assert (v >= 0) v

strongestMelee :: Maybe DiscoveryBenefit -> Time -> [(ItemId, ItemFull)]
               -> [(Double, (ItemId, ItemFull))]
strongestMelee _ _ [] = []
strongestMelee mdiscoBenefit localTime is =
  -- For simplicity we assume, if weapon not recharged, all important effects,
  -- good and bad, are disabled and only raw damage remains.
  let f (iid, itemFull) =
        let rawDmg = (damageUsefulness itemFull, (iid, itemFull))
            knownOrConstantAspects = case itemDisco itemFull of
              ItemDiscoMean IA.KindMean{kmConst} -> kmConst
              ItemDiscoFull{} -> True
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
unknownAspect f ItemFull{itemKind=IK.ItemKind{iaspects}, ..} =
  case itemDisco of
    ItemDiscoMean IA.KindMean{kmConst} ->
      let unknown x = let (minD, maxD) = Dice.minmaxDice x
                      in minD /= maxD
      in itemSuspect || not kmConst && or (concatMap (map unknown . f) iaspects)
    ItemDiscoFull{} -> False  -- all known

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
  in foldr f 0 $ filter isTmpCondition is
