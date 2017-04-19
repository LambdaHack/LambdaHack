{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), ItemSource(..)
  , itemPrice, goesIntoEqp, isMelee, goesIntoInv, goesIntoSha
  , seedToAspect, meanAspect, aspectRecordToList
  , aspectRecordFull, aspectsRandom
    -- * Item discovery types
  , ItemKindIx, ItemSeed, KindMean(..), DiscoveryKind
  , Benefit(..), DiscoveryBenefit
  , AspectRecord(..), emptyAspectRecord, sumAspectRecord, DiscoveryAspect
  , ItemFull(..), ItemDisco(..)
  , itemNoDisco, itemToFull
    -- * Inventory management types
  , ItemTimer, ItemQuant, ItemBag, ItemDict
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Hashable (Hashable)
import qualified Data.Ix as Ix
import GHC.Generics (Generic)
import System.Random (mkStdGen)

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Dice (intToDice)
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | An index of the kind id of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
newtype ItemKindIx = ItemKindIx Int
  deriving (Show, Eq, Ord, Enum, Ix.Ix, Hashable, Binary)

data KindMean = KindMean
  { kmKind :: !(Kind.Id IK.ItemKind)
  , kmMean :: !AspectRecord
  }
  deriving (Show, Eq, Generic)

instance Binary KindMean

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is 1-1.
type DiscoveryKind = EM.EnumMap ItemKindIx KindMean

-- | Fields are intentionally kept non-strict, because they are recomputed
-- often, but not used every time. The fields are, in order:
-- 1. whether the item should be kept in equipment (not in pack nor stash)
-- 2. the total benefit from picking the item up (to use or to put in equipment)
-- 3. the benefit of applying the item to self
-- 4. the (usually negative) benefit of hitting a foe in meleeing with the item
-- 5. the (usually negative) benefit of flinging an item at an opponent
data Benefit = Benefit
  { benInEqp  :: Bool
  , benPickup :: Int
  , benApply  :: Int
  , benMelee  :: Int
  , benFling  :: Int
  }
  deriving (Show, Generic)

instance Binary Benefit

type DiscoveryBenefit = EM.EnumMap ItemId Benefit

-- | A seed for rolling aspects of an item
-- Clients have partial knowledge of how item ids map to the seeds.
-- They gain knowledge by identifying items.
newtype ItemSeed = ItemSeed Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

data AspectRecord = AspectRecord
  { aTimeout     :: !Int
  , aHurtMelee   :: !Int
  , aArmorMelee  :: !Int
  , aArmorRanged :: !Int
  , aMaxHP       :: !Int
  , aMaxCalm     :: !Int
  , aSpeed       :: !Int
  , aSight       :: !Int
  , aSmell       :: !Int
  , aShine       :: !Int
  , aNocto       :: !Int
  , aAggression  :: !Int
  , aSkills      :: !Ability.Skills
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary AspectRecord

instance Hashable AspectRecord

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout     = 0
  , aHurtMelee   = 0
  , aArmorMelee  = 0
  , aArmorRanged = 0
  , aMaxHP       = 0
  , aMaxCalm     = 0
  , aSpeed       = 0
  , aSight       = 0
  , aSmell       = 0
  , aShine       = 0
  , aNocto       = 0
  , aAggression  = 0
  , aSkills      = Ability.zeroSkills
  }

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout     = 0
  , aHurtMelee   = sum $ mapScale aHurtMelee l
  , aArmorMelee  = sum $ mapScale aArmorMelee l
  , aArmorRanged = sum $ mapScale aArmorRanged l
  , aMaxHP       = sum $ mapScale aMaxHP l
  , aMaxCalm     = sum $ mapScale aMaxCalm l
  , aSpeed       = sum $ mapScale aSpeed l
  , aSight       = sum $ mapScale aSight l
  , aSmell       = sum $ mapScale aSmell l
  , aShine       = sum $ mapScale aShine l
  , aNocto       = sum $ mapScale aNocto l
  , aAggression  = sum $ mapScale aAggression l
  , aSkills      = EM.unionsWith (+) $ mapScaleAbility l
  }
 where
  mapScale f = map (\(ar, k) -> f ar * k)
  mapScaleAbility = map (\(ar, k) -> Ability.scaleSkills k $ aSkills ar)

-- | The map of item ids to item aspects.
-- The full map is known by the server.
type DiscoveryAspect = EM.EnumMap ItemId AspectRecord

data ItemDisco = ItemDisco
  { itemKindId     :: !(Kind.Id IK.ItemKind)
  , itemKind       :: !IK.ItemKind
  , itemAspectMean :: !AspectRecord
  , itemAspect     :: !(Maybe AspectRecord)
  }
  deriving Show

data ItemFull = ItemFull
  { itemBase  :: !Item
  , itemK     :: !Int
  , itemTimer :: !ItemTimer
  , itemDisco :: !(Maybe ItemDisco)
  }
  deriving Show

itemNoDisco :: (Item, Int) -> ItemFull
itemNoDisco (itemBase, itemK) =
  ItemFull {itemBase, itemK, itemTimer = [], itemDisco=Nothing}

itemToFull :: Kind.COps -> DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
           -> ItemQuant
           -> ItemFull
itemToFull Kind.COps{coitem=Kind.Ops{okind}}
           disco discoAspect iid itemBase (itemK, itemTimer) =
  let itemDisco = case EM.lookup (jkindIx itemBase) disco of
        Nothing -> Nothing
        Just KindMean{..} -> Just ItemDisco{ itemKindId = kmKind
                                           , itemKind = okind kmKind
                                           , itemAspectMean = kmMean
                                           , itemAspect = EM.lookup iid discoAspect }
  in ItemFull {..}

-- | Game items in actor possesion or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx    -- ^ index pointing to the kind of the item
  , jlid     :: !LevelId       -- ^ lowest level the item was created at
  , jfid     :: !(Maybe FactionId)
                               -- ^ the faction that created the item, if any
  , jsymbol  :: !Char          -- ^ map symbol
  , jname    :: !Text          -- ^ generic name
  , jflavour :: !Flavour       -- ^ flavour
  , jfeature :: ![IK.Feature]  -- ^ public properties
  , jweight  :: !Int           -- ^ weight in grams, obvious enough
  , jdamage  :: !Dice.Dice     -- ^ impact damage of this particular weapon
  }
  deriving (Show, Eq, Generic)

instance Hashable Item

instance Binary Item

data ItemSource =
    ItemSourceLevel !LevelId
  | ItemSourceFaction !FactionId
  deriving (Show, Eq, Generic)

instance Hashable ItemSource

instance Binary ItemSource

-- | Price an item, taking count into consideration.
itemPrice :: (Item, Int) -> Int
itemPrice (item, jcount) =
  case jsymbol item of
    '$' -> jcount
    '*' -> jcount * 100
    _   -> 0

goesIntoEqp :: Item -> Bool
goesIntoEqp item = IK.Equipable `elem` jfeature item
                   || IK.Meleeable `elem` jfeature item

isMelee :: Item -> Bool
isMelee item = IK.Meleeable `elem` jfeature item

goesIntoInv :: Item -> Bool
goesIntoInv item = IK.Precious `notElem` jfeature item
                   && not (goesIntoEqp item)

goesIntoSha :: Item -> Bool
goesIntoSha item = IK.Precious `elem` jfeature item
                   && not (goesIntoEqp item)

aspectRecordToList :: AspectRecord -> [IK.Aspect]
aspectRecordToList AspectRecord{..} =
  [IK.Timeout $ intToDice aTimeout | aTimeout /= 0]
  ++ [IK.AddHurtMelee $ intToDice aHurtMelee | aHurtMelee /= 0]
  ++ [IK.AddArmorMelee $ intToDice aArmorMelee | aArmorMelee /= 0]
  ++ [IK.AddArmorRanged $ intToDice aArmorRanged | aArmorRanged /= 0]
  ++ [IK.AddMaxHP $ intToDice aMaxHP | aMaxHP /= 0]
  ++ [IK.AddMaxCalm $ intToDice aMaxCalm | aMaxCalm /= 0]
  ++ [IK.AddSpeed $ intToDice aSpeed | aSpeed /= 0]
  ++ [IK.AddSight $ intToDice aSight | aSight /= 0]
  ++ [IK.AddSmell $ intToDice aSmell | aSmell /= 0]
  ++ [IK.AddShine $ intToDice aShine | aShine /= 0]
  ++ [IK.AddNocto $ intToDice aNocto | aNocto /= 0]
  ++ [IK.AddAggression $ intToDice aAggression | aAggression /= 0]
  ++ [IK.AddAbility ab $ intToDice n | (ab, n) <- EM.assocs aSkills, n /= 0]

castAspect :: AbsDepth -> AbsDepth -> AspectRecord -> IK.Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    IK.Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddHurtMelee d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aHurtMelee = n + aHurtMelee ar}
    IK.AddArmorMelee d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorMelee = n + aArmorMelee ar}
    IK.AddArmorRanged d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorRanged = n + aArmorRanged ar}
    IK.AddMaxHP d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aMaxHP = n + aMaxHP ar}
    IK.AddMaxCalm d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aMaxCalm = n + aMaxCalm ar}
    IK.AddSpeed d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSpeed = n + aSpeed ar}
    IK.AddSight d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSight = n + aSight ar}
    IK.AddSmell d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSmell = n + aSmell ar}
    IK.AddShine d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aShine = n + aShine ar}
    IK.AddNocto d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aNocto = n + aNocto ar}
    IK.AddAggression d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aAggression = n + aAggression ar}
    IK.AddAbility ab d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                                (aSkills ar)}

addMeanAspect :: AspectRecord -> IK.Aspect -> AspectRecord
addMeanAspect !ar !asp =
  case asp of
    IK.Timeout d ->
      let n = Dice.meanDice d
      in assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddHurtMelee d ->
      let n = Dice.meanDice d
      in ar {aHurtMelee = n + aHurtMelee ar}
    IK.AddArmorMelee d ->
      let n = Dice.meanDice d
      in ar {aArmorMelee = n + aArmorMelee ar}
    IK.AddArmorRanged d ->
      let n = Dice.meanDice d
      in ar {aArmorRanged = n + aArmorRanged ar}
    IK.AddMaxHP d ->
      let n = Dice.meanDice d
      in ar {aMaxHP = n + aMaxHP ar}
    IK.AddMaxCalm d ->
      let n = Dice.meanDice d
      in ar {aMaxCalm = n + aMaxCalm ar}
    IK.AddSpeed d ->
      let n = Dice.meanDice d
      in ar {aSpeed = n + aSpeed ar}
    IK.AddSight d ->
      let n = Dice.meanDice d
      in ar {aSight = n + aSight ar}
    IK.AddSmell d ->
      let n = Dice.meanDice d
      in ar {aSmell = n + aSmell ar}
    IK.AddShine d ->
      let n = Dice.meanDice d
      in ar {aShine = n + aShine ar}
    IK.AddNocto d ->
      let n = Dice.meanDice d
      in ar {aNocto = n + aNocto ar}
    IK.AddAggression d ->
      let n = Dice.meanDice d
      in ar {aAggression = n + aAggression ar}
    IK.AddAbility ab d ->
      let n = Dice.meanDice d
      in ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                         (aSkills ar)}

seedToAspect :: ItemSeed -> IK.ItemKind -> AbsDepth -> AbsDepth -> AspectRecord
seedToAspect (ItemSeed itemSeed) kind ldepth totalDepth =
  let rollM = foldlM' (castAspect ldepth totalDepth) emptyAspectRecord
                      (IK.iaspects kind)
  in St.evalState rollM (mkStdGen itemSeed)

-- If @False@, aspects of this kind are most probably fixed, not random.
aspectsRandom :: IK.ItemKind -> Bool
aspectsRandom kind =
  let rollM = foldlM' (castAspect (AbsDepth 10) (AbsDepth 10))
                      emptyAspectRecord (IK.iaspects kind)
      gen = mkStdGen 0
  in show gen /= show (St.execState rollM gen)

meanAspect :: IK.ItemKind -> AspectRecord
meanAspect kind = foldl' addMeanAspect emptyAspectRecord (IK.iaspects kind)

aspectRecordFull :: ItemFull -> AspectRecord
aspectRecordFull itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAspect=Just aspectRecord} -> aspectRecord
    Just ItemDisco{itemAspectMean} -> itemAspectMean
    Nothing -> emptyAspectRecord

type ItemTimer = [Time]

type ItemQuant = (Int, ItemTimer)

type ItemBag = EM.EnumMap ItemId ItemQuant

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item
