{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..)
  , seedToAspect, meanAspect, aspectRecordToList, aspectRecordFull
    -- * Item discovery types
  , ItemKindIx, KindMean(..), DiscoveryKind, ItemSeed
  , AspectRecord(..), emptyAspectRecord, sumAspectRecord, DiscoveryAspect
  , ItemFull(..), ItemDisco(..), itemNoDisco
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
-- The full map, as known by the server, is a bijection.
type DiscoveryKind = EM.EnumMap ItemKindIx KindMean

-- | A seed for rolling aspects of an item
-- Clients have partial knowledge of how item ids map to the seeds.
-- They gain knowledge by identifying items.
newtype ItemSeed = ItemSeed Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

data AspectRecord = AspectRecord
  { aTimeout     :: !Int
  , aHurtMelee   :: !Int
  , aHurtRanged  :: !Int
  , aArmorMelee  :: !Int
  , aArmorRanged :: !Int
  , aMaxHP       :: !Int
  , aMaxCalm     :: !Int
  , aSpeed       :: !Int
  , aSight       :: !Int
  , aSmell       :: !Int
  , aShine       :: !Int
  , aNocto       :: !Int
  , aSkills      :: !Ability.Skills
  }
  deriving (Show, Eq, Generic)

instance Binary AspectRecord

instance Hashable AspectRecord

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout     = 0
  , aHurtMelee   = 0
  , aHurtRanged  = 0
  , aArmorMelee  = 0
  , aArmorRanged = 0
  , aMaxHP       = 0
  , aMaxCalm     = 0
  , aSpeed       = 0
  , aSight       = 0
  , aSmell       = 0
  , aShine       = 0
  , aNocto       = 0
  , aSkills      = Ability.zeroSkills
  }

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout     = 0
  , aHurtMelee   = trim200 $ sum $ mapScale aHurtMelee l
  , aHurtRanged  = trim200 $ sum $ mapScale aHurtRanged l
  , aArmorMelee  = trim200 $ sum $ mapScale aArmorMelee l
  , aArmorRanged = trim200 $ sum $ mapScale aArmorRanged l
  , aMaxHP       = sum $ mapScale aMaxHP l
  , aMaxCalm     = sum $ mapScale aMaxCalm l
  , aSpeed       = sum $ mapScale aSpeed l
  , aSight       = sum $ mapScale aSight l
  , aSmell       = sum $ mapScale aSmell l
  , aShine       = sum $ mapScale aShine l
  , aNocto       = sum $ mapScale aNocto l
  , aSkills      = EM.unionsWith (+) $ mapScaleAbility l
  }
 where
  mapScale f = map (\(ar, k) -> f ar * k)
  mapScaleAbility = map (\(ar, k) -> Ability.scaleSkills k $ aSkills ar)

trim200 :: Int -> Int
trim200 x = min 200 $ max (-200) x

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

-- | Game items in actor possesion or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx    -- ^ index pointing to the kind of the item
  , jlid     :: !LevelId       -- ^ the level on which item was created
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

aspectRecordToList :: AspectRecord -> [IK.Aspect]
aspectRecordToList AspectRecord{..} =
  [IK.Timeout $ intToDice aTimeout | aTimeout /= 0]
  ++ [IK.AddHurtMelee $ intToDice aHurtMelee | aHurtMelee /= 0]
  ++ [IK.AddHurtRanged $ intToDice aHurtRanged | aHurtRanged /= 0]
  ++ [IK.AddArmorMelee $ intToDice aArmorMelee | aArmorMelee /= 0]
  ++ [IK.AddArmorRanged $ intToDice aArmorRanged | aArmorRanged /= 0]
  ++ [IK.AddMaxHP $ intToDice aMaxHP | aMaxHP /= 0]
  ++ [IK.AddMaxCalm $ intToDice aMaxCalm | aMaxCalm /= 0]
  ++ [IK.AddSpeed $ intToDice aSpeed | aSpeed /= 0]
  ++ [IK.AddSight $ intToDice aSight | aSight /= 0]
  ++ [IK.AddSmell $ intToDice aSmell | aSmell /= 0]
  ++ [IK.AddShine $ intToDice aShine | aShine /= 0]
  ++ [IK.AddNocto $ intToDice aNocto | aNocto /= 0]
  ++ [IK.AddAbility ab $ intToDice n | (ab, n) <- EM.assocs aSkills, n /= 0]

castAspect :: AbsDepth -> AbsDepth -> AspectRecord -> IK.Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    IK.Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddHurtMelee d -> do  -- TODO: lenses would reduce duplication below
      n <- castDice ldepth totalDepth d
      return $! ar {aHurtMelee = trim200 $ n + aHurtMelee ar}
    IK.AddHurtRanged d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aHurtRanged = trim200 $ n + aHurtRanged ar}
    IK.AddArmorMelee d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorMelee = trim200 $ n + aArmorMelee ar}
    IK.AddArmorRanged d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorRanged = trim200 $ n + aArmorRanged ar}
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
      in ar {aHurtMelee = trim200 $ n + aHurtMelee ar}
    IK.AddHurtRanged d ->
      let n = Dice.meanDice d
      in ar {aHurtRanged = trim200 $ n + aHurtRanged ar}
    IK.AddArmorMelee d ->
      let n = Dice.meanDice d
      in ar {aArmorMelee = trim200 $ n + aArmorMelee ar}
    IK.AddArmorRanged d ->
      let n = Dice.meanDice d
      in ar {aArmorRanged = trim200 $ n + aArmorRanged ar}
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
    IK.AddAbility ab d ->
      let n = Dice.meanDice d
      in ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                         (aSkills ar)}

seedToAspect :: ItemSeed -> IK.ItemKind -> AbsDepth -> AbsDepth -> AspectRecord
seedToAspect (ItemSeed itemSeed) kind ldepth totalDepth =
  let rollM = foldlM' (castAspect ldepth totalDepth) emptyAspectRecord
                      (IK.iaspects kind)
  in St.evalState rollM (mkStdGen itemSeed)

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
