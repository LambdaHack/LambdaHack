{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of item aspects and its operations.
module Game.LambdaHack.Common.ItemAspect
  ( AspectRecord(..), KindMean(..), ItemSpeedup
  , emptyAspectRecord, addMeanAspect, castAspect, aspectsRandom
  , sumAspectRecord, aspectRecordToList, rollAspectRecord
  , getAbility, emptyItemSpeedup, getKindMean, speedupItem
  , prEqpSlot
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ceilingMeanDice, meanAspect
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Binary
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import qualified System.Random as R

import           Game.LambdaHack.Common.Ability (EqpSlot (..))
import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.ContentData
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | Record of sums of aspect values of an item, container, actor, etc.
data AspectRecord = AspectRecord
  { aTimeout :: Int
  , aSkills  :: Ability.Skills
  , aFlags   :: Ability.Flags
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable AspectRecord

instance Binary AspectRecord

-- | Partial information about an item, deduced from its item kind.
-- These are assigned to each 'ItemKind'. The @kmConst@ flag says whether
-- the item's aspect record is constant rather than random or dependent
-- on item creation dungeon level.
data KindMean = KindMean
  { kmConst :: Bool  -- ^ whether the item doesn't need second identification
  , kmMean  :: AspectRecord  -- ^ mean value of item's possible aspect records
  }
  deriving (Show, Eq, Ord, Generic)

-- | Map from an item kind identifier to the mean aspect value for the kind.
--
-- Significant portions of this map are unused and so intentially kept
-- unevaluated.
newtype ItemSpeedup = ItemSpeedup (V.Vector KindMean)
  deriving (Show, Eq, Generic)

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout = 0
  , aSkills  = Ability.zeroSkills
  , aFlags   = Ability.Flags ES.empty
  }

castAspect :: Dice.AbsDepth -> Dice.AbsDepth -> AspectRecord -> IK.Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    IK.Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddAbility ab d -> do
      n <- castDice ldepth totalDepth d
      return $! if n /= 0
                then ar {aSkills = Ability.addAb ab n (aSkills ar)}
                else ar
    IK.SetFeature feat ->
      return $! ar {aFlags = Ability.Flags
                             $ ES.insert feat (Ability.flags $ aFlags ar)}
    _ -> return ar

-- If @False@, aspects of this kind are most probably fixed, not random
-- nor dependent on dungeon level where the item is created.
aspectsRandom :: [IK.Aspect] -> Bool
aspectsRandom ass =
  let rollM depth =
        foldlM' (castAspect (Dice.AbsDepth depth) (Dice.AbsDepth 10))
                emptyAspectRecord ass
      gen = R.mkStdGen 0
      (ar0, gen0) = St.runState (rollM 0) gen
      (ar1, gen1) = St.runState (rollM 10) gen0
  in show gen /= show gen0 || show gen /= show gen1 || ar0 /= ar1

addMeanAspect :: AspectRecord -> IK.Aspect -> AspectRecord
addMeanAspect !ar !asp =
  case asp of
    IK.Timeout d ->
      let n = ceilingMeanDice d
      in assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddAbility ab d ->
      let n = ceilingMeanDice d
      in if n /= 0
         then ar {aSkills = Ability.addAb ab n (aSkills ar)}
         else ar
    IK.SetFeature feat ->
      ar {aFlags = Ability.Flags $ ES.insert feat (Ability.flags $ aFlags ar)}
    _ -> ar

ceilingMeanDice :: Dice.Dice -> Int
ceilingMeanDice d = ceiling $ Dice.meanDice d

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout = 0
  , aSkills  = Ability.sumScaledAbility $ map (first aSkills) l
  , aFlags   = Ability.Flags ES.empty
  }

aspectRecordToList :: AspectRecord -> [IK.Aspect]
aspectRecordToList AspectRecord{..} =
  [IK.Timeout $ Dice.intToDice aTimeout | aTimeout /= 0]
  ++ [ IK.AddAbility ab $ Dice.intToDice n
     | (ab, n) <- Ability.skillsToList aSkills ]
  ++ [ IK.SetFeature feat
     | feat <- ES.elems $ Ability.flags aFlags ]

rollAspectRecord :: [IK.Aspect] -> Dice.AbsDepth -> Dice.AbsDepth
                 -> Rnd AspectRecord
rollAspectRecord ass ldepth totalDepth =
  foldlM' (castAspect ldepth totalDepth) emptyAspectRecord ass

getAbility :: Ability.Ability -> AspectRecord -> Int
{-# INLINE getAbility #-}
getAbility ab ar = Ability.getAb ab $ aSkills ar

emptyItemSpeedup :: ItemSpeedup
emptyItemSpeedup = ItemSpeedup V.empty

getKindMean :: ContentId IK.ItemKind -> ItemSpeedup -> KindMean
getKindMean kindId (ItemSpeedup is) = is V.! contentIdIndex kindId

speedupItem :: ContentData IK.ItemKind -> ItemSpeedup
speedupItem coitem =
  let f !kind =
        let kmMean = meanAspect kind
            kmConst = not $ aspectsRandom (IK.iaspects kind)
        in KindMean{..}
  in ItemSpeedup $! omapVector coitem f

meanAspect :: IK.ItemKind -> AspectRecord
meanAspect kind = foldl' addMeanAspect emptyAspectRecord (IK.iaspects kind)

prEqpSlot :: EqpSlot -> AspectRecord -> Int
prEqpSlot eqpSlot ar@AspectRecord{..} =
  case eqpSlot of
    EqpSlotMiscBonus ->
      aTimeout  -- usually better items have longer timeout
      + Ability.getAb Ability.AbMaxCalm aSkills
      + Ability.getAb Ability.AbSmell aSkills
      + Ability.getAb Ability.AbNocto aSkills
          -- powerful, but hard to boost over aSight
    EqpSlotAddHurtMelee -> Ability.getAb Ability.AbHurtMelee aSkills
    EqpSlotAddArmorMelee -> Ability.getAb Ability.AbArmorMelee aSkills
    EqpSlotAddArmorRanged -> Ability.getAb Ability.AbArmorRanged aSkills
    EqpSlotAddMaxHP -> Ability.getAb Ability.AbMaxHP aSkills
    EqpSlotAddSpeed -> Ability.getAb Ability.AbSpeed aSkills
    EqpSlotAddSight -> Ability.getAb Ability.AbSight aSkills
    EqpSlotLightSource -> Ability.getAb Ability.AbShine aSkills
    EqpSlotWeapon -> error $ "" `showFailure` ar
    EqpSlotMiscAbility ->
      Ability.getAb Ability.AbWait aSkills
      + Ability.getAb Ability.AbMoveItem aSkills
    EqpSlotAbMove -> Ability.getAb Ability.AbMove aSkills
    EqpSlotAbMelee -> Ability.getAb Ability.AbMelee aSkills
    EqpSlotAbDisplace -> Ability.getAb Ability.AbDisplace aSkills
    EqpSlotAbAlter -> Ability.getAb Ability.AbAlter aSkills
    EqpSlotAbProject -> Ability.getAb Ability.AbProject aSkills
    EqpSlotAbApply -> Ability.getAb Ability.AbApply aSkills
    EqpSlotAddMaxCalm -> Ability.getAb Ability.AbMaxCalm aSkills
    EqpSlotAddSmell -> Ability.getAb Ability.AbSmell aSkills
    EqpSlotAddNocto -> Ability.getAb Ability.AbNocto aSkills
    EqpSlotAddAggression -> Ability.getAb Ability.AbAggression aSkills
    EqpSlotAbWait -> Ability.getAb Ability.AbWait aSkills
    EqpSlotAbMoveItem -> Ability.getAb Ability.AbMoveItem aSkills
