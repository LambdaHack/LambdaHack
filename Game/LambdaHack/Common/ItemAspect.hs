{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of item aspects and its operations.
module Game.LambdaHack.Common.ItemAspect
  ( Aspect(..), AspectRecord(..), KindMean(..), EqpSlot(..)
  , emptyAspectRecord, addMeanAspect, castAspect, aspectsRandom
  , sumAspectRecord, aspectRecordToList, rollAspectRecord, prEqpSlot
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ceilingMeanDice
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import qualified Control.Monad.Trans.State.Strict as St
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Random

-- | Aspects of items. Those that are named @Add*@ are additive
-- (starting at 0) for all items wielded by an actor and they affect the actor.
data Aspect =
    Timeout Dice.Dice         -- ^ some effects disabled until item recharges;
                              --   expressed in game turns
  | AddAbility Ability.Ability Dice.Dice  -- ^ bonus to an ability
  deriving (Show, Eq, Ord, Generic)

-- | Record of sums of aspect values of an item, container, actor, etc.
data AspectRecord = AspectRecord
  { aTimeout :: Int
  , aSkills  :: Ability.Skills
  }
  deriving (Show, Eq, Ord, Generic)

-- | Partial information about an item, deduced from its item kind.
-- These are assigned to each 'ItemKind'. The @kmConst@ flag says whether
-- the item's aspect record is constant rather than random or dependent
-- on item creation dungeon level.
data KindMean = KindMean
  { kmConst :: Bool  -- ^ whether the item doesn't need second identification
  , kmMean  :: AspectRecord  -- ^ mean value of item's possible aspect records
  }
  deriving (Show, Eq, Ord, Generic)

-- | AI and UI hints about the role of the item.
data EqpSlot =
    EqpSlotMiscBonus
  | EqpSlotAddHurtMelee
  | EqpSlotAddArmorMelee
  | EqpSlotAddArmorRanged
  | EqpSlotAddMaxHP
  | EqpSlotAddSpeed
  | EqpSlotAddSight
  | EqpSlotLightSource
  | EqpSlotWeapon
  | EqpSlotMiscAbility
  | EqpSlotAbMove
  | EqpSlotAbMelee
  | EqpSlotAbDisplace
  | EqpSlotAbAlter
  | EqpSlotAbProject
  | EqpSlotAbApply
  -- Do not use in content:
  | EqpSlotAddMaxCalm
  | EqpSlotAddSmell
  | EqpSlotAddNocto
  | EqpSlotAddAggression
  | EqpSlotAbWait
  | EqpSlotAbMoveItem
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData Aspect

instance NFData EqpSlot

instance Hashable AspectRecord

instance Binary AspectRecord

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout     = 0
  , aSkills      = Ability.zeroSkills
  }

castAspect :: Dice.AbsDepth -> Dice.AbsDepth -> AspectRecord -> Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    AddAbility ab d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                                (aSkills ar)}

-- If @False@, aspects of this kind are most probably fixed, not random
-- nor dependent on dungeon level where the item is created.
aspectsRandom :: [Aspect] -> Bool
aspectsRandom ass =
  let rollM depth =
        foldlM' (castAspect (Dice.AbsDepth depth) (Dice.AbsDepth 10))
                emptyAspectRecord ass
      gen = R.mkStdGen 0
      (ar0, gen0) = St.runState (rollM 0) gen
      (ar1, gen1) = St.runState (rollM 10) gen0
  in show gen /= show gen0 || show gen /= show gen1 || ar0 /= ar1

addMeanAspect :: AspectRecord -> Aspect -> AspectRecord
addMeanAspect !ar !asp =
  case asp of
    Timeout d ->
      let n = ceilingMeanDice d
      in assert (aTimeout ar == 0) $ ar {aTimeout = n}
    AddAbility ab d ->
      let n = ceilingMeanDice d
      in ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                         (aSkills ar)}

ceilingMeanDice :: Dice.Dice -> Int
ceilingMeanDice d = ceiling $ Dice.meanDice d

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout     = 0
  , aSkills      = sumScaledAbility
  }
 where
  sumScaledAbility =
    EM.unionsWith (+) $ map (\(ar, k) -> Ability.scaleSkills k $ aSkills ar) l

aspectRecordToList :: AspectRecord -> [Aspect]
aspectRecordToList AspectRecord{..} =
  [Timeout $ Dice.intToDice aTimeout | aTimeout /= 0]
  ++ [AddAbility ab $ Dice.intToDice n | (ab, n) <- EM.assocs aSkills, n /= 0]

rollAspectRecord :: [Aspect] -> Dice.AbsDepth -> Dice.AbsDepth
                 -> Rnd AspectRecord
rollAspectRecord ass ldepth totalDepth =
  foldlM' (castAspect ldepth totalDepth) emptyAspectRecord ass

prEqpSlot :: EqpSlot -> AspectRecord -> Int
prEqpSlot eqpSlot ar@AspectRecord{..} =
  case eqpSlot of
    EqpSlotMiscBonus ->
      aTimeout  -- usually better items have longer timeout
      + EM.findWithDefault 0 Ability.AbMaxCalm aSkills
      + EM.findWithDefault 0 Ability.AbSmell aSkills
      + EM.findWithDefault 0 Ability.AbNocto aSkills
          -- powerful, but hard to boost over aSight
    EqpSlotAddHurtMelee -> EM.findWithDefault 0 Ability.AbHurtMelee aSkills
    EqpSlotAddArmorMelee -> EM.findWithDefault 0 Ability.AbArmorMelee aSkills
    EqpSlotAddArmorRanged -> EM.findWithDefault 0 Ability.AbArmorRanged aSkills
    EqpSlotAddMaxHP -> EM.findWithDefault 0 Ability.AbMaxHP aSkills
    EqpSlotAddSpeed -> EM.findWithDefault 0 Ability.AbSpeed aSkills
    EqpSlotAddSight -> EM.findWithDefault 0 Ability.AbSight aSkills
    EqpSlotLightSource -> EM.findWithDefault 0 Ability.AbShine aSkills
    EqpSlotWeapon -> error $ "" `showFailure` ar
    EqpSlotMiscAbility ->
      EM.findWithDefault 0 Ability.AbWait aSkills
      + EM.findWithDefault 0 Ability.AbMoveItem aSkills
    EqpSlotAbMove -> EM.findWithDefault 0 Ability.AbMove aSkills
    EqpSlotAbMelee -> EM.findWithDefault 0 Ability.AbMelee aSkills
    EqpSlotAbDisplace -> EM.findWithDefault 0 Ability.AbDisplace aSkills
    EqpSlotAbAlter -> EM.findWithDefault 0 Ability.AbAlter aSkills
    EqpSlotAbProject -> EM.findWithDefault 0 Ability.AbProject aSkills
    EqpSlotAbApply -> EM.findWithDefault 0 Ability.AbApply aSkills
    EqpSlotAddMaxCalm -> EM.findWithDefault 0 Ability.AbMaxCalm aSkills
    EqpSlotAddSmell -> EM.findWithDefault 0 Ability.AbSmell aSkills
    EqpSlotAddNocto -> EM.findWithDefault 0 Ability.AbNocto aSkills
    EqpSlotAddAggression ->EM.findWithDefault 0 Ability.AbAggression aSkills
    EqpSlotAbWait -> EM.findWithDefault 0 Ability.AbWait aSkills
    EqpSlotAbMoveItem -> EM.findWithDefault 0 Ability.AbMoveItem aSkills
