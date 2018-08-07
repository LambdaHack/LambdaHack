{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of item aspects and its operations.
module Game.LambdaHack.Common.ItemAspect
  ( Aspect(..), AspectRecord(..), KindMean(..)
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
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)
import qualified System.Random as R

import           Game.LambdaHack.Common.Ability (EqpSlot (..))
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

instance NFData Aspect

instance Hashable AspectRecord

instance Binary AspectRecord

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout = 0
  , aSkills  = Ability.zeroSkills
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
      return $! if n /= 0
                then ar {aSkills = Ability.addAb ab n (aSkills ar)}
                else ar

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
      in if n /= 0
         then ar {aSkills = Ability.addAb ab n (aSkills ar)}
         else ar

ceilingMeanDice :: Dice.Dice -> Int
ceilingMeanDice d = ceiling $ Dice.meanDice d

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout = 0
  , aSkills  = Ability.sumScaledAbility $ map (first aSkills) l
  }

aspectRecordToList :: AspectRecord -> [Aspect]
aspectRecordToList AspectRecord{..} =
  [Timeout $ Dice.intToDice aTimeout | aTimeout /= 0]
  ++ [ AddAbility ab $ Dice.intToDice n
     | (ab, n) <- Ability.skillsToList aSkills ]

rollAspectRecord :: [Aspect] -> Dice.AbsDepth -> Dice.AbsDepth
                 -> Rnd AspectRecord
rollAspectRecord ass ldepth totalDepth =
  foldlM' (castAspect ldepth totalDepth) emptyAspectRecord ass

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
