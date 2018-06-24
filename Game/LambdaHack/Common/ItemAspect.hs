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
  | AddHurtMelee Dice.Dice    -- ^ percentage damage bonus in melee
  | AddArmorMelee Dice.Dice   -- ^ percentage armor bonus against melee
  | AddArmorRanged Dice.Dice  -- ^ percentage armor bonus against ranged
  | AddMaxHP Dice.Dice        -- ^ maximal hp
  | AddMaxCalm Dice.Dice      -- ^ maximal calm
  | AddSpeed Dice.Dice        -- ^ speed in m/10s (not when pushed or pulled)
  | AddSight Dice.Dice        -- ^ FOV radius, where 1 means a single tile FOV
  | AddSmell Dice.Dice        -- ^ smell radius
  | AddShine Dice.Dice        -- ^ shine radius
  | AddNocto Dice.Dice        -- ^ noctovision radius
  | AddAggression Dice.Dice   -- ^ aggression, e.g., when closing in for melee
  | AddAbility Ability.Ability Dice.Dice  -- ^ bonus to an ability
  deriving (Show, Eq, Ord, Generic)

-- | Record of sums of aspect values of an item, container, actor, etc.
data AspectRecord = AspectRecord
  { aTimeout     :: Int
  , aHurtMelee   :: Int
  , aArmorMelee  :: Int
  , aArmorRanged :: Int
  , aMaxHP       :: Int
  , aMaxCalm     :: Int
  , aSpeed       :: Int
  , aSight       :: Int
  , aSmell       :: Int
  , aShine       :: Int
  , aNocto       :: Int
  , aAggression  :: Int
  , aSkills      :: Ability.Skills
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

castAspect :: Dice.AbsDepth -> Dice.AbsDepth -> AspectRecord -> Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    AddHurtMelee d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aHurtMelee = n + aHurtMelee ar}
    AddArmorMelee d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorMelee = n + aArmorMelee ar}
    AddArmorRanged d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aArmorRanged = n + aArmorRanged ar}
    AddMaxHP d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aMaxHP = n + aMaxHP ar}
    AddMaxCalm d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aMaxCalm = n + aMaxCalm ar}
    AddSpeed d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSpeed = n + aSpeed ar}
    AddSight d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSight = n + aSight ar}
    AddSmell d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aSmell = n + aSmell ar}
    AddShine d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aShine = n + aShine ar}
    AddNocto d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aNocto = n + aNocto ar}
    AddAggression d -> do
      n <- castDice ldepth totalDepth d
      return $! ar {aAggression = n + aAggression ar}
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
    AddHurtMelee d ->
      let n = ceilingMeanDice d
      in ar {aHurtMelee = n + aHurtMelee ar}
    AddArmorMelee d ->
      let n = ceilingMeanDice d
      in ar {aArmorMelee = n + aArmorMelee ar}
    AddArmorRanged d ->
      let n = ceilingMeanDice d
      in ar {aArmorRanged = n + aArmorRanged ar}
    AddMaxHP d ->
      let n = ceilingMeanDice d
      in ar {aMaxHP = n + aMaxHP ar}
    AddMaxCalm d ->
      let n = ceilingMeanDice d
      in ar {aMaxCalm = n + aMaxCalm ar}
    AddSpeed d ->
      let n = ceilingMeanDice d
      in ar {aSpeed = n + aSpeed ar}
    AddSight d ->
      let n = ceilingMeanDice d
      in ar {aSight = n + aSight ar}
    AddSmell d ->
      let n = ceilingMeanDice d
      in ar {aSmell = n + aSmell ar}
    AddShine d ->
      let n = ceilingMeanDice d
      in ar {aShine = n + aShine ar}
    AddNocto d ->
      let n = ceilingMeanDice d
      in ar {aNocto = n + aNocto ar}
    AddAggression d ->
      let n = ceilingMeanDice d
      in ar {aAggression = n + aAggression ar}
    AddAbility ab d ->
      let n = ceilingMeanDice d
      in ar {aSkills = Ability.addSkills (EM.singleton ab n)
                                         (aSkills ar)}

ceilingMeanDice :: Dice.Dice -> Int
ceilingMeanDice d = ceiling $ Dice.meanDice d

sumAspectRecord :: [(AspectRecord, Int)] -> AspectRecord
sumAspectRecord l = AspectRecord
  { aTimeout     = 0
  , aHurtMelee   = sumScaled aHurtMelee
  , aArmorMelee  = sumScaled aArmorMelee
  , aArmorRanged = sumScaled aArmorRanged
  , aMaxHP       = sumScaled aMaxHP
  , aMaxCalm     = sumScaled aMaxCalm
  , aSpeed       = sumScaled aSpeed
  , aSight       = sumScaled aSight
  , aSmell       = sumScaled aSmell
  , aShine       = sumScaled aShine
  , aNocto       = sumScaled aNocto
  , aAggression  = sumScaled aAggression
  , aSkills      = sumScaledAbility
  }
 where
  sumScaled f = sum $ map (\(ar, k) -> f ar * k) l
  sumScaledAbility =
    EM.unionsWith (+) $ map (\(ar, k) -> Ability.scaleSkills k $ aSkills ar) l

aspectRecordToList :: AspectRecord -> [Aspect]
aspectRecordToList AspectRecord{..} =
  [Timeout $ Dice.intToDice aTimeout | aTimeout /= 0]
  ++ [AddHurtMelee $ Dice.intToDice aHurtMelee | aHurtMelee /= 0]
  ++ [AddArmorMelee $ Dice.intToDice aArmorMelee | aArmorMelee /= 0]
  ++ [AddArmorRanged $ Dice.intToDice aArmorRanged | aArmorRanged /= 0]
  ++ [AddMaxHP $ Dice.intToDice aMaxHP | aMaxHP /= 0]
  ++ [AddMaxCalm $ Dice.intToDice aMaxCalm | aMaxCalm /= 0]
  ++ [AddSpeed $ Dice.intToDice aSpeed | aSpeed /= 0]
  ++ [AddSight $ Dice.intToDice aSight | aSight /= 0]
  ++ [AddSmell $ Dice.intToDice aSmell | aSmell /= 0]
  ++ [AddShine $ Dice.intToDice aShine | aShine /= 0]
  ++ [AddNocto $ Dice.intToDice aNocto | aNocto /= 0]
  ++ [AddAggression $ Dice.intToDice aAggression | aAggression /= 0]
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
      + aMaxCalm + aSmell
      + aNocto  -- powerful, but hard to boost over aSight
    EqpSlotAddHurtMelee -> aHurtMelee
    EqpSlotAddArmorMelee -> aArmorMelee
    EqpSlotAddArmorRanged -> aArmorRanged
    EqpSlotAddMaxHP -> aMaxHP
    EqpSlotAddSpeed -> aSpeed
    EqpSlotAddSight -> aSight
    EqpSlotLightSource -> aShine
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
    EqpSlotAddMaxCalm -> aMaxCalm
    EqpSlotAddSmell -> aSmell
    EqpSlotAddNocto -> aNocto
    EqpSlotAddAggression -> aAggression
    EqpSlotAbWait -> EM.findWithDefault 0 Ability.AbWait aSkills
    EqpSlotAbMoveItem -> EM.findWithDefault 0 Ability.AbMoveItem aSkills
