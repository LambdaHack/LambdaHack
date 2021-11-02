{-# LANGUAGE DeriveGeneric #-}
-- | The type of item aspects and its operations.
module Game.LambdaHack.Common.ItemAspect
  ( AspectRecord(..), KindMean(..)
  , emptyAspectRecord, addMeanAspect, castAspect, aspectsRandom
  , aspectRecordToList, rollAspectRecord, getSkill, checkFlag, meanAspect
  , onlyMinorEffects, itemTrajectory, totalRange, isHumanTrinket
  , goesIntoEqp, loreFromContainer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ceilingMeanDice
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Binary
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | Record of skills conferred by an item as well as of item flags
-- and other item aspects.
data AspectRecord = AspectRecord
  { aTimeout   :: Int
  , aSkills    :: Ability.Skills
  , aFlags     :: Ability.Flags
  , aELabel    :: Text
  , aToThrow   :: IK.ThrowMod
  , aPresentAs :: Maybe (GroupName IK.ItemKind)
  , aEqpSlot   :: Maybe Ability.EqpSlot
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable AspectRecord

instance Binary AspectRecord

-- | Partial information about an item, deduced from its item kind.
-- These are assigned to each 'IK.ItemKind'. The @kmConst@ flag says whether
-- the item's aspect record is constant rather than random or dependent
-- on item creation dungeon level.
data KindMean = KindMean
  { kmConst :: Bool  -- ^ whether the item doesn't need second identification
  , kmMean  :: AspectRecord  -- ^ mean value of item's possible aspect records
  }
  deriving (Show, Eq, Ord)

emptyAspectRecord :: AspectRecord
emptyAspectRecord = AspectRecord
  { aTimeout = 0
  , aSkills = Ability.zeroSkills
  , aFlags = Ability.Flags ES.empty
  , aELabel = ""
  , aToThrow = IK.ThrowMod 100 100 1
  , aPresentAs = Nothing
  , aEqpSlot = Nothing
  }

castAspect :: Dice.AbsDepth -> Dice.AbsDepth -> AspectRecord -> IK.Aspect
           -> Rnd AspectRecord
castAspect !ldepth !totalDepth !ar !asp =
  case asp of
    IK.Timeout d -> do
      n <- castDice ldepth totalDepth d
      return $! assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddSkill sk d -> do
      n <- castDice ldepth totalDepth d
      return $! if n /= 0
                then ar {aSkills = Ability.addSk sk n (aSkills ar)}
                else ar
    IK.SetFlag feat ->
      return $! ar {aFlags = Ability.Flags
                             $ ES.insert feat (Ability.flags $ aFlags ar)}
    IK.ELabel t -> return $! ar {aELabel = t}
    IK.ToThrow tt -> return $! ar {aToThrow = tt}
    IK.PresentAs ha -> return $! ar {aPresentAs = Just ha}
    IK.EqpSlot slot -> return $! ar {aEqpSlot = Just slot}
    IK.Odds d aspects1 aspects2 -> do
      pick1 <- oddsDice ldepth totalDepth d
      foldlM' (castAspect ldepth totalDepth) ar $
        if pick1 then aspects1 else aspects2

-- If @False@, aspects of this kind are most probably fixed, not random
-- nor dependent on dungeon level where the item is created.
aspectsRandom :: [IK.Aspect] -> Bool
aspectsRandom ass =
  let rollM depth =
        foldlM' (castAspect (Dice.AbsDepth depth) (Dice.AbsDepth 10))
                emptyAspectRecord ass
      gen = SM.mkSMGen 0
      (ar0, gen0) = St.runState (rollM 0) gen
      (ar1, gen1) = St.runState (rollM 10) gen0
  in show gen /= show gen0 || show gen /= show gen1 || ar0 /= ar1

addMeanAspect :: AspectRecord -> IK.Aspect -> AspectRecord
addMeanAspect !ar !asp =
  case asp of
    IK.Timeout d ->
      let n = ceilingMeanDice d
      in assert (aTimeout ar == 0) $ ar {aTimeout = n}
    IK.AddSkill sk d ->
      let n = ceilingMeanDice d
      in if n /= 0
         then ar {aSkills = Ability.addSk sk n (aSkills ar)}
         else ar
    IK.SetFlag feat ->
      ar {aFlags = Ability.Flags $ ES.insert feat (Ability.flags $ aFlags ar)}
    IK.ELabel t -> ar {aELabel = t}
    IK.ToThrow tt -> ar {aToThrow = tt}
    IK.PresentAs ha -> ar {aPresentAs = Just ha}
    IK.EqpSlot slot -> ar {aEqpSlot = Just slot}
    IK.Odds{} -> ar  -- can't tell, especially since we don't know the level

ceilingMeanDice :: Dice.Dice -> Int
ceilingMeanDice d = ceiling $ Dice.meanDice d

aspectRecordToList :: AspectRecord -> [IK.Aspect]
aspectRecordToList AspectRecord{..} =
  [IK.Timeout $ Dice.intToDice aTimeout | aTimeout /= 0]
  ++ [ IK.AddSkill sk $ Dice.intToDice n
     | (sk, n) <- Ability.skillsToList aSkills ]
  ++ [IK.SetFlag feat | feat <- ES.elems $ Ability.flags aFlags]
  ++ [IK.ELabel aELabel | not $ T.null aELabel]
  ++ [IK.ToThrow aToThrow | not $ aToThrow == IK.ThrowMod 100 100 1]
  ++ maybe [] (\ha -> [IK.PresentAs ha]) aPresentAs
  ++ maybe [] (\slot -> [IK.EqpSlot slot]) aEqpSlot

rollAspectRecord :: [IK.Aspect] -> Dice.AbsDepth -> Dice.AbsDepth
                 -> Rnd AspectRecord
rollAspectRecord ass ldepth totalDepth =
  foldlM' (castAspect ldepth totalDepth) emptyAspectRecord ass

getSkill :: Ability.Skill -> AspectRecord -> Int
{-# INLINE getSkill #-}
getSkill sk ar = Ability.getSk sk $ aSkills ar

checkFlag :: Ability.Flag -> AspectRecord -> Bool
{-# INLINE checkFlag #-}
checkFlag flag ar = Ability.checkFl flag (aFlags ar)

meanAspect :: IK.ItemKind -> AspectRecord
meanAspect kind = foldl' addMeanAspect emptyAspectRecord (IK.iaspects kind)

-- Kinetic damage is not considered major effect, even though it
-- identifies an item, when one hits with it. However, it's tedious
-- to wait for weapon identification until first hit and also
-- if a weapon is periodically activated, the kinetic damage would not apply,
-- so we'd need special cases that force identification or warn
-- or here not consider kinetic damage a major effect if item is periodic.
-- So we opt for KISS and identify effect-less weapons at pick-up,
-- not at first hit.
onlyMinorEffects :: AspectRecord -> IK.ItemKind -> Bool
onlyMinorEffects ar kind =
  checkFlag Ability.MinorEffects ar  -- override
  || not (any (not . IK.alwaysDudEffect) $ IK.ieffects kind)
       -- exhibits no major effects

itemTrajectory :: AspectRecord -> IK.ItemKind -> [Point]
               -> ([Vector], (Speed, Int))
itemTrajectory ar itemKind path =
  let IK.ThrowMod{..} = aToThrow ar
  in computeTrajectory (IK.iweight itemKind) throwVelocity throwLinger path

totalRange :: AspectRecord -> IK.ItemKind -> Int
totalRange ar itemKind = snd $ snd $ itemTrajectory ar itemKind []

isHumanTrinket :: IK.ItemKind -> Bool
isHumanTrinket itemKind =
  maybe False (> 0) $ lookup IK.VALUABLE $ IK.ifreq itemKind
    -- risk from treasure hunters

goesIntoEqp :: AspectRecord -> Bool
goesIntoEqp ar = checkFlag Ability.Equipable ar
                 || checkFlag Ability.Meleeable ar

loreFromContainer :: AspectRecord -> Container -> SLore
loreFromContainer arItem c = case c of
  CFloor{} -> SItem
  CEmbed{} -> SEmbed
  CActor _ store -> if | checkFlag Ability.Blast arItem -> SBlast
                       | checkFlag Ability.Condition arItem -> SCondition
                       | otherwise -> loreFromMode $ MStore store
  CTrunk{} -> if checkFlag Ability.Blast arItem then SBlast else STrunk
