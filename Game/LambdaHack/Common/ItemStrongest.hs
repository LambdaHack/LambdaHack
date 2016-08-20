-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthOnSmash, strengthCreateOrgan, strengthDropOrgan
  , strengthEqpSlot, strengthEffect, strongestSlot, sumSlotNoFilter
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  , unknownMelee, filterRecharging, stripRecharging, stripOnSmash
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Ord as Ord

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind

strengthEffect :: (Effect -> [b]) -> ItemFull -> [b]
{-# INLINE strengthEffect #-}
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemKind=ItemKind{ieffects}} ->
      concatMap f ieffects
    Nothing -> []

strengthFeature :: (Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

strengthOnSmash :: ItemFull -> [Effect]
strengthOnSmash =
  let p (OnSmash eff) = [eff]
      p _ = []
  in strengthEffect p

strengthCreateOrgan :: ItemFull -> [GroupName ItemKind]
strengthCreateOrgan =
  let p (CreateItem COrgan grp _) = [grp]
      p (Recharging (CreateItem COrgan grp _)) = [grp]
      p _ = []
  in strengthEffect p

strengthDropOrgan :: ItemFull -> [GroupName ItemKind]
strengthDropOrgan =
  let p (DropItem COrgan grp _) = [grp]
      p (Recharging (DropItem COrgan grp _)) = [grp]
      p _ = []
  in strengthEffect p

strengthEqpSlot :: Item -> Maybe (EqpSlot, Text)
strengthEqpSlot item =
  let p (EqpSlot eqpSlot t) = [(eqpSlot, t)]
      p _ = []
  in case strengthFeature p item of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, item)

strengthToThrow :: Item -> ThrowMod
strengthToThrow item =
  let p (ToThrow tmod) = [tmod]
      p _ = []
  in case strengthFeature p item of
    [] -> ThrowMod 100 100
    [x] -> x
    xs -> assert `failure` (xs, item)

computeTrajectory :: Int -> Int -> Int -> [Point] -> ([Vector], (Speed, Int))
computeTrajectory weight throwVelocity throwLinger path =
  let speed = speedFromWeight weight throwVelocity
      trange = rangeFromSpeedAndLinger speed throwLinger
      btrajectory = take trange $ pathToTrajectory path
  in (btrajectory, (speed, trange))

itemTrajectory :: Item -> [Point] -> ([Vector], (Speed, Int))
itemTrajectory item path =
  let ThrowMod{..} = strengthToThrow item
  in computeTrajectory (jweight item) throwVelocity throwLinger path

totalRange :: Item -> Int
totalRange item = snd $ snd $ itemTrajectory item []

strengthFromEqpSlot :: EqpSlot -> ItemFull -> Int
strengthFromEqpSlot eqpSlot itemFull =
  let AspectRecord{..} = aspectRecordFull itemFull
  in case eqpSlot of
    EqpSlotPeriodic -> if aPeriodic then aTimeout else 0
    EqpSlotTimeout -> aTimeout
    EqpSlotAddHurtMelee -> aHurtMelee
    EqpSlotAddHurtRanged -> aHurtRanged
    EqpSlotAddArmorMelee -> aArmorMelee
    EqpSlotAddArmorRanged -> aArmorRanged
    EqpSlotAddMaxHP -> aMaxHP
    EqpSlotAddMaxCalm -> aMaxCalm
    EqpSlotAddSpeed -> aSpeed
    EqpSlotAddSight -> aSight
    EqpSlotAddSmell -> aSmell
    EqpSlotAddShine -> aShine
    EqpSlotAddNocto -> aNocto
    EqpSlotWeapon ->
      let p (Hurt d) = [Dice.meanDice d]
          p (Burn d) = [Dice.meanDice d]
          p _ = []
      in sum (strengthEffect p itemFull)
    EqpSlotAddAbility ab -> EM.findWithDefault 0 ab aAbility

strongestSlotNoFilter :: EqpSlot -> [(ItemId, ItemFull)]
                      -> [(Int, (ItemId, ItemFull))]
strongestSlotNoFilter eqpSlot is =
  let f (iid, itemFull) = ( strengthFromEqpSlot eqpSlot itemFull
                          , (iid, itemFull) )
  in sortBy (flip $ Ord.comparing fst) $ map f is

strongestSlot :: EqpSlot -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot eqpSlot is =
  let f (_, itemFull) = case strengthEqpSlot $ itemBase itemFull of
        Just (eqpSlot2, _) | eqpSlot2 == eqpSlot -> True
        _ -> False
      slotIs = filter f is
  in strongestSlotNoFilter eqpSlot slotIs

sumSlotNoFilter :: EqpSlot -> AspectRecord -> Int
sumSlotNoFilter eqpSlot AspectRecord{..} =
  case eqpSlot of
    EqpSlotPeriodic -> assert `failure` eqpSlot
    EqpSlotTimeout -> aTimeout
    EqpSlotAddHurtMelee -> aHurtMelee
    EqpSlotAddHurtRanged -> aHurtRanged
    EqpSlotAddArmorMelee -> aArmorMelee
    EqpSlotAddArmorRanged -> aArmorRanged
    EqpSlotAddMaxHP -> aMaxHP
    EqpSlotAddMaxCalm -> aMaxCalm
    EqpSlotAddSpeed -> aSpeed
    EqpSlotAddSight -> aSight
    EqpSlotAddSmell -> aSmell
    EqpSlotAddShine -> aShine
    EqpSlotAddNocto -> aNocto
    EqpSlotWeapon -> assert `failure` eqpSlot
    EqpSlotAddAbility ab -> EM.findWithDefault 0 ab aAbility

unknownAspect :: (Aspect -> [Dice.Dice]) -> ItemFull -> Bool
unknownAspect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAspect=Nothing, itemKind=ItemKind{iaspects}} ->
      let unknown x = Dice.minDice x /= Dice.maxDice x
      in or $ concatMap (map unknown . f) iaspects
    _ -> False  -- we don't know if it affect the aspect, so we assume 0

unknownMelee :: [ItemFull] -> Bool
unknownMelee =
  let p (AddHurtMelee k) = [k]
      p _ = []
      f itemFull b = b || unknownAspect p itemFull
  in foldr f False

filterRecharging :: [Effect] -> [Effect]
filterRecharging effs =
  let getRechargingEffect :: Effect -> Maybe Effect
      getRechargingEffect e@Recharging{} = Just e
      getRechargingEffect _ = Nothing
  in mapMaybe getRechargingEffect effs

stripRecharging :: [Effect] -> [Effect]
stripRecharging effs =
  let getRechargingEffect :: Effect -> Maybe Effect
      getRechargingEffect (Recharging e) = Just e
      getRechargingEffect _ = Nothing
  in mapMaybe getRechargingEffect effs

stripOnSmash :: [Effect] -> [Effect]
stripOnSmash effs =
  let getOnSmashEffect :: Effect -> Maybe Effect
      getOnSmashEffect (OnSmash e) = Just e
      getOnSmashEffect _ = Nothing
  in mapMaybe getOnSmashEffect effs
