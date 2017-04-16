-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthOnSmash, strengthCreateOrgan, strengthDropOrgan
  , strengthEqpSlot, strengthToThrow, strengthEffect, strongestSlot
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  , unknownMelee, filterRecharging, stripRecharging, stripOnSmash
  , hasCharge, damageUsefulness, strongestMelee, prEqpSlot
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Ord as Ord

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind

strengthEffect :: (Effect -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemKind=ItemKind{ieffects}} ->
      concatMap f ieffects
    Nothing -> []

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
  let p (DropItem _ _ COrgan grp) = [grp]
      p (Recharging (DropItem _ _ COrgan grp)) = [grp]
      p _ = []
  in strengthEffect p

strengthEqpSlot :: ItemFull -> Maybe EqpSlot
strengthEqpSlot item =
  let p (EqpSlot eqpSlot) = [eqpSlot]
      p _ = []
  in case strengthEffect p item of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, item)

strengthToThrow :: Item -> ThrowMod
strengthToThrow item =
  let p (ToThrow tmod) = [tmod]
      p _ = []
  in case concatMap p (jfeature item) of
    [] -> ThrowMod 100 100
    [x] -> x
    xs -> assert `failure` (xs, item)

computeTrajectory :: Int -> Int -> Int -> [Point] -> ([Vector], (Speed, Int))
computeTrajectory weight throwVelocity throwLinger path =
  let speed = speedFromWeight weight throwVelocity
      trange = rangeFromSpeedAndLinger speed throwLinger
      btrajectory = pathToTrajectory $ take trange path
  in (btrajectory, (speed, trange))

itemTrajectory :: Item -> [Point] -> ([Vector], (Speed, Int))
itemTrajectory item path =
  let ThrowMod{..} = strengthToThrow item
  in computeTrajectory (jweight item) throwVelocity throwLinger path

totalRange :: Item -> Int
totalRange item = snd $ snd $ itemTrajectory item []

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
    EqpSlotWeapon -> assert `failure` ar
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

hasCharge :: Time -> ItemFull -> Bool
hasCharge localTime itemFull@ItemFull{..} =
  let timeout = aTimeout $ aspectRecordFull itemFull
      timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
      charging startT = timeShift startT timeoutTurns > localTime
      it1 = filter charging itemTimer
  in length it1 < itemK

damageUsefulness :: Item -> Int
damageUsefulness item = min 150 (10 * Dice.meanDice (jdamage item))

-- We assume extra weapon effects are useful and so such
-- weapons are preferred over weapons with no effects.
-- If the player doesn't like a particular weapon's extra effect,
-- he has to manage this manually.
strongestMelee :: Maybe DiscoveryBenefit -> Time -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestMelee _ _ [] = []
strongestMelee mdiscoBenefit localTime is =
  -- For simplicity we assume, if weapon not recharged, all important effects
  -- are disabled and only raw damage remains.
  let f (iid, itemFull) =
        let rawDmg = (damageUsefulness $ itemBase itemFull, (iid, itemFull))
        in case mdiscoBenefit of
          Just discoBenefit | hasCharge localTime itemFull ->
            -- For fighting, as opposed to equipping, we value weapon
            -- only for its raw damage and harming effects.
            case EM.lookup iid discoBenefit of
              Just (_, (_, effFoe)) -> ( if effFoe < 0 then - effFoe else 0
                                       , (iid, itemFull) )
              Nothing -> rawDmg
          _  -> rawDmg
  in sortBy (flip $ Ord.comparing fst) $ map f is

-- This ignores items that don't go into equipment, as determined in @inEqp@.
-- They are removed from equipment elsewhere vie @harmful@.
strongestSlot :: DiscoveryBenefit -> EqpSlot -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot discoBenefit eqpSlot is =
  let f (iid, itemFull) =
        let rawDmg = (damageUsefulness $ itemBase itemFull, (iid, itemFull))
        in if eqpSlot == EqpSlotWeapon
           then case EM.lookup iid discoBenefit of
             Just ((pickupSum, inEqp), (_, _)) ->
               if not inEqp then Nothing else Just (pickupSum, (iid, itemFull))
             Nothing -> Just rawDmg
           else let inEqp = case EM.lookup iid discoBenefit of
                      Just ((_, i), (_, _)) -> i
                      Nothing -> goesIntoEqp (itemBase itemFull)
                in if not inEqp then Nothing
                   else Just ( prEqpSlot eqpSlot $ aspectRecordFull itemFull
                             , (iid, itemFull) )
  in sortBy (flip $ Ord.comparing fst) $ mapMaybe f is

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
