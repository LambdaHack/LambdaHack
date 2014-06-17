-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthAspect, strengthEffect, strengthFeature
  , strengthMelee, strengthPeriodic, strengthArmor
  , strengthSightRadius, strengthSmellRadius
  , strengthLight, strengthToThrow, strengthEqpSlot
  , strongestItem, strengthFromEqpSlot, strongestSlotNoFilter, strongestSlot
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  , unknownPrecious, permittedRanged
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import Data.Text (Text)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind

dice999 :: Dice.Dice -> Int
dice999 d = if Dice.minDice d == Dice.maxDice d
            then Dice.minDice d
            else 999

strengthAspect :: (Aspect Int -> [b]) -> ItemFull -> [b]
strengthAspect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jaspects}} ->
      concatMap f jaspects
    Just ItemDisco{itemKind=ItemKind{iaspects}} ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (aspectTrav x (return . dice999)) ()
      in concatMap f $ map trav iaspects
    Nothing -> []

strengthAspectMaybe :: Show b => (Aspect Int -> [b]) -> ItemFull -> Maybe b
strengthAspectMaybe f itemFull =
  case strengthAspect f itemFull of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, itemFull)

strengthEffect :: (Effect Int -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
      concatMap f jeffects
    Just ItemDisco{itemKind=ItemKind{ieffects}} ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (effectTrav x (return . dice999)) ()
      in concatMap f $ map trav ieffects
    Nothing -> []

strengthFeature :: (IF.Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

strengthMelee :: ItemFull -> Maybe Int
strengthMelee itemFull =
  let durable = IF.Durable `elem` jfeature (itemBase itemFull)
      p (Hurt d k) = [floor (Dice.meanDice d) + k]
      p _ = []
      hasNoEffects = case itemDisco itemFull of
        Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
          null jeffects
        Just ItemDisco{itemKind=ItemKind{ieffects}} ->
          null ieffects
        Nothing -> True
  in if hasNoEffects
     then Nothing
     else Just $ sum (strengthEffect p itemFull)
                 + if durable then 1000000 else 0

strengthPeriodic :: ItemFull -> Maybe Int
strengthPeriodic =
  let p (Periodic k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthArmor :: ItemFull -> Maybe Int
strengthArmor =
  let p (ArmorMelee k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthSightRadius :: ItemFull -> Maybe Int
strengthSightRadius =
  let p (SightRadius k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthSmellRadius :: ItemFull -> Maybe Int
strengthSmellRadius =
  let p (SmellRadius k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthLight :: Item -> Maybe Int
strengthLight item =
  let p (IF.Light k) = [k]
      p _ = []
  in case strengthFeature p item of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, item)

strengthEqpSlot :: Item -> Maybe (IF.EqpSlot, Text)
strengthEqpSlot item =
  let p (IF.EqpSlot eqpSlot t) = [(eqpSlot, t)]
      p _ = []
  in case strengthFeature p item of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, item)

strengthToThrow :: Item -> ThrowMod Int
strengthToThrow item =
  let p (IF.ToThrow tmod) = [tmod]
      p _ = []
  in case strengthFeature p item of
    [] -> ThrowMod 100 100
    [x] -> x
    xs -> assert `failure` (xs, item)

totalRange :: Item -> Int
totalRange item =
  let ThrowMod{..} = strengthToThrow item
      speed = speedFromWeight (jweight item) throwVelocity
  in rangeFromSpeedAndLinger speed throwLinger

computeTrajectory :: Int -> Int -> Int -> [Point] -> ([Vector], Speed)
computeTrajectory weight throwVelocity throwLinger path =
  let speed = speedFromWeight weight throwVelocity
      trange = rangeFromSpeedAndLinger speed throwLinger
      btrajectory = take trange $ pathToTrajectory path
  in (btrajectory, speed)

itemTrajectory :: Item -> [Point] -> ([Vector], Speed)
itemTrajectory item path =
  let ThrowMod{..} = strengthToThrow item
  in computeTrajectory (jweight item) throwVelocity throwLinger path

strongestItem :: Ord b => Bool -> [(ItemId, ItemFull)] -> (ItemFull -> Maybe b)
              -> [(b, (ItemId, ItemFull))]
strongestItem onlyOn is p =
  let pv (iid, item) = (\v -> (v, (iid, item))) <$> (p item)
      onlyIs = if onlyOn then filter (itemIsOn . snd) is else is
      pis = mapMaybe pv onlyIs
  in sortBy (flip $ Ord.comparing fst) pis

strengthFromEqpSlot :: IF.EqpSlot -> ItemFull -> Maybe Int
strengthFromEqpSlot eqpSlot =
  case eqpSlot of
    IF.EqpSlotArmorMelee -> strengthArmor
    IF.EqpSlotSightRadius -> strengthSightRadius
    IF.EqpSlotSmellRadius -> strengthSmellRadius
    IF.EqpSlotLight -> strengthLight . itemBase
    IF.EqpSlotWeapon -> strengthMelee

strongestSlotNoFilter :: IF.EqpSlot -> Bool -> [(ItemId, ItemFull)]
                      -> [(Int, (ItemId, ItemFull))]
strongestSlotNoFilter eqpSlot onlyOn is =
  strongestItem onlyOn is $ strengthFromEqpSlot eqpSlot

strongestSlot :: IF.EqpSlot -> Bool -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot eqpSlot onlyOn is =
  let f (_, itemFull) = case strengthEqpSlot $ itemBase itemFull of
        Just (eqpSlot2, _) | eqpSlot2 == eqpSlot -> True
        _ -> False
      slotIs = filter f is
  in strongestSlotNoFilter eqpSlot onlyOn slotIs

unknownPrecious :: ItemFull -> Bool
unknownPrecious itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just _} -> False
    _ -> IF.Precious `elem` jfeature (itemBase itemFull)

permittedRanged :: ItemFull -> Bool
permittedRanged itemFull = not (unknownPrecious itemFull)
                           && case strengthEqpSlot (itemBase itemFull) of
                                Just (IF.EqpSlotLight, _) -> True
                                Just _ -> False
                                Nothing -> True
