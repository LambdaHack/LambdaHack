-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthAspect, strengthEffect, strengthFeature
  , strengthToThrow, strengthEqpSlot, strengthFromEqpSlot
  , strongestSlotNoFilter, strongestSlot, sumSlotNoFilter
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  , unknownPrecious, permittedRanged
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import Data.Text (Text)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Item
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

strengthFeature :: (Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

strengthMelee :: ItemFull -> Maybe Int
strengthMelee itemFull =
  let durable = Durable `elem` jfeature (itemBase itemFull)
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

strengthAddMaxHP :: ItemFull -> Maybe Int
strengthAddMaxHP =
  let p (AddMaxHP k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddMaxCalm :: ItemFull -> Maybe Int
strengthAddMaxCalm =
  let p (AddMaxCalm k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddSpeed :: ItemFull -> Maybe Int
strengthAddSpeed =
  let p (AddSpeed k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAbility :: ItemFull -> Maybe Int
strengthAbility itemFull =
  let p (AddSkills a) = [a]
      p _ = []
  in sum . EM.elems <$> strengthAspectMaybe p itemFull

strengthArmorMelee :: ItemFull -> Maybe Int
strengthArmorMelee =
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

strengthAddLight :: ItemFull -> Maybe Int
strengthAddLight =
  let p (AddLight k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthEqpSlot :: Item -> Maybe (EqpSlot, Text)
strengthEqpSlot item =
  let p (EqpSlot eqpSlot t) = [(eqpSlot, t)]
      p _ = []
  in case strengthFeature p item of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, item)

strengthToThrow :: Item -> ThrowMod Int
strengthToThrow item =
  let p (ToThrow tmod) = [tmod]
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

-- TODO: when all below are apsects, define with
-- (EqpSlotAddMaxHP, AddMaxHP k) -> [k]
strengthFromEqpSlot :: EqpSlot -> ItemFull -> Maybe Int
strengthFromEqpSlot eqpSlot =
  case eqpSlot of
    EqpSlotPeriodic -> strengthPeriodic  -- a very crude approximation
    EqpSlotAddMaxHP -> strengthAddMaxHP
    EqpSlotAddMaxCalm -> strengthAddMaxCalm
    EqpSlotAddSpeed -> strengthAddSpeed
    EqpSlotAbility -> strengthAbility
    EqpSlotArmorMelee -> strengthArmorMelee  -- a very crude approximation
    EqpSlotSightRadius -> strengthSightRadius
    EqpSlotSmellRadius -> strengthSmellRadius
    EqpSlotAddLight -> strengthAddLight
    EqpSlotWeapon -> strengthMelee

strongestSlotNoFilter :: EqpSlot -> [(ItemId, ItemFull)]
                      -> [(Int, (ItemId, ItemFull))]
strongestSlotNoFilter eqpSlot is =
  let f = strengthFromEqpSlot eqpSlot
      g (iid, itemFull) = (\v -> (v, (iid, itemFull))) <$> (f itemFull)
  in sortBy (flip $ Ord.comparing fst) $ mapMaybe g is

strongestSlot :: EqpSlot -> [(ItemId, ItemFull)]
              -> [(Int, (ItemId, ItemFull))]
strongestSlot eqpSlot is =
  let f (_, itemFull) = case strengthEqpSlot $ itemBase itemFull of
        Just (eqpSlot2, _) | eqpSlot2 == eqpSlot -> True
        _ -> False
      slotIs = filter f is
  in strongestSlotNoFilter eqpSlot slotIs

sumSlotNoFilter :: EqpSlot -> [(ItemId, ItemFull)] -> Int
sumSlotNoFilter eqpSlot is =
  let f = strengthFromEqpSlot eqpSlot
      g (_, itemFull) = (* itemK itemFull) <$> f itemFull
  in sum $ mapMaybe g is

unknownPrecious :: ItemFull -> Bool
unknownPrecious itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just _} -> False
    _ -> Precious `elem` jfeature (itemBase itemFull)

permittedRanged :: ItemFull -> Bool
permittedRanged itemFull = not (unknownPrecious itemFull)
                           && case strengthEqpSlot (itemBase itemFull) of
                                Just (EqpSlotAddLight, _) -> True
                                Just _ -> False
                                Nothing -> True
