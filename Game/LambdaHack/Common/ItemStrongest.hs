-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthAspect, strengthEffect, strengthFeature
  , strengthMelee, strengthPeriodic, strengthArmor
  , strengthSightRadius, strengthSmellRadius, strengthIntelligence
  , strengthLight, strengthToThrow, strengthEqpSlot, isFragile
  , strongestItem, strongestSword, strongestShield
  , strongestSightRadius, strongestSmellRadius, strongestIntelligence
  , strongestLight
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory, strengthFromAspect
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.List
import qualified Data.Ord as Ord
import Data.Text (Text)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind

strongestItem :: Ord b => Bool -> [(ItemId, ItemFull)] -> (ItemFull -> [b])
              -> [(b, (ItemId, ItemFull))]
strongestItem onlyOn is p =
  let pv (iid, item) = map (\v -> (v, (iid, item))) (p item)
      onlyIs = if onlyOn then filter (itemIsOn . snd) is else is
      pis = concatMap pv onlyIs
  in sortBy (flip $ Ord.comparing fst) pis

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

strengthMelee :: Kind.COps -> ItemFull -> [Int]
strengthMelee Kind.COps{corule} itemFull =
  if jsymbol (itemBase itemFull) `elem` ritemMelee (Kind.stdRuleset corule)
  then let p (Hurt d k) = [floor (Dice.meanDice d) + k]
           p _ = []
       in strengthEffect p itemFull
  else []

strongestSword :: Kind.COps -> Bool -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestSword cops onlyOn is = strongestItem onlyOn is (strengthMelee cops)

strengthPeriodic :: ItemFull -> Maybe Int
strengthPeriodic itemFull =
  let p (Periodic k) = [k]
      p _ = []
  in case strengthAspect p itemFull of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, itemFull)

strengthArmor :: ItemFull -> [Int]
strengthArmor =
  let p (ArmorMelee k) = [k]
      p _ = []
  in strengthAspect p

strongestShield :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestShield onlyOn is = strongestItem onlyOn is strengthArmor

strengthSightRadius :: ItemFull -> [Int]
strengthSightRadius =
  let p (SightRadius k) = [k]
      p _ = []
  in strengthAspect p

strongestSightRadius :: Bool -> [(ItemId, ItemFull)]
                     -> [(Int, (ItemId, ItemFull))]
strongestSightRadius onlyOn is = strongestItem onlyOn is strengthSightRadius

strengthSmellRadius :: ItemFull -> [Int]
strengthSmellRadius =
  let p (SmellRadius k) = [k]
      p _ = []
  in strengthAspect p

strongestSmellRadius :: Bool -> [(ItemId, ItemFull)]
                     -> [(Int, (ItemId, ItemFull))]
strongestSmellRadius onlyOn is = strongestItem onlyOn is strengthSmellRadius

strengthIntelligence :: ItemFull -> [Int]
strengthIntelligence =
  let p (Intelligence k) = [k]
      p _ = []
  in strengthAspect p

strongestIntelligence :: Bool -> [(ItemId, ItemFull)]
                      -> [(Int, (ItemId, ItemFull))]
strongestIntelligence onlyOn is = strongestItem onlyOn is strengthIntelligence

strengthLight :: Item -> [Int]
strengthLight =
  let p (IF.Light k) = [k]
      p _ = []
  in strengthFeature p

strongestLight :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestLight onlyOn is = strongestItem onlyOn is (strengthLight . itemBase)

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

isFragile :: Item -> Bool
isFragile item =
  let p IF.Fragile = [()]
      p _ = []
  in case strengthFeature p item of
    [] -> False
    [()] -> True
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

strengthFromAspect :: Kind.COps -> IF.EqpSlot -> ItemFull -> [Int]
strengthFromAspect cops eqpSlot =
  case eqpSlot of
    IF.EqpSlotArmorMelee -> strengthArmor
    IF.EqpSlotSightRadius -> strengthSightRadius
    IF.EqpSlotSmellRadius -> strengthSmellRadius
    IF.EqpSlotLight -> strengthLight . itemBase
    IF.EqpSlotWeapon -> strengthMelee cops
