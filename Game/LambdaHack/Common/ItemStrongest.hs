-- | Determining the strongest item wrt some property.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.ItemStrongest
  ( -- * Strongest items
    strengthOnSmash, strengthCreateOrgan, strengthDropOrgan
  , strengthToThrow, strengthEqpSlot, strengthFromEqpSlot, strengthEffect
  , strongestSlotNoFilter, strongestSlot, sumSlotNoFilter, sumSkills
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  , unknownMelee, allRecharging, stripRecharging, stripOnSmash
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import Data.Text (Text)

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind

strengthAspect :: (Aspect Int -> [b]) -> ItemFull -> [b]
strengthAspect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jaspects}} ->
      concatMap f jaspects
    Just ItemDisco{itemKind=ItemKind{iaspects}} ->
      -- Approximation. For some effects lower values are better,
      -- so we can't put 999 here (and for summation, this is wrong).
      let trav x = St.evalState (aspectTrav x (return . Dice.meanDice))
                                ()
      in concatMap f $ map trav iaspects
    Nothing -> []

strengthAspectMaybe :: Show b => (Aspect Int -> [b]) -> ItemFull -> Maybe b
strengthAspectMaybe f itemFull =
  case strengthAspect f itemFull of
    [] -> Nothing
    [x] -> Just x
    xs -> assert `failure` (xs, itemFull)

strengthEffect :: (Effect -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
      concatMap f jeffects
    Just ItemDisco{itemKind=ItemKind{ieffects}} ->
      concatMap f ieffects
    Nothing -> []

strengthFeature :: (Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

-- Called only by the server, so 999 is OK.
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

strengthPeriodic :: ItemFull -> Maybe Int
strengthPeriodic itemFull =
  let p Periodic = [()]
      p _ = []
      isPeriodic = isJust $ strengthAspectMaybe p itemFull
      q (Timeout k) = [k]
      q _ = []
  in if isPeriodic then strengthAspectMaybe q itemFull else Nothing

strengthTimeout :: ItemFull -> Maybe Int
strengthTimeout =
  let p (Timeout k) = [k]
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

strengthAllAddSkills :: ItemFull -> Maybe Ability.Skills
strengthAllAddSkills =
  let p (AddSkills a) = [a]
      p _ = []
  in strengthAspectMaybe p

strengthAddSkills :: Ability.Ability -> ItemFull -> Maybe Int
strengthAddSkills ab =
  let p (AddSkills a) = [fromMaybe 0 $ EM.lookup ab a]
      p _ = []
  in strengthAspectMaybe p

strengthAddHurtMelee :: ItemFull -> Maybe Int
strengthAddHurtMelee =
  let p (AddHurtMelee k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddHurtRanged :: ItemFull -> Maybe Int
strengthAddHurtRanged =
  let p (AddHurtRanged k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddArmorMelee :: ItemFull -> Maybe Int
strengthAddArmorMelee =
  let p (AddArmorMelee k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddArmorRanged :: ItemFull -> Maybe Int
strengthAddArmorRanged =
  let p (AddArmorRanged k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddSight :: ItemFull -> Maybe Int
strengthAddSight =
  let p (AddSight k) = [k]
      p _ = []
  in strengthAspectMaybe p

strengthAddSmell :: ItemFull -> Maybe Int
strengthAddSmell =
  let p (AddSmell k) = [k]
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

-- TODO: when all below are aspects, define with
-- (EqpSlotAddMaxHP, AddMaxHP k) -> [k]
strengthFromEqpSlot :: EqpSlot -> ItemFull -> Maybe Int
strengthFromEqpSlot eqpSlot =
  case eqpSlot of
    EqpSlotPeriodic -> strengthPeriodic
    EqpSlotTimeout -> strengthTimeout
    EqpSlotAddMaxHP -> strengthAddMaxHP
    EqpSlotAddMaxCalm -> strengthAddMaxCalm
    EqpSlotAddSpeed -> strengthAddSpeed
    EqpSlotAddSkills ab -> strengthAddSkills ab
    EqpSlotAddHurtMelee -> strengthAddHurtMelee
    EqpSlotAddHurtRanged -> strengthAddHurtRanged
    EqpSlotAddArmorMelee -> strengthAddArmorMelee
    EqpSlotAddArmorRanged -> strengthAddArmorRanged
    EqpSlotAddSight -> strengthAddSight
    EqpSlotAddSmell -> strengthAddSmell
    EqpSlotAddLight -> strengthAddLight

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

sumSlotNoFilter :: EqpSlot -> [ItemFull] -> Int
sumSlotNoFilter eqpSlot is =
  let f = strengthFromEqpSlot eqpSlot
      g itemFull = (* itemK itemFull) <$> f itemFull
  in sum $ mapMaybe g is

sumSkills :: [ItemFull] -> Ability.Skills
sumSkills is =
  let g itemFull = (Ability.scaleSkills (itemK itemFull))
                   <$> strengthAllAddSkills itemFull
  in foldr Ability.addSkills Ability.zeroSkills $ mapMaybe g is

unknownAspect :: (Aspect Dice.Dice -> [Dice.Dice]) -> ItemFull -> Bool
unknownAspect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE=Nothing, itemKind=ItemKind{iaspects}} ->
      let unknown x = Dice.minDice x /= Dice.maxDice x
      in or $ concatMap (map unknown . f) iaspects
    _ -> False

unknownMelee :: [ItemFull] -> Bool
unknownMelee =
  let p (AddHurtMelee k) = [k]
      p _ = []
      f itemFull b = b || unknownAspect p itemFull
  in foldr f False

allRecharging :: [Effect] -> [Effect]
allRecharging effs =
  let getRechargingEffect :: Effect -> Maybe (Effect)
      getRechargingEffect e@Recharging{} = Just e
      getRechargingEffect _ = Nothing
  in mapMaybe getRechargingEffect effs

stripRecharging :: [Effect] -> [Effect]
stripRecharging effs =
  let getRechargingEffect :: Effect -> Maybe (Effect)
      getRechargingEffect (Recharging e) = Just e
      getRechargingEffect _ = Nothing
  in mapMaybe getRechargingEffect effs

stripOnSmash :: [Effect] -> [Effect]
stripOnSmash effs =
  let getOnSmashEffect :: Effect -> Maybe (Effect)
      getOnSmashEffect (OnSmash e) = Just e
      getOnSmashEffect _ = Nothing
  in mapMaybe getOnSmashEffect effs
