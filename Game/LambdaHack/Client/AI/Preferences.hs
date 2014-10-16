-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( totalUsefulness, effectToBenefit
  ) where

import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.Maybe

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Kind.COps -> Actor -> [ItemFull] -> Faction
                -> Effect.Effect Int -> Int
effectToBenefit cops b activeItems fact eff =
  let dungeonDweller = not $ fcanEscape $ gplayer fact
  in case eff of
    Effect.NoEffect _ -> 0
    Effect.RefillHP p ->
      let hpMax = sumSlotNoFilter Effect.EqpSlotAddMaxHP activeItems
      in if p > 0
         then 1 + 10 * min p (fromIntegral $ (xM hpMax - bhp b) `divUp` oneM)
         else max (-99) (10 * p)
    Effect.Hurt d -> -(min 99 $ round (10 * Dice.meanDice d))
    Effect.RefillCalm p ->
      let calmMax = sumSlotNoFilter Effect.EqpSlotAddMaxCalm activeItems
      in if p > 0
         then 1 + min p (fromIntegral $ (xM calmMax - bcalm b) `divUp` oneM)
         else max (-20) p
    Effect.Dominate -> -200
    Effect.Impress -> -10
    Effect.CallFriend p -> 20 * p
    Effect.Summon{} | dungeonDweller -> 1 -- probably summons friends or crazies
    Effect.Summon{} -> 0                  -- probably generates enemies
    Effect.CreateItem p -> 20 * p
    Effect.ApplyPerfume -> -10
    Effect.Burn p -> -15 * p           -- usually splash damage, etc.
    Effect.Ascend{} -> 1               -- change levels sensibly, in teams
    Effect.Escape{} -> 10000           -- AI wants to win; spawners to guard
    Effect.Paralyze p -> -20 * p
    Effect.InsertMove p -> 50 * p
    Effect.DropBestWeapon -> -50
    Effect.DropEqp ' ' False -> -80
    Effect.DropEqp ' ' True -> -100
    Effect.DropEqp _ False -> -40
    Effect.DropEqp _ True -> -50
    Effect.SendFlying _ -> -10  -- but useful on self sometimes, too
    Effect.PushActor _ -> -10  -- but useful on self sometimes, too
    Effect.PullActor _ -> -10
    Effect.Teleport p | p <= 9 -> 10  -- blink to shoot at foe
    Effect.Teleport p | p <= 19 -> 1  -- neither escape nor repositioning
    Effect.Teleport p -> -5 * p  -- get rid of the foe
    Effect.PolyItem _ -> 0  -- AI would loop
    Effect.Identify _ -> 0  -- AI would loop
    Effect.ActivateInv ' ' -> -100
    Effect.ActivateInv _ -> -50
    Effect.Explode _ -> -10
    Effect.OneOf _ -> 1  -- usually a mixed blessing, but slightly beneficial
    Effect.OnSmash _ -> -10
    Effect.Recharging e -> effectToBenefit cops b activeItems fact e
                           `div` 3  -- TODO: use Timeout
    Effect.TimedAspect k asp -> k * (aspectToBenefit cops b asp) `div` 50

-- | Return the value to add to effect value and another to multiply it.
aspectToBenefit :: Kind.COps -> Actor -> Effect.Aspect Int -> Int
aspectToBenefit _cops _b asp =
  case asp of
    Effect.Periodic{} -> 0
    Effect.Timeout{} -> 0
    Effect.AddMaxHP p -> p * 10
    Effect.AddMaxCalm p -> p `divUp` 2
    Effect.AddSpeed p -> p * 10000
    Effect.AddSkills m -> 5 * sum (EM.elems m)
    Effect.AddHurtMelee p -> p `divUp` 3
    Effect.AddHurtRanged p -> p `divUp` 5
    Effect.AddArmorMelee p -> p `divUp` 5
    Effect.AddArmorRanged p -> p `divUp` 10
    Effect.AddSight p -> p * 10
    Effect.AddSmell p -> p * 2
    Effect.AddLight p -> p * 10

-- | Determine the total benefit from having an item in eqp or inv,
-- according to item type, and also the benefit confered by equipping the item
-- and from meleeing with it or applying it or throwing it.
totalUsefulness :: Kind.COps -> Actor -> [ItemFull] -> Faction -> ItemFull
                -> Maybe (Int, (Int, Int))
totalUsefulness cops b activeItems fact itemFull =
  let ben effects aspects =
        let effBens = map (effectToBenefit cops b activeItems fact) effects
            aspBens = map (aspectToBenefit cops b) aspects
            periodicEffBens = map (effectToBenefit cops b activeItems fact)
                                  (allRecharging effects)
            periodicBens =
              case strengthFromEqpSlot Effect.EqpSlotPeriodic itemFull of
                Nothing -> []
                Just timeout ->
                  map (\eff -> eff * 10 `div` timeout) periodicEffBens
            selfBens = aspBens ++ periodicBens
            eqpSum = if not (null selfBens) && minimum selfBens < -10
                                            && maximum selfBens > 10
                     then 0  -- significant mixed blessings out of AI control
                     else sum selfBens
            effSum = sum effBens
            isWeapon =
              isJust (strengthFromEqpSlot Effect.EqpSlotWeapon itemFull)
            totalSum = if goesIntoInv $ itemBase itemFull
                       then effSum
                       else if isWeapon
                            then effSum + eqpSum
                            else eqpSum
        in (totalSum, (eqpSum, effSum))
  in case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jaspects, jeffects}} ->
      Just $ ben jeffects jaspects
    Just ItemDisco{itemKind=ItemKind{iaspects, ieffects}} ->
      let travA x =
            St.evalState (Effect.aspectTrav x (return . round . Dice.meanDice))
                         ()
          jaspects = map travA iaspects
          travE x =
            St.evalState (Effect.effectTrav x (return . round . Dice.meanDice))
                         ()
          jeffects = map travE ieffects
      in Just $ ben jeffects jaspects
    _ -> Nothing
