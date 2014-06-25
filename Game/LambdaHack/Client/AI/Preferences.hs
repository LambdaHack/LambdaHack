-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( effAspToBenefit, effectToBenefit
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc

-- TODO: also take other ItemFeatures into account, e.g., splash damage.
-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Kind.COps -> Actor -> [ItemFull] -> Faction
                -> Effect.Effect Int -> Int
effectToBenefit cops b activeItems fact eff =
  let isHorror = isHorrorFact cops fact
  in case eff of
    Effect.NoEffect -> 0
    Effect.RefillHP p ->
      let hpMax = sumSlotNoFilter Effect.EqpSlotAddMaxHP activeItems
      in if p > 0
         then 10 * min p (hpMax - bhp b)
         else max (-99) (10 * p)
    Effect.Hurt d p -> -(min 99 $ 10 * p + round (10 * Dice.meanDice d))
    Effect.RefillCalm p ->
      let calmMax = sumSlotNoFilter Effect.EqpSlotAddMaxCalm activeItems
      in if p > 0
         then min p (calmMax - bcalm b)
         else max (-20) p
    Effect.Dominate -> -200
    Effect.Impress -> -10
    Effect.CallFriend p -> 20 * p
    Effect.Summon{} | isHorror -> 1    -- probably generates friends or crazies
    Effect.Summon{} -> 0               -- probably generates enemies
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
    Effect.Teleport p -> -5 * p  -- but useful on self sometimes
    Effect.ActivateEqp ' ' -> -100
    Effect.ActivateEqp _ -> -50
    Effect.Explode _ -> -10
    Effect.OnSmash _ -> -10
    Effect.TimedAspect k asp -> k * fst (aspectToBenefit cops b asp) `div` 50

-- | Return the value to add to effect value and another to multiply it.
aspectToBenefit :: Kind.COps -> Actor -> Effect.Aspect Int -> (Int, Int)
aspectToBenefit _cops _b asp =
  case asp of
    Effect.Periodic n -> (0, n)
    Effect.AddMaxHP p -> (p * 10, 1)
    Effect.AddMaxCalm p -> (p, 1)
    Effect.AddSpeed p -> (p * 10000, 1)
    Effect.AddSkills m -> (5 * sum (EM.elems m), 1)
    Effect.AddArmorMelee p -> (p `divUp` 10, 1)
    Effect.AddSight p -> (p * 10, 1)
    Effect.AddSmell p -> (p * 2, 1)
    Effect.AddLight p -> (p * 10, 1)

effAspToBenefit :: Kind.COps -> Actor -> [ItemFull] -> Faction
                -> [Effect.Effect Int] -> [Effect.Aspect Int]
                -> Int
effAspToBenefit cops b activeItems fact effects aspects =
  let eBens = map (effectToBenefit cops b activeItems fact) effects
      (addBens, multBens) = unzip $ map (aspectToBenefit cops b) aspects
      addBen = sum addBens
      multBen = product multBens
      scaledBen = map (* multBen) $ addBen : eBens
  in if minimum scaledBen < -10 && maximum scaledBen > 10
     then 0  -- this is big deal mixed blessing, AI too stupid to decide
     else sum scaledBen
