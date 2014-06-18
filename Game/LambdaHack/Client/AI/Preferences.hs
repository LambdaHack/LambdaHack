-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( effAspToBenefit, effectToBenefit
  ) where

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ActorKind

-- TODO: also take other ItemFeatures into account, e.g., splash damage.
-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Kind.COps -> Actor -> Effect.Effect Int -> Int
effectToBenefit cops@Kind.COps{coactor=Kind.Ops{okind}} b eff =
  let kind = okind $ bkind b
  in case eff of
    Effect.NoEffect -> 0
    Effect.Heal p -> if p > 0
                     then 10 * min p (Dice.maxDice (ahp kind) - bhp b)
                     else max (-99) (10 * p)
    Effect.Hurt d p -> -(min 99 $ 10 * p + round (10 * Dice.meanDice d))
    Effect.Calm p -> if p > 0
                     then min p (Dice.maxDice (acalm kind) - bcalm b)
                     else max (-20) p
    Effect.Dominate -> -200
    Effect.Impress -> -10
    Effect.CallFriend p -> 100 * p
    Effect.Summon{} -> -1              -- may or may not spawn a friendly
    Effect.CreateItem p -> 20 * p
    Effect.ApplyPerfume -> -10
    Effect.Burn p -> -15 * p           -- usually splash damage, etc.
    Effect.Blast p -> -5 * p           -- unreliable
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
    Effect.TimedAspect k asp -> k * fst (aspectToBenefit cops b asp) `div` 50

-- | Return the value to add to effect value and another to multiply it.
aspectToBenefit :: Kind.COps -> Actor -> Effect.Aspect Int -> (Int, Int)
aspectToBenefit _cops _b asp =
  case asp of
    Effect.NoAspect -> (0, 1)
    Effect.Periodic n -> (0, n)
    Effect.AddMaxHP p -> (p * 10, 1)
    Effect.AddMaxCalm p -> (p, 1)
    Effect.AddSpeed p -> (p * 10000, 1)
    Effect.AddAbility _ -> (50, 1)
    Effect.DeleteAbility _ -> (-50, 1)
    Effect.ArmorMelee p -> (p `divUp` 10, 1)
    Effect.Explode{} -> (0, 10)
    Effect.SightRadius p -> (p * 20, 1)
    Effect.SmellRadius p -> (p * 2, 1)

effAspToBenefit :: Kind.COps -> Actor
                -> [Effect.Effect Int] -> [Effect.Aspect Int]
                -> Int
effAspToBenefit cops b effects aspects =
  let eBens = map (effectToBenefit cops b) effects
      (addBens, multBens) = unzip $ map (aspectToBenefit cops b) aspects
      addBen = sum addBens
      multBen = product multBens
      scaledBen = map (* multBen) $ addBen : eBens
  in if minimum scaledBen < -10 && maximum scaledBen > 10
     then 0  -- this is big deal mixed blessing, AI too stupid to decide
     else sum scaledBen
