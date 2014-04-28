-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( effectToBenefit
  ) where

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Kind as Kind
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
                     else max (-99) (10 * p)  -- usually splash damage
    Effect.Hurt d p -> -(min 99 $ 5 * p + round (5 * Dice.meanDice d))
    Effect.Mindprobe{} -> 0            -- AI can't benefit yet
    Effect.Dominate -> -100
    Effect.Impress -> -10
    Effect.CallFriend p -> p * 100
    Effect.Summon{} -> 1               -- may or may not spawn a friendly
    Effect.CreateItem p -> p * 20
    Effect.ApplyPerfume -> -10
    Effect.Burn p -> -(15 * p)       -- usually splash damage, etc.
    Effect.Blast p -> -(5 * p)       -- unreliable
    Effect.Ascend{} -> 1               -- change levels sensibly, in teams
    Effect.Escape{} -> 10000           -- AI wants to win; spawners to guard
    Effect.TimedAspect k asp -> k * aspectToBenefit cops b asp `div` 50

aspectToBenefit :: Kind.COps -> Actor -> Effect.Aspect Int -> Int
aspectToBenefit _cops _b asp =
  case asp of
    Effect.NoAspect -> 0
    Effect.ArmorMelee _ -> 5
    Effect.Haste p -> p * 5
    Effect.Light p -> p
    Effect.Regeneration p -> p
    Effect.Steadfastness p -> p
