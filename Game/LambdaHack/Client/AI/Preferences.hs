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
    Effect.Dominate -> -200
    Effect.Impress -> -10
    Effect.CallFriend p -> p * 100
    Effect.Summon{} -> 1               -- may or may not spawn a friendly
    Effect.CreateItem p -> p * 20
    Effect.ApplyPerfume -> -10
    Effect.Burn p -> -(15 * p)       -- usually splash damage, etc.
    Effect.Blast p -> -(5 * p)       -- unreliable
    Effect.Ascend{} -> 1               -- change levels sensibly, in teams
    Effect.Escape{} -> 10000           -- AI wants to win; spawners to guard
    Effect.Paralyze p -> -10 * p
    Effect.InsertMove p -> 50 * p
    Effect.DropBestWeapon -> -50
    Effect.DropAllEqp False -> -80
    Effect.DropAllEqp True -> -100
    Effect.SendFlying _ _ -> -10  -- but useful on self sometimes, too
    Effect.PushActor _ _ -> -10  -- but useful on self sometimes, too
    Effect.PullActor _ _ -> -10
    Effect.Teleport p -> -5 * p  -- but useful on self sometimes
    Effect.ActivateAllEqp -> -100
    Effect.TimedAspect k asp -> k * aspectToBenefit cops b asp `div` 50

aspectToBenefit :: Kind.COps -> Actor -> Effect.Aspect Int -> Int
aspectToBenefit _cops _b asp =
  case asp of
    Effect.NoAspect -> 0
    Effect.ArmorMelee _ -> 5
    Effect.Haste p -> p * 5
    Effect.Regeneration p -> p
    Effect.Steadfastness p -> p
    Effect.Explode{} -> 2
