-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( effectToBenefit
  ) where

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified Game.LambdaHack.Common.Random as Random
import Game.LambdaHack.Content.ActorKind

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Kind.COps -> Actor -> Effect.Effect Int -> Int
effectToBenefit Kind.COps{coactor=Kind.Ops{okind}} b eff =
  let kind = okind $ bkind b
  in case eff of
    Effect.NoEffect -> 0
    (Effect.Heal p) -> 10 * min p (Random.maxDice (ahp kind) - bhp b)
    (Effect.Hurt _ p) -> -(p * 10)     -- TODO: dice ignored, not capped
    Effect.Mindprobe{} -> 0            -- AI can't benefit yet
    Effect.Dominate -> -100
    (Effect.CallFriend p) -> p * 100
    Effect.Summon{} -> 1               -- may or may not spawn a friendly
    (Effect.CreateItem p) -> p * 20
    Effect.ApplyPerfume -> 1           -- TODO: tweak vs smell mechanics
    Effect.Regeneration p -> p
    Effect.Searching{} -> 0
    Effect.Ascend{} -> 1               -- change levels sensibly, in teams
    Effect.Escape{} -> 10000           -- AI wants to win; spawners to guard
