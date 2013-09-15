{-# LANGUAGE DeriveFunctor, DeriveGeneric, OverloadedStrings #-}
-- | Effects of content on other content. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Common.Effect
  ( Effect(..), effectTrav, effectToSuffix, effectToBenefit
  ) where

import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.Hashable as Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Utils.Assert

-- TODO: document each constructor
-- Effects of items, tiles, etc. The type argument represents power.
-- either as a random formula dependent on level, or as a final rolled value.
data Effect a =
    NoEffect
  | Heal Int
  | Hurt !RollDice a
  | Mindprobe Int       -- the @Int@ is a hack to send the result to clients
  | Dominate
  | CallFriend Int
  | Summon Int
  | CreateItem Int
  | ApplyPerfume
  | Regeneration a
  | Searching a
  | Ascend Int
  | Descend Int
  | Escape
  deriving (Show, Read, Eq, Ord, Generic, Functor)

instance Hashable.Hashable a => Hashable.Hashable (Effect a)

instance Binary a => Binary (Effect a)

-- TODO: Traversable?
-- | Transform an effect using a stateful function.
effectTrav :: Effect a -> (a -> St.State s b) -> St.State s (Effect b)
effectTrav NoEffect _ = return NoEffect
effectTrav (Heal p) _ = return $ Heal p
effectTrav (Hurt dice a) f = do
  b <- f a
  return $ Hurt dice b
effectTrav (Mindprobe x) _ = return $ Mindprobe x
effectTrav Dominate _ = return Dominate
effectTrav (CallFriend p) _ = return $ CallFriend p
effectTrav (Summon p) _ = return $ Summon p
effectTrav (CreateItem p) _ = return $ CreateItem p
effectTrav ApplyPerfume _ = return ApplyPerfume
effectTrav (Regeneration a) f = do
  b <- f a
  return $ Regeneration b
effectTrav (Searching a) f = do
  b <- f a
  return $ Searching b
effectTrav (Ascend p) _ = return $ Ascend p
effectTrav (Descend p) _ = return $ Descend p
effectTrav Escape _ = return Escape

-- | Suffix to append to a basic content name if the content causes the effect.
effectToSuff :: Effect a -> (a -> Text) -> Text
effectToSuff effect f =
  case St.evalState (effectTrav effect $ return . f) () of
    NoEffect -> ""
    Heal p | p > 0 -> "of healing" <> affixBonus p
    Heal 0 -> "of bloodletting"
    Heal p -> "of wounding" <> affixBonus p
    Hurt dice t -> "(" <> showT dice <> ")" <> t
    Mindprobe{} -> "of soul searching"
    Dominate -> "of domination"
    CallFriend p -> "of aid calling" <> affixPower p
    Summon p -> "of summoning" <> affixPower p
    CreateItem p -> "of item creation" <> affixPower p
    ApplyPerfume -> "of rose water"
    Regeneration t -> "of regeneration" <> t
    Searching t -> "of searching" <> t
    Ascend p -> "of ascending" <> affixPower p
    Descend p -> "of descending" <> affixPower p
    Escape -> "of escaping"

effectToSuffix :: Effect Int -> Text
effectToSuffix effect = effectToSuff effect affixBonus

affixPower :: Int -> Text
affixPower p = case compare p 1 of
  EQ -> ""
  LT -> assert `failure` p
  GT -> " (+" <> showT p <> ")"

affixBonus :: Int -> Text
affixBonus p = case compare p 0 of
  EQ -> ""
  LT -> " (" <> showT p <> ")"
  GT -> " (+" <> showT p <> ")"

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Effect Int -> Int
effectToBenefit NoEffect = 0
effectToBenefit (Heal p) = p * 10       -- TODO: depends on (maxhp - hp)
effectToBenefit (Hurt _ p) = -(p * 10)  -- TODO: dice ignored for now
effectToBenefit Mindprobe{} = 0         -- AI can't benefit yet
effectToBenefit Dominate = 1            -- hard to use; TODO: limit by IQ
effectToBenefit (CallFriend p) = p * 100
effectToBenefit Summon{} = 5            -- may or may not spawn a friendly
effectToBenefit (CreateItem p) = p * 20
effectToBenefit ApplyPerfume = 0
effectToBenefit Regeneration{} = 0      -- bigger benefit from carrying around
effectToBenefit Searching{} = 0         -- AI doesn't search yet
effectToBenefit Ascend{} = 5            -- AI can't choose levels smartly yet
effectToBenefit Descend{} = 20          -- but it prefers to hide deep down
effectToBenefit Escape = 100            -- hero AI wants to win ASAP, monster
                                        -- AI sits on the exit to block it
