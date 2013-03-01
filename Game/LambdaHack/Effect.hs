{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- | Effects of content on other content. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Effect
  ( Effect(..), effectToSuffix, effectToBenefit
  ) where

import Data.Binary
import qualified Data.Hashable as Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Msg
import Game.LambdaHack.Random

-- TODO: document each constructor
data Effect =
    NoEffect
  | Heal Int
  | Hurt !RollDice Int
  | Mindprobe Int       -- the @Int@ is a hack to send the result to clients
  | Dominate
  | SummonFriend Int
  | SpawnMonster Int
  | CreateItem Int
  | ApplyPerfume
  | Regeneration Int
  | Searching Int
  | Ascend Int
  | Descend Int
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable.Hashable Effect

instance Binary Effect

-- | Suffix to append to a basic content name, if the content causes the effect.
effectToSuffix :: Effect -> Text
effectToSuffix NoEffect = ""
effectToSuffix (Heal p) | p > 0 = "of healing" <> affixPower p
effectToSuffix (Heal 0) = "of bloodletting"
effectToSuffix (Heal p) = "of wounding" <> affixPower p
effectToSuffix (Hurt dice p) = "(" <> showT dice <> ")" <> affixPower p
effectToSuffix Mindprobe{} = "of soul searching"
effectToSuffix Dominate = "of domination"
effectToSuffix (SummonFriend p) = "of aid calling" <> affixPower p
effectToSuffix (SpawnMonster p) = "of spawning" <> affixPower p
effectToSuffix (CreateItem p) = "of item creation" <> affixPower p
effectToSuffix ApplyPerfume = "of rose water"
effectToSuffix (Regeneration p) = "of regeneration" <> affixPower p
effectToSuffix (Searching p) = "of searching" <> affixPower p
effectToSuffix (Ascend p) = "of ascending" <> affixPower p
effectToSuffix (Descend p) = "of descending" <> affixPower p

affixPower :: Int -> Text
affixPower p = case compare p 0 of
  EQ -> ""
  LT -> " (" <> showT p <> ")"
  GT -> " (+" <> showT p <> ")"

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Effect -> Int
effectToBenefit NoEffect = 0
effectToBenefit (Heal p) = p * 10         -- TODO: depends on (maxhp - hp)
effectToBenefit (Hurt _ p) = -(p * 10)  -- TODO: dice ignored for now
effectToBenefit Mindprobe{} = 0         -- AI can't benefit yet
effectToBenefit Dominate = 1            -- hard to use; TODO: limit by IQ
effectToBenefit (SummonFriend p) = p * 100
effectToBenefit SpawnMonster{} = 5      -- may or may not spawn a friendly
effectToBenefit (CreateItem p) = p * 20
effectToBenefit ApplyPerfume = 0
effectToBenefit Regeneration{} = 0      -- bigger benefit from carrying around
effectToBenefit Searching{} = 0         -- AI doesn't search yet
effectToBenefit Ascend{} = 1            -- AI can't choose levels smartly yet
effectToBenefit Descend{} = 1
