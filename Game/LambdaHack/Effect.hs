{-# LANGUAGE OverloadedStrings #-}
-- | Effects of content on other content. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Effect
  ( Effect(..), effectToSuffix, effectToBenefit
  ) where

import Data.Text (Text)

import Game.LambdaHack.Msg
import Game.LambdaHack.Random

-- TODO: document each constructor
-- | All possible effects, some of them parameterized or dependent
-- on outside coefficients, e.g., item power.
data Effect =
    NoEffect
  | Heal             -- healing strength in ipower
  | Wound !RollDice  -- base damage, to-dam bonus in ipower
  | Mindprobe
  | Dominate
  | SummonFriend
  | SpawnMonster
  | CreateItem
  | ApplyPerfume
  | Regeneration
  | Searching
  | Ascend
  | Descend
  deriving (Show, Read, Eq, Ord)

-- | Suffix to append to a basic content name, if the content causes the effect.
effectToSuffix :: Effect -> Text
effectToSuffix NoEffect = ""
effectToSuffix Heal = "of healing"
effectToSuffix (Wound dice@(RollDice a b)) =
  if a == 0 && b == 0
  then "of wounding"
  else "(" <> showT dice <> ")"
effectToSuffix Mindprobe = "of soul searching"
effectToSuffix Dominate = "of domination"
effectToSuffix SummonFriend = "of aid calling"
effectToSuffix SpawnMonster = "of spawning"
effectToSuffix CreateItem = "of item creation"
effectToSuffix ApplyPerfume = "of rose water"
effectToSuffix Regeneration = "of regeneration"
effectToSuffix Searching = "of searching"
effectToSuffix Ascend = "of ascending"
effectToSuffix Descend = "of descending"

-- | How much AI benefits from applying the effect. Multipllied by item power.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Effect -> Int
effectToBenefit NoEffect = 0
effectToBenefit Heal = 10           -- TODO: depends on (maxhp - hp)
effectToBenefit (Wound _) = -10     -- TODO: dice ignored for now
effectToBenefit Mindprobe = 1       -- hard to use; TODO: limit by IQ
effectToBenefit Dominate = 1        -- hard to use; TODO: limit by IQ
effectToBenefit SummonFriend = 100
effectToBenefit SpawnMonster = 5    -- may or may not spawn a friendly
effectToBenefit CreateItem = 100
effectToBenefit ApplyPerfume = 0
effectToBenefit Regeneration = 0    -- much more benefit from carrying around
effectToBenefit Searching = 0       -- AI does not need to search
effectToBenefit Ascend = 1          -- AI can't choose levels smartly yet
effectToBenefit Descend = 1
