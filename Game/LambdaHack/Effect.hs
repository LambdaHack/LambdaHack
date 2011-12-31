module Game.LambdaHack.Effect where

import Game.LambdaHack.Random

data Effect =
    NoEffect
  | Heal             -- healing strength in ipower
  | Wound !RollDice  -- base damage, to-dam bonus in ipower
  | Dominate
  | SummonFriend
  | SummonEnemy
  | ApplyPerfume
  | Regeneration
  | Searching
  | Teleport
  deriving (Show, Read, Eq, Ord)

effectToName :: Effect -> String
effectToName NoEffect = ""
effectToName Heal = "of healing"
effectToName (Wound (RollDice a b)) =
  if a == 0 && b == 0
  then "of wounding"
  else "(" ++ show a ++ "d" ++ show b ++ ")"
effectToName Dominate = "of domination"
effectToName SummonFriend = "of aid calling"
effectToName SummonEnemy = "of summoning"
effectToName ApplyPerfume = "of rose water"
effectToName Regeneration = "of regeneration"
effectToName Searching = "of searching"
effectToName Teleport = "of teleportation"

-- | How much AI benefits from applying the effect. Multipllied by item power.
-- Negative means harm to the enemy when thrown. Zero won't ever be used.
effectToBenefit :: Effect -> Int
effectToBenefit NoEffect = 0
effectToBenefit Heal = 10           -- TODO: depends on (maxhp - hp)
effectToBenefit (Wound _) = -10     -- TODO: dice ignored for now
effectToBenefit Dominate = 0        -- AI can't use this
effectToBenefit SummonFriend = 100
effectToBenefit SummonEnemy = 0
effectToBenefit ApplyPerfume = 0
effectToBenefit Regeneration = 0    -- much more benefit from carrying around
effectToBenefit Searching = 0       -- AI does not need to search
effectToBenefit Teleport = 0        -- AI does not know when to teleport
