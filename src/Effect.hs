module Effect where

import Random
import WorldLoc

data Effect =
    NoEffect
  | Heal            -- healing strength in ipower
  | Wound RollDice  -- base damage, to-dam bonus in ipower
  | Dominate
  | SummonFriend
  | SummonEnemy
  | ApplyPerfume
  | Regneration
  | Searching
  | Teleport WorldLoc
  deriving (Show, Eq, Ord)

effectToName :: Effect -> String
effectToName NoEffect = ""
effectToName Heal = "of healing"
effectToName (Wound (a, b)) = if a == 0 && b == 0
                              then "of wounding"
                              else "(" ++ show a ++ "d" ++ show b ++ ")"
effectToName Dominate = "of domination"
effectToName SummonFriend = "of aid calling"
effectToName SummonEnemy = "of summoning"
effectToName ApplyPerfume = "of rose water"
effectToName Regneration = "of regeneration"
effectToName Searching = "of searching"
effectToName (Teleport _) = "of teleport"

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
effectToBenefit Regneration = 0     -- much more benefit from carrying around
effectToBenefit Searching = 0       -- AI does not need to search
effectToBenefit (Teleport _) = 0    -- AI does not know when to teleport
