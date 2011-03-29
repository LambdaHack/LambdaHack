module Effect where

import Random

data Effect =
    NoEffect
  | Heal            -- healing strength in ipower
  | Wound RollDice  -- base damage, to-dam bonus in ipower
  | Dominate
  | SummonFriend
  | SummonEnemy
  | ApplyPerfume
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
