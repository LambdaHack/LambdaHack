module Effect where

data Effect =
    NoEffect
  | Heal Int
  | Wound Int  -- base damage, to-dam bonus in ipower
  | Dominate
  | SummonFriend
  | SummonEnemy
  | ApplyWater
  deriving (Show, Eq, Ord)

effectToName :: Effect -> String
effectToName NoEffect = ""
effectToName (Heal n) = "of healing (" ++ show n ++ ")"
effectToName (Wound n) = "(" ++ show n ++ "d1)"
effectToName Dominate = "of domination"
effectToName SummonFriend = "of aid calling"
effectToName SummonEnemy = "of summoning"
effectToName ApplyWater = "of water"
