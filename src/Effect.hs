module Effect where

data Effect =
    NoEffect
  | AffectHP Int  -- base damage, to-dam bonus in Item
  | Dominate
  | SummonFriend
  | SummonEnemy
  deriving (Show, Eq, Ord)

effectToName :: Effect -> String
effectToName NoEffect = ""
effectToName (AffectHP n)
  | n > 0 = "of healing (" ++ show n ++ ")"
  | n < 0 = "" -- "(base dmg " ++ show (-n) ++ ")"
  | otherwise = "of life"
effectToName Dominate = "of domination"
effectToName SummonFriend = "of aid calling"
effectToName SummonEnemy = "of summoning"
