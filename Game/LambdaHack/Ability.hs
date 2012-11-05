-- | AI strategy abilities.
module Game.LambdaHack.Ability
  ( Ability(..)
  ) where

-- | All possible AI actor abilities. AI chooses among these when considering
-- the next action to perform.
data Ability =
    Heal      -- ^ heal if almost dead
  | Flee      -- ^ flee if almost dead
  | Melee     -- ^ melee
  | Ranged    -- ^ ranged attack
  | Chase     -- ^ chase the opponent
  | Tools     -- ^ gather and use tools
  | Continue  -- ^ move along the set path, if any
  | Wander    -- ^ wander around, regardless of any opponents
  deriving (Show, Eq, Ord, Enum, Bounded)
