-- | AI strategy abilities.
module Game.LambdaHack.Ability
  ( Ability(..)
  ) where

-- | All possible AI actor abilities. AI chooses among these when considering
-- the next action to perform. The ability descriptions refer to the target
-- that any actor picks each turn, depending on the actor's characteristics
-- and his environment.
data Ability =
    Track   -- ^ move along a set path, if any, meleeing any opponents
  | Heal    -- ^ heal if almost dead
  | Flee    -- ^ flee if almost dead
  | Melee   -- ^ melee target
  | Pickup  -- ^ gather items, if no foes visible
  | Ranged  -- ^ attack the visible target opponent at range, some of the time
  | Tools   -- ^ use items, if target opponent visible, some of the time
  | Chase   -- ^ chase the target, ignoring any actors on the way
  | Wander  -- ^ go to a non-actor target, meleeing any opponents on the way
  deriving (Show, Eq, Ord, Enum, Bounded)
