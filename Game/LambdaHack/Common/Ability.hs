-- | AI strategy abilities.
module Game.LambdaHack.Common.Ability
  ( Ability(..)
  ) where

import Data.Binary

-- | All possible AI actor abilities. AI chooses among these when considering
-- the next action to perform. The ability descriptions refer to the target
-- that any actor picks each turn, depending on the actor's characteristics
-- and his environment.
data Ability =
    Heal      -- ^ heal if almost dead
  | Flee      -- ^ flee if almost dead or far from friends
  | Melee     -- ^ melee target
  | Displace  -- ^ switch places with a friend
  | Pickup    -- ^ gather items, if no foes visible
  | Trigger   -- ^ trigger a feature
  | Ranged    -- ^ attack the visible target opponent at range, some of the time
  | Tools     -- ^ use items, if target opponent visible, some of the time
  | Chase     -- ^ chase the target, ignoring any actors on the way
  | Wander    -- ^ wander around, meleeing any opponents on the way
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8
