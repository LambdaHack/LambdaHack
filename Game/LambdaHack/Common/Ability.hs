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
    FirstAid  -- ^ try to heal if almost dead
  | Flee      -- ^ flee if almost dead
  | Melee     -- ^ melee target
  | Displace  -- ^ switch places with an actor
  | Pickup    -- ^ gather items
  | Trigger   -- ^ trigger a feature underneath
  | Ranged    -- ^ attack the visible target opponent at range
  | Tools     -- ^ use items
  | Chase     -- ^ try hard to get closer to the target
  | Wander    -- ^ if nothing else to do, wander towards the target
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Binary Ability where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8
