-- | A set of atomic commands. These are the largest building blocks
-- that have no components that can be observed in isolation.
-- We also try to make them respect the laws of energy and mass conservation.
-- So, e.g., item removed from inventory is not an atomic commands,
-- but item dropped from the inventory to the ground is. This makes
-- it easier to undo the commands. In principle, the commands are the only
-- way to affect game state. Clients should be sent state updates
-- after each atomic command they can observe.
module Game.LambdaHack.Server.CmdAtomic
  ( CmdAtomic(..)
  ) where

import Game.LambdaHack.Actor

-- | Abstract syntax of atomic commands.
data CmdAtomic =
    HealAtomic Int ActorId
  deriving Show
