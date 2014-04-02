-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.).
module Game.LambdaHack.Content.FactionKind
  ( FactionKind(..), validateFactionKind
  ) where

import Data.Text (Text)
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Misc

-- | Faction properties that are fixed for a given kind of factions.
data FactionKind = FactionKind
  { fsymbol        :: !Char       -- ^ a symbol
  , fname          :: !Text       -- ^ short description
  , ffreq          :: !Freqs      -- ^ frequency within groups
  , fAbilityLeader :: ![Ability]  -- ^ abilities of the picked leader
  , fAbilityOther  :: ![Ability]  -- ^ abilities of the other actors
  }
  deriving Show

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
validateFactionKind :: [FactionKind] -> [FactionKind]
validateFactionKind _ = []
