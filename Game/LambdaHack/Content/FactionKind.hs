-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.).
module Game.LambdaHack.Content.FactionKind
  ( FactionKind(..), fvalidate
  ) where

import Game.LambdaHack.Misc

-- | Faction properties that are fixed for a given kind of factions.
data FactionKind = FactionKind
  { fsymbol     :: !Char      -- ^ a symbol
  , fname       :: !String    -- ^ short description
  , ffreq       :: !Freqs     -- ^ frequency within groups
  , fAiSelected :: !String    -- ^ Ai to use for the selected actor
  , fAiIdle     :: !String    -- ^ Ai to use for idle actors
  , fenemy      :: ![String]  -- ^ initially in war with these factions
  , fally       :: ![String]  -- ^ initially allied with these factions
  }
  deriving Show

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
fvalidate :: [FactionKind] -> [FactionKind]
fvalidate _ = []
