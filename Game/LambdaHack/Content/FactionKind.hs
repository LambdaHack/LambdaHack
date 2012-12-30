-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.).
module Game.LambdaHack.Content.FactionKind
  ( FactionKind(..), fvalidate
  ) where

import Data.Text (Text)
import Game.LambdaHack.Misc

-- | Faction properties that are fixed for a given kind of factions.
data FactionKind = FactionKind
  { fsymbol     :: !Char    -- ^ a symbol
  , fname       :: !Text    -- ^ short description
  , ffreq       :: !Freqs   -- ^ frequency within groups
  , fAiSelected :: !Text    -- ^ AI to use for the selected actor
  , fAiIdle     :: !Text    -- ^ AI to use for idle actors
  , fenemy      :: ![Text]  -- ^ initially in war with such factions
  , fally       :: ![Text]  -- ^ initially allied with such factions
  , fspawn      :: !Int     -- ^ spawns actors that often
  }
  deriving Show

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
fvalidate :: [FactionKind] -> [FactionKind]
fvalidate _ = []
