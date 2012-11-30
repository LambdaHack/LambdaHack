-- | The type of kinds of AI strategies.
module Game.LambdaHack.Content.StrategyKind
  ( StrategyKind(..), svalidate
  ) where

import Data.Text (Text)

import Game.LambdaHack.Ability
import Game.LambdaHack.Misc

-- | Strategy properties that are fixed for a given kind of strategies.
data StrategyKind = StrategyKind
  { ssymbol    :: !Char       -- ^ a symbol
  , sname      :: !Text       -- ^ short description
  , sfreq      :: !Freqs      -- ^ frequency within groups
  , sabilities :: ![Ability]  -- ^ abilities to pick from in roughly that order
  }
  deriving Show

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
svalidate :: [StrategyKind] -> [StrategyKind]
svalidate _ = []
