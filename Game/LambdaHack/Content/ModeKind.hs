-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Players(..), Player(..), ModeKind(..), mvalidate
  ) where

import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

import Game.LambdaHack.Common.Misc (Freqs, LevelId)

-- | Faction properties that are fixed for a given kind of factions.
data ModeKind = ModeKind
  { msymbol  :: !Char       -- ^ a symbol
  , mname    :: !Text       -- ^ short description
  , mfreq    :: !Freqs      -- ^ frequency within groups
  , mplayers :: !Players    -- ^ players taking part in the game
  , mcaves   :: !Caves      -- ^ arena of the game
  }
  deriving Show

type Caves = EM.EnumMap LevelId (Text, Bool)

data Players = Players
  { playersHuman    :: ![Player]
  , playersComputer :: ![Player]
  , playersEnemy    :: ![(Text, Text)]
  , playersAlly     :: ![(Text, Text)]
  }
  deriving Show

data Player = Player
  { playerName    :: !Text
  , playerKind    :: !Text
  , playerInitial :: !Int
  , playerEntry   :: !LevelId
  }
  deriving Show

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
mvalidate :: [ModeKind] -> [ModeKind]
mvalidate _ = []
