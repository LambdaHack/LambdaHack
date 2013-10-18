-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Players(..), Player(..), ModeKind(..), mvalidate
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)
import NLP.Miniutter.English ()

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
  { playersHuman    :: ![Player]        -- ^ players with UI
  , playersComputer :: ![Player]        -- ^ players without UI
  , playersEnemy    :: ![(Text, Text)]  -- ^ the initial enmity matrix
  , playersAlly     :: ![(Text, Text)]  -- ^ the initial aliance matrix
  }
  deriving (Show, Eq)

data Player = Player
  { playerName     :: !Text     -- ^ name of the player
  , playerFaction  :: !Text     -- ^ name of faction(s) the player can control
  , playerEntry    :: !LevelId  -- ^ level where the initial members start
  , playerInitial  :: !Int      -- ^ number of initial members
  , playerAiLeader :: !Bool     -- ^ is the leader under AI control?
  , playerAiOther  :: !Bool     -- ^ are the others under AI control?
  , playerForceUI  :: !(Maybe Bool)  -- ^ force creation of the UI client
  }
  deriving (Show, Eq)

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
mvalidate :: [ModeKind] -> [ModeKind]
mvalidate _ = []

instance Binary Player where
  put Player{..} = do
    put playerName
    put playerFaction
    put playerEntry
    put playerInitial
    put playerAiLeader
    put playerAiOther
    put playerForceUI
  get = do
    playerName <- get
    playerFaction <- get
    playerEntry <- get
    playerInitial <- get
    playerAiLeader <- get
    playerAiOther <- get
    playerForceUI <- get
    return Player{..}
