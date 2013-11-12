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
  { msymbol  :: !Char     -- ^ a symbol
  , mname    :: !Text     -- ^ short description
  , mfreq    :: !Freqs    -- ^ frequency within groups
  , mplayers :: !Players  -- ^ players taking part in the game
  , mcaves   :: !Caves    -- ^ arena of the game
  }
  deriving Show

type Caves = EM.EnumMap LevelId (Text, Maybe Bool)

data Players = Players
  { playersList  :: ![Player]        -- ^ players, both human and computer
  , playersEnemy :: ![(Text, Text)]  -- ^ the initial enmity matrix
  , playersAlly  :: ![(Text, Text)]  -- ^ the initial aliance matrix
  }
  deriving (Show, Eq)

data Player = Player
  { playerName     :: !Text     -- ^ name of the player
  , playerFaction  :: !Text     -- ^ name of faction(s) the player can control
  , playerEntry    :: !LevelId  -- ^ level where the initial members start
  , playerInitial  :: !Int      -- ^ number of initial members
  , playerAiLeader :: !Bool     -- ^ is the leader under AI control?
  , playerAiOther  :: !Bool     -- ^ are the others under AI control?
  , playerHuman    :: !Bool     -- ^ is the player controlled by human?
  , playerUI       :: !Bool     -- ^ does the faction have a UI client
                                -- (for control or passive observation)
  }
  deriving (Show, Eq)

-- TODO: assert if no UI, both Ai are on and there are some non-spawners
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
    put playerHuman
    put playerUI
  get = do
    playerName <- get
    playerFaction <- get
    playerEntry <- get
    playerInitial <- get
    playerAiLeader <- get
    playerAiOther <- get
    playerHuman <- get
    playerUI <- get
    return Player{..}
