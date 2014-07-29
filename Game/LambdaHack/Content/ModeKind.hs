-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Roster(..), Player(..), ModeKind(..), validateModeKind
  ) where

import Data.Binary
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU ()

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Misc

-- | Game mode specification.
data ModeKind = ModeKind
  { msymbol :: !Char    -- ^ a symbol (matches the keypress, if any)
  , mname   :: !Text    -- ^ short description
  , mfreq   :: !Freqs   -- ^ frequency within groups
  , mroster :: !Roster  -- ^ players taking part in the game
  , mcaves  :: !Caves   -- ^ arena of the game
  , mdesc   :: !Text    -- ^ description
  }
  deriving Show

-- | Requested cave groups for particular levels. The second component
-- is the @Escape@ feature on the level. @True@ means it's represented
-- by @<@, @False@, by @>@.
type Caves = IM.IntMap (GroupName, Maybe Bool)

-- | The specification of players for the game mode.
data Roster = Roster
  { rosterList  :: ![Player]        -- ^ players in the particular team
  , rosterEnemy :: ![(Text, Text)]  -- ^ the initial enmity matrix
  , rosterAlly  :: ![(Text, Text)]  -- ^ the initial aliance matrix
  }
  deriving (Show, Eq)

-- | Properties of a particular player.
data Player = Player
  { fname         :: !Text     -- ^ name of the player
  , fgroup        :: !GroupName  -- ^ name of the monster group to control
  , ffreq         :: !Freqs    -- ^ frequency within groups
  , fskillsLeader :: !Skills   -- ^ skills of the currently selected leader
  , fskillsOther  :: !Skills   -- ^ skills of the other actors
  , fisSpawn      :: !Bool     -- ^ whether the player is a spawn (score, AI)
  , fisHero       :: !Bool     -- ^ whether the player is a hero (score, AI, UI)
  , fentry        :: !Int      -- ^ level where the initial members start
  , finitial      :: !Int      -- ^ number of initial members
  , fleader       :: !Bool     -- ^ leaderless factions can't be controlled
                               --   by a human or a user-supplied AI client
  , fisAI         :: !Bool     -- ^ is the faction under AI control?
  , fisUI         :: !Bool     -- ^ does the faction have a UI client
                               --   (for control or passive observation)
  }
  deriving (Show, Eq)

-- TODO: assert every Player's playerName's first word's length <= 15
-- TODO: assert if no UI, both Ai are on and there are some non-spawners;
-- assert that rosterEnemy and rosterAlly mention only factions in play.
-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
-- In particular, for the @Player@, @fSkillsOther@ needn't be a subset
-- of @fSkillsLeader@.
validateModeKind :: [ModeKind] -> [ModeKind]
validateModeKind _ = []

instance Binary Player where
  put Player{..} = do
    put fname
    put fgroup
    put ffreq
    put fskillsLeader
    put fskillsOther
    put fisSpawn
    put fisHero
    put fentry
    put finitial
    put fleader
    put fisAI
    put fisUI
  get = do
    fname <- get
    fgroup <- get
    ffreq <- get
    fskillsLeader <- get
    fskillsOther <- get
    fisSpawn <- get
    fisHero <- get
    fentry <- get
    finitial <- get
    fleader <- get
    fisAI <- get
    fisUI <- get
    return $! Player{..}
