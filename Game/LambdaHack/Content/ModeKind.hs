{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Roster(..), Player(..), ModeKind(..), LeaderMode(..)
  , validateSingleModeKind, validateAllModeKind
  ) where

import Data.Binary
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import GHC.Generics (Generic)
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
  { fname          :: !Text     -- ^ name of the player
  , fgroup         :: !GroupName  -- ^ name of the monster group to control
  , fskillsLeader  :: !Skills   -- ^ skills of the currently selected leader
  , fskillsOther   :: !Skills   -- ^ skills of the other actors
  , fcanEscape     :: !Bool     -- ^ the player can escape the dungeon
  , fneverEmpty    :: !Bool     -- ^ the faction declared killed if no actors
  , fhasNumbers    :: !Bool     -- ^ whether actors have numbers, not symbols
  , fhasGender     :: !Bool     -- ^ whether actors have gender
  , foverrideAI    :: !(Maybe (){-TODO: only the follow-leader AI for now-})
                                -- ^ override all member AIs with this one
  , fentryLevel    :: !Int      -- ^ level where the initial members start
  , finitialActors :: !Int      -- ^ number of initial members
  , fhasLeader     :: !LeaderMode  -- ^ the mode of switching the leader
  , fisAI          :: !Bool     -- ^ is the faction under AI control?
  , fhasUI         :: !Bool     -- ^ does the faction have a UI client
                                --   (for control or passive observation)
  }
  deriving (Show, Eq, Generic)

instance Binary Player

data LeaderMode =
    LeaderNull  -- ^ faction has no leader
  | LeaderMode  -- ^ whenever faction has any actor, it has a leader
    { autoDungeon :: !Bool  -- ^ leader change between levels only automatic
    , autoLevel   :: !Bool  -- ^ leader change within a level only automatic
                            --   (currently no change at all in this case)
    }
  deriving (Show, Eq, Generic)

instance Binary LeaderMode

-- TODO: assert every Player's playerName's first word's length <= 15
-- TODO: assert if no UI, both Ai are on and there are some non-spawners;
-- assert that rosterEnemy and rosterAlly mention only factions in play.
-- | Catch invalid game mode kind definitions.
-- Note that for the @Player@, @fSkillsOther@ needn't be a subset
-- of @fSkillsLeader@.
validateSingleModeKind :: ModeKind -> [Text]
validateSingleModeKind _ = []

-- | Validate all game mode kinds.
validateAllModeKind :: [ModeKind] -> [Text]
validateAllModeKind _ = []
