{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Roster(..), Player(..), ModeKind(..), Tactic(..), LeaderMode(..)
  , validateSingleModeKind, validateAllModeKind
  ) where

import Data.Binary
import Data.Hashable (Hashable)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU ()

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg

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
  { fname          :: !Text        -- ^ name of the player
  , fgroup         :: !GroupName   -- ^ name of the monster group to control
  , fskillsLeader  :: !Skills      -- ^ skills of the currently selected leader
  , fskillsOther   :: !Skills      -- ^ skills of the other actors
  , fcanEscape     :: !Bool        -- ^ the player can escape the dungeon
  , fneverEmpty    :: !Bool        -- ^ the faction declared killed if no actors
  , fhasNumbers    :: !Bool        -- ^ whether actors have numbers, not symbols
  , fhasGender     :: !Bool        -- ^ whether actors have gender
  , ftactic        :: !Tactic      -- ^ members behave according to this tactic
  , fentryLevel    :: !Int         -- ^ level where the initial members start
  , finitialActors :: !Int         -- ^ number of initial members
  , fhasLeader     :: !LeaderMode  -- ^ the mode of switching the leader
  , fisAI          :: !Bool        -- ^ is the faction under AI control?
  , fhasUI         :: !Bool        -- ^ does the faction have a UI client
                                   --   (for control or passive observation)
  }
  deriving (Show, Eq, Generic)

instance Binary Player

-- Keep this type here, so that Contents/ is as self-contained as possible.
-- TODO: alway shoot, never shoot, etc., but there is too many and this is best
-- expressed via skills, and also we risk micromanagement, so let's stop
-- and think first; perhaps only have as many tactics as needed for realistic
-- AI behaviour in our game modes; perhaps even expose only some of them to UI
data Tactic =
    TBlock    -- ^ always only wait, even if enemy in melee range
  | TFollow   -- ^ always follow leader's target or his position if no target
  | TExplore  -- ^ if enemy nearby, attack, if no items, etc., explore unknown
  | TRoam     -- ^ if enemy nearby, attack, if no items, etc., roam randomly
  | TPatrol   -- ^ find an open and uncrowded area, patrol it according
              --   to sight radius and fallback temporarily to @TRoam@
              --   when enemy is seen by the faction and is within
              --   the actor's sight radius
              --   TODO (currently the same as TExplore; should it chase
              --   targets too (TRoam) and only switch to TPatrol when none?)
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Tactic where
  show TBlock = "block and wait"
  show TFollow = "follow leader's target or position"
  show TExplore = "explore unknown, chase targets"
  show TRoam = "roam freely, chase targets"
  show TPatrol = "find and patrol an area (TODO)"

instance Binary Tactic

instance Hashable Tactic

-- TODO: this partially overlaps fisAI (LeaderNull implies fisAI)
data LeaderMode =
    LeaderNull  -- ^ faction can have no leader
  | LeaderMode  -- ^ whenever faction has any actor, it has a leader
    { autoDungeon :: !Bool  -- ^ leader change between levels only automatic
    , autoLevel   :: !Bool  -- ^ leader change within a level only automatic
                            --   (for UI, currently no leader change here)
    }
  deriving (Show, Eq, Generic)

instance Binary LeaderMode

-- TODO: (spans multiple contents) Check that caves with the given groups exist.
-- | Catch invalid game mode kind definitions.
validateSingleModeKind :: ModeKind -> [Text]
validateSingleModeKind ModeKind{..} =
  [ "mname longer than 20" | T.length mname > 20 ]
  ++ validateSingleRoster mcaves mroster

-- TODO: if the diplomacy system stays in, check no teams are at once
-- in war and alliance, taking into account symmetry (but not transitvity)
-- | Checks, in particular, that there is at least one faction with fneverEmpty
-- or the game could get stuck when the dungeon is devoid of actors
validateSingleRoster :: Caves -> Roster -> [Text]
validateSingleRoster caves Roster{..} =
  [ "no player keeps the dungeon alive" | all (not . fneverEmpty) rosterList ]
  ++ concatMap (validateSinglePlayer caves) rosterList
  ++ let checkPl field pl =
           [ pl <+> "is not a player name in" <+> field
           | all ((/= pl) . fname) rosterList ]
         checkDipl field (pl1, pl2) =
           [ "self-diplomacy in" <+> field | pl1 == pl2 ]
           ++ checkPl field pl1
           ++ checkPl field pl2
     in concatMap (checkDipl "rosterEnemy") rosterEnemy
        ++ concatMap (checkDipl "rosterAlly") rosterAlly

-- Note that @fSkillsOther@ needn't be a subset of @fSkillsLeader@.
validateSinglePlayer :: Caves -> Player -> [Text]
validateSinglePlayer caves Player{..} =
  [ "fname empty:" <+> fname | T.null fname ]
  ++ [ "first word of fname longer than 15:" <+> fname
     | T.length (head $ T.words fname) > 15 ]
  ++ [ "neither AI nor UI:" <+> fname | not fisAI && not fhasUI ]
  ++ [ "fentryLevel not among caves:" <+> fname
     | fentryLevel `notElem` IM.keys caves ]

-- | Validate all game mode kinds. Currently always valid.
validateAllModeKind :: [ModeKind] -> [Text]
validateAllModeKind _ = []
