{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Roster(..), Player(..), ModeKind(..), LeaderMode(..), AutoLeader(..)
  , validateSingleModeKind, validateAllModeKind
  ) where

import Data.Binary
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU ()

import Game.LambdaHack.Common.Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind (ItemKind)

-- | Game mode specification.
data ModeKind = ModeKind
  { msymbol :: !Char    -- ^ a symbol (matches the keypress, if any)
  , mname   :: !Text    -- ^ short description
  , mfreq   :: !(Freqs ModeKind)  -- ^ frequency within groups
  , mroster :: !Roster  -- ^ players taking part in the game
  , mcaves  :: !Caves   -- ^ arena of the game
  , mdesc   :: !Text    -- ^ description
  }
  deriving Show

-- | Requested cave groups for particular levels. The second component
-- is the @Escape@ feature on the level. @True@ means it's represented
-- by @<@, @False@, by @>@.
type Caves = IM.IntMap (GroupName CaveKind, Maybe Bool)

-- | The specification of players for the game mode.
data Roster = Roster
  { rosterList  :: ![Player Dice.Dice]  -- ^ players in the particular team
  , rosterEnemy :: ![(Text, Text)]      -- ^ the initial enmity matrix
  , rosterAlly  :: ![(Text, Text)]      -- ^ the initial aliance matrix
  }
  deriving (Show, Eq)

-- | Properties of a particular player.
data Player a = Player
  { fname          :: !Text        -- ^ name of the player
  , fgroup         :: !(GroupName ItemKind)  -- ^ name of the monster group to control
  , fskillsOther   :: !Skills      -- ^ skills of the other actors
  , fcanEscape     :: !Bool        -- ^ the player can escape the dungeon
  , fneverEmpty    :: !Bool        -- ^ the faction declared killed if no actors
  , fhasNumbers    :: !Bool        -- ^ whether actors have numbers, not symbols
  , fhasGender     :: !Bool        -- ^ whether actors have gender
  , ftactic        :: !Tactic      -- ^ members behave according to this tactic
  , fentryLevel    :: !a           -- ^ level where the initial members start
  , finitialActors :: !a           -- ^ number of initial members
  , fleaderMode    :: !LeaderMode  -- ^ the mode of switching the leader
  , fhasUI         :: !Bool        -- ^ does the faction have a UI client
                                   --   (for control or passive observation)
  }
  deriving (Show, Eq, Generic)

instance Binary a => Binary (Player a)

-- | If a faction with @LeaderUI@ and @LeaderAI@ has any actor, it has a leader.
data LeaderMode =
    LeaderNull  -- ^ faction can have no leader, is whole under AI control
  | LeaderAI AutoLeader -- ^ leader under AI control
  | LeaderUI AutoLeader -- ^ leader under UI control, assumes @fhasUI@
  deriving (Show, Eq, Generic)

instance Binary LeaderMode

data AutoLeader = AutoLeader
  { autoDungeon :: !Bool
      -- ^ leader switching between levels is automatically done by the server
      --   and client is not permitted to change leaders
      --   (the frequency of leader level switching done by the server
      --   is controlled by @RuleKind.rleadLevelClips@);
      --   if the flag is @False@, server still does a subset
      --   of the automatic switching, e.g., when the old leader dies
      --   and no other actor of the faction resides on his level,
      --   but the client (particularly UI) is expected to do changes as well
  , autoLevel   :: !Bool
      -- ^ leader switching within a level is automatically done by the server
      --   and client is not permitted to change leaders
      --   (server is guaranteed to switch leader within a level very rarely,
      --   e.g., when the old leader dies);
      --   if the flag is @False@, server still does a subset
      --   of the automatic switching, but the client is permitted to do more
  }
  deriving (Show, Eq, Generic)

instance Binary AutoLeader

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
validateSinglePlayer :: Caves -> Player Dice.Dice -> [Text]
validateSinglePlayer caves Player{..} =
  [ "fname empty:" <+> fname | T.null fname ]
  ++ [ "first word of fname longer than 15:" <+> fname
     | T.length (head $ T.words fname) > 15 ]
  ++ [ "no UI client, but UI leader:" <+> fname
     | not fhasUI && case fleaderMode of
                       LeaderUI _ -> True
                       _ -> False ]
  ++ [ "fentryLevel value not among cave numbers:" <+> fname
     | any (`notElem` IM.keys caves)
           [Dice.minDice fentryLevel
            .. Dice.maxDice fentryLevel] ]  -- simplification

-- | Validate all game mode kinds. Currently always valid.
validateAllModeKind :: [ModeKind] -> [Text]
validateAllModeKind _ = []
