{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( Caves, Roster(..), Player(..), ModeKind(..), LeaderMode(..), AutoLeader(..)
  , Outcome(..), HiIndeterminant(..), HiCondPoly, HiSummand, HiPolynomial
  , validateSingleModeKind, validateAllModeKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU ()

import Game.LambdaHack.Common.Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Misc
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
type Caves = IM.IntMap (GroupName CaveKind)

-- | The specification of players for the game mode.
data Roster = Roster
  { rosterList  :: ![Player Dice.Dice]  -- ^ players in the particular team
  , rosterEnemy :: ![(Text, Text)]      -- ^ the initial enmity matrix
  , rosterAlly  :: ![(Text, Text)]      -- ^ the initial aliance matrix
  }
  deriving (Show, Eq)

-- | Outcome of a game.
data Outcome =
    Killed    -- ^ the faction was eliminated
  | Defeated  -- ^ the faction lost the game in another way
  | Camping   -- ^ game is supended
  | Conquer   -- ^ the player won by eliminating all rivals
  | Escape    -- ^ the player escaped the dungeon alive
  | Restart   -- ^ game is restarted
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Outcome

data HiIndeterminant = HiConst | HiLoot | HiBlitz | HiSurvival | HiKill | HiLoss
  deriving (Show, Eq, Ord, Generic)

instance Binary HiIndeterminant

type HiPolynomial = [(HiIndeterminant, Double)]

type HiSummand = (HiPolynomial, [Outcome])

-- | Conditional polynomial representing score calculation for this player.
type HiCondPoly = [HiSummand]

-- | Properties of a particular player.
data Player a = Player
  { fname          :: !Text        -- ^ name of the player
  , fgroup         :: !(GroupName ItemKind)  -- ^ name of the monster group to control
  , fskillsOther   :: !Skills      -- ^ fixed skill modifiers to the non-leader
                                   --   actors; also summed with skills implied
                                   --   by ftactic (which is not fixed)
  , fcanEscape     :: !Bool        -- ^ the player can escape the dungeon
  , fneverEmpty    :: !Bool        -- ^ the faction declared killed if no actors
  , fhiCondPoly    :: !HiCondPoly  -- ^ score polynomial for the player
  , fhasNumbers    :: !Bool        -- ^ whether actors have numbers, not symbols
  , fhasGender     :: !Bool        -- ^ whether actors have gender
  , ftactic        :: !Tactic      -- ^ non-leader behave according to this
                                   --   tactic; can be changed during the game
  , fentryLevel    :: !a           -- ^ level where the initial members start
  , finitialActors :: !a           -- ^ number of initial members
  , fleaderMode    :: !LeaderMode  -- ^ the mode of switching the leader
  , fhasUI         :: !Bool        -- ^ does the faction have a UI client
                                   --   (for control or passive observation)
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary a => Binary (Player a)

-- | If a faction with @LeaderUI@ and @LeaderAI@ has any actor, it has a leader.
data LeaderMode =
    LeaderNull  -- ^ faction can have no leader, is whole under AI control
  | LeaderAI AutoLeader -- ^ leader under AI control
  | LeaderUI AutoLeader -- ^ leader under UI control, assumes @fhasUI@
  deriving (Show, Eq, Ord, Generic)

instance Binary LeaderMode

data AutoLeader = AutoLeader
  { autoDungeon :: !Bool
      -- ^ leader switching between levels is automatically done by the server
      --   and client is not permitted to change to leaders from other levels
      --   (the frequency of leader level switching done by the server
      --   is controlled by @RuleKind.rleadLevelClips@);
      --   if the flag is @False@, server still does a subset
      --   of the automatic switching, e.g., when the old leader dies
      --   and no other actor of the faction resides on his level,
      --   but the client (particularly UI) is expected to do changes as well
  , autoLevel   :: !Bool
      -- ^ client is discouraged from leader switching (e.g., because
      --   non-leader actors have the same skills as leader);
      --   server is guaranteed to switch leader within a level very rarely,
      --   e.g., when the old leader dies;
      --   if the flag is @False@, server still does a subset
      --   of the automatic switching, but the client is expected to do more,
      --   because it's advantageous for that kind of a faction
  }
  deriving (Show, Eq, Ord, Generic)

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
  ++ [ "fskillsOther not negative:" <+> fname
     | any (>= 0) $ EM.elems fskillsOther ]

-- | Validate all game mode kinds. Currently always valid.
validateAllModeKind :: [ModeKind] -> [Text]
validateAllModeKind _ = []
