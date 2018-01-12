{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( ModeKind(..), makeDef
  , Caves, Roster(..), Outcome(..)
  , HiCondPoly, HiSummand, HiPolynomial, HiIndeterminant(..)
  , Player(..), LeaderMode(..), AutoLeader(..)
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll
  , validateSingleRoster, validateSinglePlayer, hardwiredModeGroups
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Ability
import           Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.ItemKind (ItemKind)

-- | Game mode specification.
data ModeKind = ModeKind
  { msymbol :: Char            -- ^ a symbol
  , mname   :: Text            -- ^ short description
  , mfreq   :: Freqs ModeKind  -- ^ frequency within groups
  , mroster :: Roster          -- ^ players taking part in the game
  , mcaves  :: Caves           -- ^ arena of the game
  , mdesc   :: Text            -- ^ description
  }
  deriving Show

-- | Requested cave groups for particular levels. The second component
-- is the @Escape@ feature on the level. @True@ means it's represented
-- by @<@, @False@, by @>@.
type Caves = IM.IntMap (GroupName CaveKind)

-- | The specification of players for the game mode.
data Roster = Roster
  { rosterList  :: [(Player, [(Int, Dice.Dice, GroupName ItemKind)])]
      -- ^ players in the particular team and levels, numbers and groups
      --   of their initial members
  , rosterEnemy :: [(Text, Text)]  -- ^ the initial enmity matrix
  , rosterAlly  :: [(Text, Text)]  -- ^ the initial aliance matrix
  }
  deriving Show

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

-- | Conditional polynomial representing score calculation for this player.
type HiCondPoly = [HiSummand]

type HiSummand = (HiPolynomial, [Outcome])

type HiPolynomial = [(HiIndeterminant, Double)]

data HiIndeterminant = HiConst | HiLoot | HiBlitz | HiSurvival | HiKill | HiLoss
  deriving (Show, Eq, Ord, Generic)

instance Binary HiIndeterminant

-- | Properties of a particular player.
data Player = Player
  { fname        :: Text        -- ^ name of the player
  , fgroups      :: [GroupName ItemKind]
                                -- ^ names of actor groups that may naturally
                                --   fall under player's control, e.g., upon
                                --   spawning or summoning
  , fskillsOther :: Skills      -- ^ fixed skill modifiers to the non-leader
                                --   actors; also summed with skills implied
                                --   by ftactic (which is not fixed)
  , fcanEscape   :: Bool        -- ^ the player can escape the dungeon
  , fneverEmpty  :: Bool        -- ^ the faction declared killed if no actors
  , fhiCondPoly  :: HiCondPoly  -- ^ score polynomial for the player
  , fhasGender   :: Bool        -- ^ whether actors have gender
  , ftactic      :: Tactic      -- ^ non-leaders behave according to this
                                --   tactic; can be changed during the game
  , fleaderMode  :: LeaderMode  -- ^ the mode of switching the leader
  , fhasUI       :: Bool        -- ^ does the faction have a UI client
                                --   (for control or passive observation)
  }
  deriving (Show, Eq, Generic)

instance Binary Player

-- | If a faction with @LeaderUI@ and @LeaderAI@ has any actor, it has a leader.
data LeaderMode =
    LeaderNull  -- ^ faction can have no leader, is whole under AI control
  | LeaderAI AutoLeader -- ^ leader under AI control
  | LeaderUI AutoLeader -- ^ leader under UI control, assumes @fhasUI@
  deriving (Show, Eq, Ord, Generic)

instance Binary LeaderMode

data AutoLeader = AutoLeader
  { autoDungeon :: Bool
      -- ^ leader switching between levels is automatically done by the server
      --   and client is not permitted to change to leaders from other levels
      --   (the frequency of leader level switching done by the server
      --   is controlled by @RuleKind.rleadLevelClips@);
      --   if the flag is @False@, server still does a subset
      --   of the automatic switching, e.g., when the old leader dies
      --   and no other actor of the faction resides on his level,
      --   but the client (particularly UI) is expected to do changes as well
  , autoLevel   :: Bool
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

-- | Catch invalid game mode kind definitions.
validateSingle :: ModeKind -> [Text]
validateSingle ModeKind{..} =
  [ "mname longer than 20" | T.length mname > 20 ]
  ++ validateSingleRoster mcaves mroster

-- | Checks, in particular, that there is at least one faction with fneverEmpty
-- or the game would get stuck as soon as the dungeon is devoid of actors.
validateSingleRoster :: Caves -> Roster -> [Text]
validateSingleRoster caves Roster{..} =
  [ "no player keeps the dungeon alive"
  | all (not . fneverEmpty . fst) rosterList ]
  ++ concatMap (validateSinglePlayer . fst) rosterList
  ++ let checkPl field pl =
           [ pl <+> "is not a player name in" <+> field
           | all ((/= pl) . fname . fst) rosterList ]
         checkDipl field (pl1, pl2) =
           [ "self-diplomacy in" <+> field | pl1 == pl2 ]
           ++ checkPl field pl1
           ++ checkPl field pl2
     in concatMap (checkDipl "rosterEnemy") rosterEnemy
        ++ concatMap (checkDipl "rosterAlly") rosterAlly
  ++ let f (_, l) = concatMap g l
         g i3@(ln, _, _) =
           if ln `elem` IM.keys caves
           then []
           else ["initial actor levels not among caves:" <+> tshow i3]
     in concatMap f rosterList

validateSinglePlayer :: Player -> [Text]
validateSinglePlayer  Player{..} =
  [ "fname empty:" <+> fname | T.null fname ]
  ++ [ "no UI client, but UI leader:" <+> fname
     | not fhasUI && case fleaderMode of
                       LeaderUI _ -> True
                       _ -> False ]
  ++ [ "fskillsOther not negative:" <+> fname
     | any (>= 0) $ EM.elems fskillsOther ]

-- | Validate game mode kinds together.
validateAll :: [ModeKind] -> [Text]
validateAll content =
  let kindFreq :: S.Set (GroupName ModeKind)  -- cf. Kind.kindFreq
      kindFreq = let tuples = [ cgroup
                              | k <- content
                              , (cgroup, n) <- mfreq k
                              , n > 0 ]
                 in S.fromList tuples
      hardwiredAbsent = filter (`S.notMember` kindFreq) hardwiredModeGroups
  in [ "Hardwired groups not in content:" <+> tshow hardwiredAbsent
     | not $ null hardwiredAbsent ]

hardwiredModeGroups :: [GroupName ModeKind]
hardwiredModeGroups = [ "campaign scenario", "starting", "starting JS" ]

makeDef :: [ModeKind] -> ContentDef ModeKind
makeDef = makeContentDef mname validateSingle validateAll mfreq
