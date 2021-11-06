{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of kinds of game modes.
module Game.LambdaHack.Content.ModeKind
  ( pattern CAMPAIGN_SCENARIO, pattern INSERT_COIN, pattern NO_CONFIRMS
  , ModeKind(..), makeData
  , Caves, Roster(..)
  , screensave
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll, validateSingleRoster, mandatoryGroups
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.PlayerKind
  (Outcome (..), PlayerKind (..), TeamContinuity (..))
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

-- | Game mode specification.
data ModeKind = ModeKind
  { msymbol   :: Char            -- ^ a symbol
  , mname     :: Text            -- ^ short description
  , mfreq     :: Freqs ModeKind  -- ^ frequency within groups
  , mtutorial :: Bool            -- ^ whether to show tutorial messages, etc.
  , mattract  :: Bool            -- ^ whether this is an attract mode
  , mroster   :: Roster          -- ^ players taking part in the game
  , mcaves    :: Caves           -- ^ arena of the game
  , mendMsg   :: [(Outcome, Text)]
      -- ^ messages displayed at each particular game ends; if message empty,
      --   the screen is skipped
  , mrules    :: Text            -- ^ rules note
  , mdesc     :: Text            -- ^ description
  , mreason   :: Text            -- ^ why/when the mode should be played
  , mhint     :: Text            -- ^ hints in case player faces difficulties
  }
  deriving Show

-- | Requested cave groups for particular level intervals.
type Caves = [([Int], [GroupName CaveKind])]

-- | The specification of players for the game mode.
data Roster = Roster
  { rosterList  :: [( PlayerKind
                    , Maybe TeamContinuity
                    , [(Int, Dice.Dice, GroupName ItemKind)] )]
      -- ^ players in the particular team and levels, numbers and groups
      --   of their initial members
  , rosterEnemy :: [(Text, Text)]  -- ^ the initial enmity matrix
  , rosterAlly  :: [(Text, Text)]  -- ^ the initial aliance matrix
  }
  deriving Show

screensave :: ModeKind -> ModeKind
screensave mk = mk { mreason = "This is one of the screensaver scenarios, not available from the main menu, with all factions controlled by AI. Feel free to take over or relinquish control at any moment, but to register a legitimate high score, choose a standard scenario instead.\n" <> mreason mk
        }

-- | Catch invalid game mode kind definitions.
validateSingle :: ModeKind -> [Text]
validateSingle ModeKind{..} =
  [ "mname longer than 20" | T.length mname > 20 ]
  ++ let f cave@(ns, l) =
           [ "not enough or too many levels for required cave groups:"
             <+> tshow cave
           | length ns /= length l ]
     in concatMap f mcaves
  ++ validateSingleRoster mcaves mroster

-- | Checks, in particular, that there is at least one faction with fneverEmpty
-- or the game would get stuck as soon as the dungeon is devoid of actors.
validateSingleRoster :: Caves -> Roster -> [Text]
validateSingleRoster caves Roster{..} =
  [ "no player keeps the dungeon alive"
  | all (\(pl, _, _) -> not $ fneverEmpty pl) rosterList ]
  ++ [ "not exactly one UI client"
     | length (filter (\(pl, _, _) -> fhasUI pl) rosterList) /= 1 ]
  ++ let tokens = mapMaybe (\(_, tc, _) -> tc) rosterList
         nubTokens = nub $ sort tokens
     in [ "duplicate team continuity token"
        | length tokens /= length nubTokens ]
  ++ let checkPl field plName =
           [ plName <+> "is not a player name in" <+> field
           | all (\(pl, _, _) -> fname pl /= plName) rosterList ]
         checkDipl field (pl1, pl2) =
           [ "self-diplomacy in" <+> field | pl1 == pl2 ]
           ++ checkPl field pl1
           ++ checkPl field pl2
     in concatMap (checkDipl "rosterEnemy") rosterEnemy
        ++ concatMap (checkDipl "rosterAlly") rosterAlly
  ++ let keys = concatMap fst caves
         minD = minimum keys
         maxD = maximum keys
         f (_, _, l) = concatMap g l
         g i3@(ln, _, _) =
           [ "initial actor levels not among caves:" <+> tshow i3
           | ln `notElem` keys ]
     in concatMap f rosterList
        ++ [ "player confused by both positive and negative level numbers"
           | signum minD /= signum maxD ]
        ++ [ "player confused by level numer zero"
           | any (== 0) keys ]

-- | Validate game mode kinds together.
validateAll :: [ModeKind] -> ContentData ModeKind -> [Text]
validateAll _ _ = []  -- so far, always valid

-- * Mandatory item groups

mandatoryGroups :: [GroupName ModeKind]
mandatoryGroups =
       [CAMPAIGN_SCENARIO, INSERT_COIN]

pattern CAMPAIGN_SCENARIO, INSERT_COIN :: GroupName ModeKind

pattern CAMPAIGN_SCENARIO = GroupName "campaign scenario"
pattern INSERT_COIN = GroupName "insert coin"

-- * Optional item groups

pattern NO_CONFIRMS :: GroupName ModeKind

pattern NO_CONFIRMS = GroupName "no confirms"

makeData :: [ModeKind] -> [GroupName ModeKind] -> [GroupName ModeKind]
         -> ContentData ModeKind
makeData content groupNamesSingleton groupNames =
  makeContentData "ModeKind" mname mfreq validateSingle validateAll content
                  groupNamesSingleton
                  (mandatoryGroups ++ groupNames)
