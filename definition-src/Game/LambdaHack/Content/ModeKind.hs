{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of game modes.
module Game.LambdaHack.Content.ModeKind
  ( pattern CAMPAIGN_SCENARIO, pattern INSERT_COIN
  , ModeKind(..), makeData
  , Caves, Roster
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll, validateSingleRoster, mandatoryGroups
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.FactionKind
  (FactionKind (..), Outcome (..))
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

-- | Game mode specification.
data ModeKind = ModeKind
  { mname     :: Text            -- ^ short description
  , mfreq     :: Freqs ModeKind  -- ^ frequency within groups
  , mtutorial :: Bool            -- ^ whether to show tutorial messages, etc.
  , mattract  :: Bool            -- ^ whether this is an attract mode
  , mroster   :: Roster          -- ^ factions taking part in the game
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

-- | The specification of factions and of levels, numbers and groups
-- of their initial members.
type Roster = [( GroupName FactionKind
               , [(Int, Dice.Dice, GroupName ItemKind)] )]

-- | Catch invalid game mode kind definitions.
validateSingle :: ContentData FactionKind -> ModeKind -> [Text]
validateSingle cofact ModeKind{..} =
  [ "mname longer than 20" | T.length mname > 20 ]
  ++ let f cave@(ns, l) =
           [ "not enough or too many levels for required cave groups:"
             <+> tshow cave
           | length ns /= length l ]
     in concatMap f mcaves
  ++ validateSingleRoster cofact mcaves mroster

-- | Checks, in particular, that there is at least one faction with fneverEmpty
-- or the game would get stuck as soon as the dungeon is devoid of actors.
validateSingleRoster :: ContentData FactionKind -> Caves -> Roster -> [Text]
validateSingleRoster cofact caves roster =
  let emptyGroups = filter (not . oexistsGroup cofact) $ map fst roster
  in [ "the following faction kind groups have no representative with non-zero frequency:"
       <+> T.intercalate ", " (map displayGroupName emptyGroups)
     | not $ null emptyGroups ]
  ++ let fkKeepsAlive acc _ _ fk = acc && fneverEmpty fk
           -- all of group elements have to keep level alive, hence conjunction
         fkGroupKeepsAlive (fkGroup, _) =
           ofoldlGroup' cofact fkGroup fkKeepsAlive True
     in [ "potentially no faction keeps the dungeon alive"
        | not $ any fkGroupKeepsAlive roster ]
  ++ let fkHasUIor acc _ _ fk = acc || fhasUI fk
           -- single group element having UI already incurs the risk
           -- of duplication, hence disjunction
         fkGroupHasUIor (fkGroup, _) =
           ofoldlGroup' cofact fkGroup fkHasUIor False
     in [ "potentially more than one UI client"
        | length (filter fkGroupHasUIor roster) > 1 ]
  ++ let fkHasUIand acc _ _ fk = acc && fhasUI fk
           -- single group element missing UI already incurs the risk
           -- of no UI in the whole game, hence disjunction
         fkGroupHasUIand (fkGroup, _) =
           ofoldlGroup' cofact fkGroup fkHasUIand True
     in [ "potentially less than one UI client"
        | length (filter fkGroupHasUIand roster) < 1 ]
  ++ let fkTokens acc _ _ fk = fteam fk : acc
         fkGroupTokens (fkGroup, _) = ofoldlGroup' cofact fkGroup fkTokens []
         tokens = concatMap (nub . sort . fkGroupTokens) roster
         nubTokens = nub . sort $ tokens
     in [ "potentially duplicate team continuity token"
        | length tokens /= length nubTokens ]
  ++ let keys = concatMap fst caves
         minD = minimum keys
         maxD = maximum keys
         f (_, l) = concatMap g l
         g i3@(ln, _, _) =
           [ "initial actor levels not among caves:" <+> tshow i3
           | ln `notElem` keys ]
     in concatMap f roster
        ++ [ "player is confused by both positive and negative level numbers"
           | signum minD /= signum maxD ]
        ++ [ "player is confused by level numer zero"
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

makeData :: ContentData FactionKind
         -> [ModeKind] -> [GroupName ModeKind] -> [GroupName ModeKind]
         -> ContentData ModeKind
makeData cofact content groupNamesSingleton groupNames =
  makeContentData "ModeKind" mname mfreq (validateSingle cofact) validateAll
                  content
                  groupNamesSingleton
                  (mandatoryGroups ++ groupNames)
