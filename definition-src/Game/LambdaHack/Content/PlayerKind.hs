{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of kinds of players present in a game, both human
-- and computer-controlled.
module Game.LambdaHack.Content.PlayerKind
  ( PlayerKind(..), makeData
  , HiCondPoly, HiSummand, HiPolynomial, HiIndeterminant(..)
  , TeamContinuity(..), Outcome(..)
  , teamExplorer, victoryOutcomes, deafeatOutcomes
  , nameOutcomePast, nameOutcomeVerb, endMessageOutcome
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs

-- | Properties of a particular player.
data PlayerKind = PlayerKind
  { fsymbol       :: Char        -- ^ a symbol
  , fname         :: Text        -- ^ name of the player
  , ffreq         :: Freqs PlayerKind
                                 -- ^ frequency within groups
  , fgroups       :: [GroupName ItemKind]
                                 -- ^ names of actor groups that may naturally
                                 --   fall under player's control, e.g., upon
                                 --   spawning or summoning
  , fskillsOther  :: Ability.Skills
                                 -- ^ fixed skill modifiers to the non-leader
                                 --   actors; also summed with skills implied
                                 --   by @fdoctrine@ (which is not fixed)
  , fcanEscape    :: Bool        -- ^ the player can escape the dungeon
  , fneverEmpty   :: Bool        -- ^ the faction declared killed if no actors
  , fhiCondPoly   :: HiCondPoly  -- ^ score formula (conditional polynomial)
  , fhasGender    :: Bool        -- ^ whether actors have gender
  , finitDoctrine :: Ability.Doctrine
                                 -- ^ initial faction's non-leaders doctrine
  , fspawnsFast   :: Bool
      -- ^ spawns fast enough that switching pointman to another level
      --   to optimize spawning is a winning tactics, which would spoil
      --   the fun, so switching is disabled in UI and AI clients
  , fhasPointman  :: Bool        -- ^ whether the faction can have a pointman
  , fhasUI        :: Bool        -- ^ does the faction have a UI client
                                 --   (for control or passive observation)
  , finitUnderAI  :: Bool        -- ^ is the faction initially under AI control
  }
  deriving (Show, Eq, Generic)

instance Binary PlayerKind

-- | Team continuity index. Starting with 1, lower than 100.
newtype TeamContinuity = TeamContinuity Int
  deriving (Show, Eq, Ord, Enum, Generic)

instance Binary TeamContinuity

-- | Conditional polynomial representing score calculation for this player.
type HiCondPoly = [HiSummand]

type HiSummand = (HiPolynomial, [Outcome])

type HiPolynomial = [(HiIndeterminant, Double)]

data HiIndeterminant =
    HiConst
  | HiLoot
  | HiSprint
  | HiBlitz
  | HiSurvival
  | HiKill
  | HiLoss
  deriving (Show, Eq, Generic)

instance Binary HiIndeterminant

-- | Outcome of a game.
data Outcome =
    Escape    -- ^ the player escaped the dungeon alive
  | Conquer   -- ^ the player won by eliminating all rivals
  | Defeated  -- ^ the faction lost the game in another way
  | Killed    -- ^ the faction was eliminated
  | Restart   -- ^ game is restarted; the quitter quit
  | Camping   -- ^ game is supended
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Outcome

teamExplorer :: TeamContinuity
teamExplorer = TeamContinuity 1

victoryOutcomes :: [Outcome]
victoryOutcomes = [Escape, Conquer]

deafeatOutcomes :: [Outcome]
deafeatOutcomes = [Defeated, Killed, Restart]

nameOutcomePast :: Outcome -> Text
nameOutcomePast = \case
  Escape   -> "emerged victorious"
  Conquer  -> "vanquished all opposition"
  Defeated -> "got decisively defeated"
  Killed   -> "got eliminated"
  Restart  -> "resigned prematurely"
  Camping  -> "set camp"

nameOutcomeVerb :: Outcome -> Text
nameOutcomeVerb = \case
  Escape   -> "emerge victorious"
  Conquer  -> "vanquish all opposition"
  Defeated -> "be decisively defeated"
  Killed   -> "be eliminated"
  Restart  -> "resign prematurely"
  Camping  -> "set camp"

endMessageOutcome :: Outcome -> Text
endMessageOutcome = \case
  Escape   -> "Can it be done more efficiently, though?"
  Conquer  -> "Can it be done in a better style, though?"
  Defeated -> "Let's hope your new overlords let you live."
  Killed   -> "Let's hope a rescue party arrives in time!"
  Restart  -> "This time for real."
  Camping  -> "See you soon, stronger and braver!"

validateSingle :: PlayerKind -> [Text]
validateSingle PlayerKind{..} =
  [ "fname longer than 50" | T.length fname > 50 ]
  ++ [ "fskillsOther not negative:" <+> fname
     | any ((>= 0) . snd) $ Ability.skillsToList fskillsOther ]

-- | Validate game player kinds together.
validateAll :: [PlayerKind] -> ContentData PlayerKind -> [Text]
validateAll _ _ = []  -- so far, always valid

makeData :: [PlayerKind] -> [GroupName PlayerKind] -> [GroupName PlayerKind]
         -> ContentData PlayerKind
makeData content groupNamesSingleton groupNames =
  makeContentData "PlayerKind" fname ffreq validateSingle validateAll content
                  groupNamesSingleton groupNames
