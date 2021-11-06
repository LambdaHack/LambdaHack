{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The type of kinds of factions present in a game, both human
-- and computer-controlled.
module Game.LambdaHack.Content.FactionKind
  ( FactionKind(..), makeData
  , HiCondPoly, HiSummand, HiPolynomial, HiIndeterminant(..)
  , TeamContinuity(..), Outcome(..)
  , teamExplorer, hiHeroLong, hiHeroMedium, hiHeroShort, hiDweller
  , victoryOutcomes, deafeatOutcomes
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

-- | Properties of a particular faction.
data FactionKind = FactionKind
  { fname         :: Text        -- ^ name of the faction
  , ffreq         :: Freqs FactionKind
                                 -- ^ frequency within groups
  , fteam         :: TeamContinuity
                                 -- ^ the team the faction identifies with
                                 --   across games and modes
  , fgroups       :: [GroupName ItemKind]
                                 -- ^ names of actor groups that may naturally
                                 --   fall under faction's control, e.g., upon
                                 --   spawning or summoning
  , fskillsOther  :: Ability.Skills
                                 -- ^ fixed skill modifiers to the non-leader
                                 --   actors; also summed with skills implied
                                 --   by @fdoctrine@ (which is not fixed)
  , fcanEscape    :: Bool        -- ^ the faction can escape the dungeon
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
  , fenemyTeams   :: [TeamContinuity]
                                 -- ^ teams starting at war with the faction
  , falliedTeams  :: [TeamContinuity]
                                 -- ^ teams starting allied with the faction
  }
  deriving (Show, Eq, Generic)

instance Binary FactionKind

-- | Team continuity index. Starting with 1, lower than 100.
newtype TeamContinuity = TeamContinuity Int
  deriving (Show, Eq, Ord, Enum, Generic)

instance Binary TeamContinuity

-- | Conditional polynomial representing score calculation for this faction.
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
    Escape    -- ^ the faction escaped the dungeon alive
  | Conquer   -- ^ the faction won by eliminating all rivals
  | Defeated  -- ^ the faction lost the game in another way
  | Killed    -- ^ the faction was eliminated
  | Restart   -- ^ game is restarted; the quitter quit
  | Camping   -- ^ game is supended
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Outcome

teamExplorer :: TeamContinuity
teamExplorer = TeamContinuity 1

hiHeroLong, hiHeroMedium, hiHeroShort, hiDweller :: HiCondPoly

hiHeroShort =
  [ ( [(HiLoot, 100)]
    , [minBound..maxBound] )
  , ( [(HiConst, 100)]
    , victoryOutcomes )
  , ( [(HiSprint, -500)]  -- speed matters, but only if fast enough
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , deafeatOutcomes )
  ]

hiHeroMedium =
  [ ( [(HiLoot, 200)]  -- usually no loot, but if so, no harm
    , [minBound..maxBound] )
  , ( [(HiConst, 200), (HiLoss, -10)]  -- normally, always positive
    , victoryOutcomes )
  , ( [(HiSprint, -500)]  -- speed matters, but only if fast enough
    , victoryOutcomes )
  , ( [(HiBlitz, -100)]  -- speed matters always
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , deafeatOutcomes )
  ]

-- Heroes in long crawls rejoice in loot.
hiHeroLong =
  [ ( [(HiLoot, 10000)]  -- multiplied by fraction of collected
    , [minBound..maxBound] )
  , ( [(HiConst, 15)]  -- a token bonus in case all loot lost, but victory
    , victoryOutcomes )
  , ( [(HiSprint, -20000)]  -- speedrun bonus, if below this number of turns
    , victoryOutcomes )
  , ( [(HiBlitz, -100)]  -- speed matters always
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , deafeatOutcomes )
  ]

-- Spawners get no points from loot, but try to kill
-- all opponents fast or at least hold up for long.
hiDweller = [ ( [(HiConst, 1000)]  -- no loot, so big win reward
              , victoryOutcomes )
            , ( [(HiConst, 1000), (HiLoss, -10)]
              , victoryOutcomes )
            , ( [(HiSprint, -1000)]  -- speedrun bonus, if below
              , victoryOutcomes )
            , ( [(HiBlitz, -100)]  -- speed matters
              , victoryOutcomes )
            , ( [(HiSurvival, 100)]
              , deafeatOutcomes )
            ]

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

validateSingle :: FactionKind -> [Text]
validateSingle FactionKind{..} =
  [ "fname longer than 50" | T.length fname > 50 ]
  ++ [ "fskillsOther not negative:" <+> fname
     | any ((>= 0) . snd) $ Ability.skillsToList fskillsOther ]
  ++ let checkLoveHate l team =
           [ "love-hate relationship for" <+> tshow team | team `elem` l ]
     in concatMap (checkLoveHate fenemyTeams) falliedTeams
  ++ let checkDipl field l team =
           [ "self-diplomacy in" <+> field | length (elemIndices team l) > 1 ]
     in concatMap (checkDipl "fenemyTeams" fenemyTeams) fenemyTeams
        ++ concatMap (checkDipl "falliedTeams" falliedTeams) falliedTeams

-- | Validate game faction kinds together.
validateAll :: [FactionKind] -> ContentData FactionKind -> [Text]
validateAll _ _ = []  -- so far, always valid

makeData :: [FactionKind] -> [GroupName FactionKind] -> [GroupName FactionKind]
         -> ContentData FactionKind
makeData = makeContentData "FactionKind" fname ffreq validateSingle validateAll
