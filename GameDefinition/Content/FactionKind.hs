-- | Basic players definitions.
module Content.FactionKind
  ( -- * Group names
    groupNamesSingleton, groupNames
  , -- * Content
    content

  , playerExplorer, playerExplorerShort, playerExplorerNoEscape, playerExplorerMedium, playerExplorerTrapped, playerExplorerAutomated, playerExplorerAutomatedTrapped, playerCompetitor, playerCompetitorShort, playerCompetitorNoEscape, playerCivilian, playerConvict, playerMonster, playerAntiMonster, playerAntiMonsterCaptive, playerMonsterTourist, playerMonsterTouristPassive, playerMonsterCaptive, playerMonsterCaptiveNarrating, playerAnimal, playerAnimalMagnificent, playerAnimalExquisite, playerAnimalCaptive, playerAnimalNarrating, playerAnimalMagnificentNarrating, playerAnimalCaptiveNarrating, playerHorror
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Content.ItemKindActor
import           Content.ItemKindOrgan
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs

groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton = []

groupNames :: [GroupName FactionKind]
groupNames = []

content :: [FactionKind]
content = [playerExplorer, playerExplorerShort, playerExplorerNoEscape, playerExplorerMedium, playerExplorerTrapped, playerExplorerAutomated, playerExplorerAutomatedTrapped, playerCompetitor, playerCompetitorShort, playerCompetitorNoEscape, playerCivilian, playerConvict, playerMonster, playerAntiMonster, playerAntiMonsterCaptive, playerMonsterTourist, playerMonsterTouristPassive, playerMonsterCaptive, playerMonsterCaptiveNarrating, playerAnimal, playerAnimalMagnificent, playerAnimalExquisite, playerAnimalCaptive, playerAnimalNarrating, playerAnimalMagnificentNarrating, playerAnimalCaptiveNarrating, playerHorror]

playerExplorer,            playerExplorerShort, playerExplorerNoEscape, playerExplorerMedium, playerExplorerTrapped, playerExplorerAutomated, playerExplorerAutomatedTrapped, playerCompetitor, playerCompetitorShort, playerCompetitorNoEscape, playerCivilian, playerConvict, playerMonster, playerAntiMonster, playerAntiMonsterCaptive, playerMonsterTourist, playerMonsterTouristPassive, playerMonsterCaptive, playerMonsterCaptiveNarrating, playerAnimal, playerAnimalMagnificent, playerAnimalExquisite, playerAnimalCaptive, playerAnimalNarrating, playerAnimalMagnificentNarrating, playerAnimalCaptiveNarrating, playerHorror :: FactionKind

-- * Teams

teamCompetitor, teamCivilian, teamConvict, teamMonster, teamAnimal, teamHorror :: TeamContinuity
teamCompetitor = TeamContinuity 2
teamCivilian = TeamContinuity 3
teamConvict = TeamContinuity 4
teamMonster = TeamContinuity 5
teamAnimal = TeamContinuity 6
teamHorror = TeamContinuity 7

-- * Content

-- ** teamExplorer

playerExplorer = FactionKind
  { fname = "Explorer"
  , ffreq = []
  , fteam = teamExplorer
  , fgroups = [HERO]
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHeroLong
  , fhasGender = True
  , finitDoctrine = TExplore
  , fspawnsFast = False
  , fhasPointman = True
  , fhasUI = True
  , finitUnderAI = False
  }
playerExplorerShort = playerExplorer {fhiCondPoly = hiHeroShort}
playerExplorerNoEscape = playerExplorer { fcanEscape = False
                                        , fhiCondPoly = hiHeroMedium }
playerExplorerMedium = playerExplorer { fhiCondPoly = hiHeroMedium }
playerExplorerTrapped = playerExplorer { fcanEscape = False
                                       , fhiCondPoly = hiHeroLong }
playerExplorerAutomated = playerExplorer
  { ffreq = []
  , fhasUI = False
  , finitUnderAI = True
  }
playerExplorerAutomatedTrapped =
  playerExplorerAutomated { fcanEscape = False
                          , fhiCondPoly = hiHeroLong }

-- ** teamCompetitor, symmetric opponets of teamExplorer

playerCompetitor = playerExplorer
  { fname = "Indigo Researcher"
  , ffreq = []
  , fteam = teamCompetitor
  , fhasUI = False
  , finitUnderAI = True
  }
playerCompetitorShort = playerCompetitor { fname = "Indigo Founder"  -- early
                                         , fhiCondPoly = hiHeroShort }
playerCompetitorNoEscape = playerCompetitor { fcanEscape = False
                                            , fhiCondPoly = hiHeroMedium }

-- ** teamCivilian

playerCivilian = FactionKind
  { fname = "Civilian"
  , ffreq = []
  , fteam = teamCivilian
  , fgroups = [HERO, CIVILIAN]
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = hiHeroMedium
  , fhasGender = True
  , finitDoctrine = TPatrol
  , fspawnsFast = False
  , fhasPointman = False  -- unorganized
  , fhasUI = False
  , finitUnderAI = True
  }

-- ** teamConvict, different demographics

playerConvict =
  playerCivilian { fname = "Hunam Convict"
                 , ffreq = []
                 , fteam = teamConvict
                 , fhasPointman = True  -- convicts organize better
                 , finitUnderAI = True }

-- ** teamMonster

playerMonster = FactionKind
  { fname = "Monster Hive"
  , ffreq = []
  , fteam = teamMonster
  , fgroups = [MONSTER, MOBILE_MONSTER]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TExplore
  , fspawnsFast = True
  , fhasPointman = True
  , fhasUI = False
  , finitUnderAI = True
  }
-- This has continuity @teamMonster@, despite being playable.
playerAntiMonster = playerMonster
  { ffreq = []
  , fhasUI = True
  , finitUnderAI = False
  }
playerAntiMonsterCaptive = playerAntiMonster {fneverEmpty = True}
-- More flavour and special backstory, but the same team.
playerMonsterTourist =
  playerAntiMonster
    { fname = "Monster Tourist Office"
    , ffreq = []
    , fcanEscape = True
    , fneverEmpty = True  -- no spawning
    , fhiCondPoly = hiHeroMedium
    , finitDoctrine = TFollow  -- follow-the-guide, as tourists do
    , fspawnsFast = False  -- on a trip, so no spawning
    , finitUnderAI = False }
playerMonsterTouristPassive =
  playerMonsterTourist { fhasUI = False
                       , finitUnderAI = True }
playerMonsterCaptive = playerMonster {fneverEmpty = True}
playerMonsterCaptiveNarrating = playerAntiMonsterCaptive {fhasUI = True}

-- ** teamAnimal

playerAnimal = FactionKind
  { fname = "Animal Kingdom"
  , ffreq = []
  , fteam = teamAnimal
  , fgroups = [ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, SCAVENGER]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TRoam  -- can't pick up, so no point exploring
  , fspawnsFast = True
  , fhasPointman = False
  , fhasUI = False
  , finitUnderAI = True
  }
-- These two differ from outside, but share information and boasting
-- about them tends to be general, too.
playerAnimalMagnificent =
  playerAnimal { fname = "Animal Magnificent Specimen Variety"
               , ffreq = []
               , fneverEmpty = True }
playerAnimalExquisite =
  playerAnimal { fname = "Animal Exquisite Herds and Packs Galore"
               , ffreq = []
               , fteam = teamHorror
                   -- in the same mode as @playerAnimalMagnificent@, so borrow
                   -- identity from horrors to avoid a clash
               , fneverEmpty = True }
playerAnimalCaptive = playerAnimal {fneverEmpty = True}
playerAnimalNarrating = playerAnimal {fhasUI = True}
playerAnimalMagnificentNarrating =
  playerAnimalMagnificent { fhasPointman = True
                          , fhasUI = True
                          , finitUnderAI = False }
playerAnimalCaptiveNarrating = playerAnimalCaptive {fhasUI = True}

-- ** teamHorror, not much of a continuity intended, but can't be ignored

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
playerHorror = FactionKind
  { fname = "Horror Den"
  , ffreq = []
  , fteam = teamHorror
  , fgroups = [IK.HORROR]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasGender = False
  , finitDoctrine = TPatrol  -- disoriented
  , fspawnsFast = False
  , fhasPointman = False
  , fhasUI = False
  , finitUnderAI = True
  }
