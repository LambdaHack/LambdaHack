-- | Definitions of kinds of factions present in a game, both human
-- and computer-controlled.
module Content.FactionKind
  ( -- * Group name patterns
    pattern EXPLORER_REPRESENTATIVE, pattern EXPLORER_SHORT, pattern EXPLORER_NO_ESCAPE, pattern EXPLORER_MEDIUM, pattern EXPLORER_TRAPPED, pattern EXPLORER_AUTOMATED, pattern EXPLORER_AUTOMATED_TRAPPED, pattern EXPLORER_CAPTIVE, pattern EXPLORER_PACIFIST, pattern COMPETITOR_REPRESENTATIVE, pattern COMPETITOR_SHORT, pattern COMPETITOR_NO_ESCAPE, pattern CIVILIAN_REPRESENTATIVE, pattern CONVICT_REPRESENTATIVE, pattern MONSTER_REPRESENTATIVE, pattern MONSTER_ANTI, pattern MONSTER_ANTI_CAPTIVE, pattern MONSTER_ANTI_PACIFIST, pattern MONSTER_TOURIST, pattern MONSTER_TOURIST_PASSIVE, pattern MONSTER_CAPTIVE, pattern MONSTER_CAPTIVE_NARRATING, pattern ANIMAL_REPRESENTATIVE, pattern ANIMAL_MAGNIFICENT, pattern ANIMAL_EXQUISITE, pattern ANIMAL_CAPTIVE, pattern ANIMAL_NARRATING, pattern ANIMAL_MAGNIFICENT_NARRATING, pattern ANIMAL_CAPTIVE_NARRATING, pattern HORROR_REPRESENTATIVE, pattern HORROR_CAPTIVE, pattern HORROR_PACIFIST
  , pattern REPRESENTATIVE
  , groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

import Content.ItemKindActor
import Content.ItemKindOrgan

-- * Group name patterns

groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton =
       [EXPLORER_REPRESENTATIVE, EXPLORER_SHORT, EXPLORER_NO_ESCAPE, EXPLORER_MEDIUM, EXPLORER_TRAPPED, EXPLORER_AUTOMATED, EXPLORER_AUTOMATED_TRAPPED, EXPLORER_CAPTIVE, EXPLORER_PACIFIST, COMPETITOR_REPRESENTATIVE, COMPETITOR_SHORT, COMPETITOR_NO_ESCAPE, CIVILIAN_REPRESENTATIVE, CONVICT_REPRESENTATIVE, MONSTER_REPRESENTATIVE, MONSTER_ANTI, MONSTER_ANTI_CAPTIVE, MONSTER_ANTI_PACIFIST, MONSTER_TOURIST, MONSTER_TOURIST_PASSIVE, MONSTER_CAPTIVE, MONSTER_CAPTIVE_NARRATING, ANIMAL_REPRESENTATIVE, ANIMAL_MAGNIFICENT, ANIMAL_EXQUISITE, ANIMAL_CAPTIVE, ANIMAL_NARRATING, ANIMAL_MAGNIFICENT_NARRATING, ANIMAL_CAPTIVE_NARRATING, HORROR_REPRESENTATIVE, HORROR_CAPTIVE, HORROR_PACIFIST]

pattern EXPLORER_REPRESENTATIVE, EXPLORER_SHORT, EXPLORER_NO_ESCAPE, EXPLORER_MEDIUM, EXPLORER_TRAPPED, EXPLORER_AUTOMATED, EXPLORER_AUTOMATED_TRAPPED, EXPLORER_CAPTIVE, EXPLORER_PACIFIST, COMPETITOR_REPRESENTATIVE, COMPETITOR_SHORT, COMPETITOR_NO_ESCAPE, CIVILIAN_REPRESENTATIVE, CONVICT_REPRESENTATIVE, MONSTER_REPRESENTATIVE, MONSTER_ANTI, MONSTER_ANTI_CAPTIVE, MONSTER_ANTI_PACIFIST, MONSTER_TOURIST, MONSTER_TOURIST_PASSIVE, MONSTER_CAPTIVE, MONSTER_CAPTIVE_NARRATING, ANIMAL_REPRESENTATIVE, ANIMAL_MAGNIFICENT, ANIMAL_EXQUISITE, ANIMAL_CAPTIVE, ANIMAL_NARRATING, ANIMAL_MAGNIFICENT_NARRATING, ANIMAL_CAPTIVE_NARRATING, HORROR_REPRESENTATIVE, HORROR_CAPTIVE, HORROR_PACIFIST :: GroupName FactionKind

groupNames :: [GroupName FactionKind]
groupNames = [REPRESENTATIVE]

pattern REPRESENTATIVE :: GroupName FactionKind

pattern REPRESENTATIVE = GroupName "representative"
pattern EXPLORER_REPRESENTATIVE = GroupName "explorer"
pattern EXPLORER_SHORT = GroupName "explorer short"
pattern EXPLORER_NO_ESCAPE = GroupName "explorer no escape"
pattern EXPLORER_MEDIUM = GroupName "explorer medium"
pattern EXPLORER_TRAPPED = GroupName "explorer trapped"
pattern EXPLORER_AUTOMATED = GroupName "explorer automated"
pattern EXPLORER_AUTOMATED_TRAPPED = GroupName "explorer automated trapped"
pattern EXPLORER_CAPTIVE = GroupName "explorer captive"
pattern EXPLORER_PACIFIST = GroupName "explorer pacifist"
pattern COMPETITOR_REPRESENTATIVE = GroupName "competitor"
pattern COMPETITOR_SHORT = GroupName "competitor short"
pattern COMPETITOR_NO_ESCAPE = GroupName "competitor no escape"
pattern CIVILIAN_REPRESENTATIVE = GroupName "civilian"
pattern CONVICT_REPRESENTATIVE = GroupName "convict"
pattern MONSTER_REPRESENTATIVE = GroupName "monster"
pattern MONSTER_ANTI = GroupName "monster anti"
pattern MONSTER_ANTI_CAPTIVE = GroupName "monster anti captive"
pattern MONSTER_ANTI_PACIFIST = GroupName "monster anti pacifist"
pattern MONSTER_TOURIST = GroupName "monster tourist"
pattern MONSTER_TOURIST_PASSIVE = GroupName "monster tourist passive"
pattern MONSTER_CAPTIVE = GroupName "monster captive"
pattern MONSTER_CAPTIVE_NARRATING = GroupName "monster captive narrating"
pattern ANIMAL_REPRESENTATIVE = GroupName "animal"
pattern ANIMAL_MAGNIFICENT = GroupName "animal magnificent"
pattern ANIMAL_EXQUISITE = GroupName "animal exquisite"
pattern ANIMAL_CAPTIVE = GroupName "animal captive"
pattern ANIMAL_NARRATING = GroupName "animal narrating"
pattern ANIMAL_MAGNIFICENT_NARRATING = GroupName "animal magnificent narrating"
pattern ANIMAL_CAPTIVE_NARRATING = GroupName "animal captive narrating"
pattern HORROR_REPRESENTATIVE = GroupName "horror"
pattern HORROR_CAPTIVE = GroupName "horror captive"
pattern HORROR_PACIFIST = GroupName "horror pacifist"

-- * Teams

teamCompetitor, teamCivilian, teamConvict, teamMonster, teamAnimal, teamHorror, teamOther :: TeamContinuity
teamCompetitor = TeamContinuity 2
teamCivilian = TeamContinuity 3
teamConvict = TeamContinuity 4
teamMonster = TeamContinuity 5
teamAnimal = TeamContinuity 6
teamHorror = TeamContinuity 7
teamOther = TeamContinuity 10

-- * Content

content :: [FactionKind]
content = [factExplorer, factExplorerShort, factExplorerNoEscape, factExplorerMedium, factExplorerTrapped, factExplorerAutomated, factExplorerAutomatedTrapped, factExplorerCaptive, factExplorerPacifist, factCompetitor, factCompetitorShort, factCompetitorNoEscape, factCivilian, factConvict, factMonster, factMonsterAnti, factMonsterAntiCaptive, factMonsterAntiPacifist, factMonsterTourist, factMonsterTouristPassive, factMonsterCaptive, factMonsterCaptiveNarrating, factAnimal, factAnimalMagnificent, factAnimalExquisite, factAnimalCaptive, factAnimalNarrating, factAnimalMagnificentNarrating, factAnimalCaptiveNarrating, factHorror, factHorrorCaptive, factHorrorPacifist]

factExplorer,            factExplorerShort, factExplorerNoEscape, factExplorerMedium, factExplorerTrapped, factExplorerAutomated, factExplorerAutomatedTrapped, factExplorerCaptive, factExplorerPacifist, factCompetitor, factCompetitorShort, factCompetitorNoEscape, factCivilian, factConvict, factMonster, factMonsterAnti, factMonsterAntiCaptive, factMonsterAntiPacifist, factMonsterTourist, factMonsterTouristPassive, factMonsterCaptive, factMonsterCaptiveNarrating, factAnimal, factAnimalMagnificent, factAnimalExquisite, factAnimalCaptive, factAnimalNarrating, factAnimalMagnificentNarrating, factAnimalCaptiveNarrating, factHorror, factHorrorCaptive, factHorrorPacifist :: FactionKind

-- * Content

-- ** teamExplorer

factExplorer = FactionKind
  { fname = "Explorer"
  , ffreq = [(EXPLORER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamExplorer
  , fgroups = [(HERO, 100)]  -- don't spam the escapists, etc., in description
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
  , fenemyTeams = [teamCompetitor, teamMonster, teamAnimal, teamHorror]
  , falliedTeams = []
  }
factExplorerShort = factExplorer
  { ffreq = [(EXPLORER_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = [teamMonster, teamAnimal, teamHorror]
  }
factExplorerNoEscape = factExplorer
  { ffreq = [(EXPLORER_NO_ESCAPE, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroMedium
  }
factExplorerMedium = factExplorer
  { ffreq = [(EXPLORER_MEDIUM, 1)]
  , fhiCondPoly = hiHeroMedium
  }
factExplorerTrapped = factExplorer
  { ffreq = [(EXPLORER_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }
factExplorerAutomated = factExplorer
  { ffreq = [(EXPLORER_AUTOMATED, 1)]
  , fhasUI = False
  , finitUnderAI = True
  }
factExplorerAutomatedTrapped = factExplorerAutomated
  { ffreq = [(EXPLORER_AUTOMATED_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }
factExplorerCaptive = factExplorer
  { ffreq = [(EXPLORER_CAPTIVE, 1)]
  , fneverEmpty = True  -- already there
  }
factExplorerPacifist = factExplorerCaptive
  { ffreq = [(EXPLORER_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }

-- ** teamCompetitor, symmetric opponents of teamExplorer

factCompetitor = factExplorer
  { fname = "Indigo Researcher"
  , ffreq = [(COMPETITOR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCompetitor
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = [teamExplorer, teamMonster, teamAnimal, teamHorror]
  , falliedTeams = []
  }
factCompetitorShort = factCompetitor
  { fname = "Indigo Founder"  -- early
  , ffreq = [(COMPETITOR_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = [teamMonster, teamAnimal, teamHorror]
  }
factCompetitorNoEscape = factCompetitor
  { ffreq = [(COMPETITOR_NO_ESCAPE, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroMedium
  }

-- ** teamCivilian

factCivilian = FactionKind
  { fname = "Civilian"
  , ffreq = [(CIVILIAN_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCivilian
  , fgroups = [(HERO, 100), (CIVILIAN, 100)]  -- symmetric vs player
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
  , fenemyTeams = [teamMonster, teamAnimal, teamHorror]
  , falliedTeams = []
  }

-- ** teamConvict, different demographics

factConvict = factCivilian
  { fname = "Hunam Convict"
  , ffreq = [(CONVICT_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamConvict
  , fhasPointman = True  -- convicts organize better
  , finitUnderAI = True
  , fenemyTeams = [teamMonster, teamAnimal, teamHorror]
  , falliedTeams = []
  }

-- ** teamMonster

factMonster = FactionKind
  { fname = "Monster Hive"
  , ffreq = [(MONSTER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamMonster
  , fgroups = [ (MONSTER, 100)
              , (MOBILE_MONSTER, 1) ]
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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = [teamAnimal]
  }
-- This has continuity @teamMonster@, despite being playable.
factMonsterAnti = factMonster
  { ffreq = [(MONSTER_ANTI, 1)]
  , fhasUI = True
  , finitUnderAI = False
  }
factMonsterAntiCaptive = factMonsterAnti
  { ffreq = [(MONSTER_ANTI_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factMonsterAntiPacifist = factMonsterAntiCaptive
  { ffreq = [(MONSTER_ANTI_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }
-- More flavour and special backstory, but the same team.
factMonsterTourist = factMonsterAnti
  { fname = "Monster Tourist Office"
  , ffreq = [(MONSTER_TOURIST, 1)]
  , fcanEscape = True
  , fneverEmpty = True  -- no spawning
  , fhiCondPoly = hiHeroMedium
  , finitDoctrine = TFollow  -- follow-the-guide, as tourists do
  , fspawnsFast = False  -- on a trip, so no spawning
  , finitUnderAI = False
  , fenemyTeams =
      [teamAnimal, teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
factMonsterTouristPassive = factMonsterTourist
  { ffreq = [(MONSTER_TOURIST_PASSIVE, 1)]
  , fhasUI = False
  , finitUnderAI = True
  }
factMonsterCaptive = factMonster
  { ffreq = [(MONSTER_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factMonsterCaptiveNarrating = factMonsterAntiCaptive
  { ffreq = [(MONSTER_CAPTIVE_NARRATING, 1)]
  , fhasUI = True
  }

-- ** teamAnimal

factAnimal = FactionKind
  { fname = "Animal Kingdom"
  , ffreq = [(ANIMAL_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamAnimal
  , fgroups = [ (ANIMAL, 100), (INSECT, 100), (GEOPHENOMENON, 100)
                   -- only the distinct enough ones
              , (MOBILE_ANIMAL, 1), (IMMOBILE_ANIMAL, 1), (SCAVENGER, 1) ]
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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = [teamMonster]
  }
-- These two differ from outside, but share information and boasting
-- about them tends to be general, too.
factAnimalMagnificent = factAnimal
  { fname = "Animal Magnificent Specimen Variety"
  , ffreq = [(ANIMAL_MAGNIFICENT, 1)]
  , fneverEmpty = True
  , fenemyTeams =
      [teamMonster, teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
factAnimalExquisite = factAnimal
  { fname = "Animal Exquisite Herds and Packs Galore"
  , ffreq = [(ANIMAL_EXQUISITE, 1)]
  , fteam = teamOther
      -- in the same mode as @factAnimalMagnificent@, so borrow
      -- identity from horrors to avoid a clash
  , fneverEmpty = True
  , fenemyTeams =
      [teamMonster, teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
factAnimalCaptive = factAnimal
  { ffreq = [(ANIMAL_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factAnimalNarrating = factAnimal
  { ffreq = [(ANIMAL_NARRATING, 1)]
  , fhasUI = True
  }
factAnimalMagnificentNarrating = factAnimalMagnificent
  { ffreq = [(ANIMAL_MAGNIFICENT_NARRATING, 1)]
  , fhasPointman = True
  , fhasUI = True
  , finitUnderAI = False
  }
factAnimalCaptiveNarrating = factAnimalCaptive
  { ffreq = [(ANIMAL_CAPTIVE_NARRATING, 1)]
  , fhasUI = True
  }

-- ** teamHorror, not much of a continuity intended, but can't be ignored

-- | A special faction, for summoned actors that don't belong to any
-- of the main factions of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror faction should be added to host them.
factHorror = FactionKind
  { fname = "Horror Den"
  , ffreq = [(HORROR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamHorror
  , fgroups = [(IK.HORROR, 100)]
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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
factHorrorCaptive = factHorror
  { ffreq = [(HORROR_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factHorrorPacifist = factHorrorCaptive
  { ffreq = [(HORROR_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }
