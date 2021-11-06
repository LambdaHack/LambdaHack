-- | Basic players definitions.
module Content.FactionKind
  ( -- * Group name patterns
    pattern EXPLORER_REPRESENTATIVE, pattern EXPLORER_SHORT, pattern EXPLORER_NO_ESCAPE, pattern EXPLORER_MEDIUM, pattern EXPLORER_TRAPPED, pattern EXPLORER_AUTOMATED, pattern EXPLORER_AUTOMATED_TRAPPED, pattern COMPETITOR_REPRESENTATIVE, pattern COMPETITOR_SHORT, pattern COMPETITOR_NO_ESCAPE, pattern CIVILIAN_REPRESENTATIVE, pattern CONVICT_REPRESENTATIVE, pattern MONSTER_REPRESENTATIVE, pattern MONSTER_ANTI, pattern MONSTER_ANTI_CAPTIVE, pattern MONSTER_TOURIST, pattern MONSTER_TOURIST_PASSIVE, pattern MONSTER_CAPTIVE, pattern MONSTER_CAPTIVE_NARRATING, pattern ANIMAL_REPRESENTATIVE, pattern ANIMAL_MAGNIFICENT, pattern ANIMAL_EXQUISITE, pattern ANIMAL_CAPTIVE, pattern ANIMAL_NARRATING, pattern ANIMAL_MAGNIFICENT_NARRATING, pattern ANIMAL_CAPTIVE_NARRATING, pattern HORROR_REPRESENTATIVE
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

import           Content.ItemKindActor
import           Content.ItemKindOrgan
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

-- * Group name patterns

groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton =
       [EXPLORER_REPRESENTATIVE, EXPLORER_SHORT, EXPLORER_NO_ESCAPE, EXPLORER_MEDIUM, EXPLORER_TRAPPED, EXPLORER_AUTOMATED, EXPLORER_AUTOMATED_TRAPPED, COMPETITOR_REPRESENTATIVE, COMPETITOR_SHORT, COMPETITOR_NO_ESCAPE, CIVILIAN_REPRESENTATIVE, CONVICT_REPRESENTATIVE, MONSTER_REPRESENTATIVE, MONSTER_ANTI, MONSTER_ANTI_CAPTIVE, MONSTER_TOURIST, MONSTER_TOURIST_PASSIVE, MONSTER_CAPTIVE, MONSTER_CAPTIVE_NARRATING, ANIMAL_REPRESENTATIVE, ANIMAL_MAGNIFICENT, ANIMAL_EXQUISITE, ANIMAL_CAPTIVE, ANIMAL_NARRATING, ANIMAL_MAGNIFICENT_NARRATING, ANIMAL_CAPTIVE_NARRATING, HORROR_REPRESENTATIVE]

pattern EXPLORER_REPRESENTATIVE, EXPLORER_SHORT, EXPLORER_NO_ESCAPE, EXPLORER_MEDIUM, EXPLORER_TRAPPED, EXPLORER_AUTOMATED, EXPLORER_AUTOMATED_TRAPPED, COMPETITOR_REPRESENTATIVE, COMPETITOR_SHORT, COMPETITOR_NO_ESCAPE, CIVILIAN_REPRESENTATIVE, CONVICT_REPRESENTATIVE, MONSTER_REPRESENTATIVE, MONSTER_ANTI, MONSTER_ANTI_CAPTIVE, MONSTER_TOURIST, MONSTER_TOURIST_PASSIVE, MONSTER_CAPTIVE, MONSTER_CAPTIVE_NARRATING, ANIMAL_REPRESENTATIVE, ANIMAL_MAGNIFICENT, ANIMAL_EXQUISITE, ANIMAL_CAPTIVE, ANIMAL_NARRATING, ANIMAL_MAGNIFICENT_NARRATING, ANIMAL_CAPTIVE_NARRATING, HORROR_REPRESENTATIVE :: GroupName FactionKind

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
pattern COMPETITOR_REPRESENTATIVE = GroupName "competitor"
pattern COMPETITOR_SHORT = GroupName "competitor short"
pattern COMPETITOR_NO_ESCAPE = GroupName "competitor no escape"
pattern CIVILIAN_REPRESENTATIVE = GroupName "civilian"
pattern CONVICT_REPRESENTATIVE = GroupName "convict"
pattern MONSTER_REPRESENTATIVE = GroupName "monster"
pattern MONSTER_ANTI = GroupName "monster anti"
pattern MONSTER_ANTI_CAPTIVE = GroupName "monster anti captive"
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

-- * Teams

teamCompetitor, teamCivilian, teamConvict, teamMonster, teamAnimal, teamHorror :: TeamContinuity
teamCompetitor = TeamContinuity 2
teamCivilian = TeamContinuity 3
teamConvict = TeamContinuity 4
teamMonster = TeamContinuity 5
teamAnimal = TeamContinuity 6
teamHorror = TeamContinuity 7

-- * Content

content :: [FactionKind]
content = [playerExplorer, playerExplorerShort, playerExplorerNoEscape, playerExplorerMedium, playerExplorerTrapped, playerExplorerAutomated, playerExplorerAutomatedTrapped, playerCompetitor, playerCompetitorShort, playerCompetitorNoEscape, playerCivilian, playerConvict, playerMonster, playerAntiMonster, playerAntiMonsterCaptive, playerMonsterTourist, playerMonsterTouristPassive, playerMonsterCaptive, playerMonsterCaptiveNarrating, playerAnimal, playerAnimalMagnificent, playerAnimalExquisite, playerAnimalCaptive, playerAnimalNarrating, playerAnimalMagnificentNarrating, playerAnimalCaptiveNarrating, playerHorror]

playerExplorer,            playerExplorerShort, playerExplorerNoEscape, playerExplorerMedium, playerExplorerTrapped, playerExplorerAutomated, playerExplorerAutomatedTrapped, playerCompetitor, playerCompetitorShort, playerCompetitorNoEscape, playerCivilian, playerConvict, playerMonster, playerAntiMonster, playerAntiMonsterCaptive, playerMonsterTourist, playerMonsterTouristPassive, playerMonsterCaptive, playerMonsterCaptiveNarrating, playerAnimal, playerAnimalMagnificent, playerAnimalExquisite, playerAnimalCaptive, playerAnimalNarrating, playerAnimalMagnificentNarrating, playerAnimalCaptiveNarrating, playerHorror :: FactionKind

-- * Content

-- ** teamExplorer

playerExplorer = FactionKind
  { fname = "Explorer"
  , ffreq = [(EXPLORER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
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
playerExplorerShort = playerExplorer
  { ffreq = [(EXPLORER_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  }
playerExplorerNoEscape = playerExplorer
  { ffreq = [(EXPLORER_NO_ESCAPE, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroMedium
  }
playerExplorerMedium = playerExplorer
  { ffreq = [(EXPLORER_MEDIUM, 1)]
  , fhiCondPoly = hiHeroMedium
  }
playerExplorerTrapped = playerExplorer
  { ffreq = [(EXPLORER_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }
playerExplorerAutomated = playerExplorer
  { ffreq = [(EXPLORER_AUTOMATED, 1)]
  , fhasUI = False
  , finitUnderAI = True
  }
playerExplorerAutomatedTrapped = playerExplorerAutomated
  { ffreq = [(EXPLORER_AUTOMATED_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }

-- ** teamCompetitor, symmetric opponets of teamExplorer

playerCompetitor = playerExplorer
  { fname = "Indigo Researcher"
  , ffreq = [(COMPETITOR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCompetitor
  , fhasUI = False
  , finitUnderAI = True
  }
playerCompetitorShort = playerCompetitor
  { fname = "Indigo Founder"  -- early
  , ffreq = [(COMPETITOR_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  }
playerCompetitorNoEscape = playerCompetitor
  { ffreq = [(COMPETITOR_NO_ESCAPE, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroMedium
  }

-- ** teamCivilian

playerCivilian = FactionKind
  { fname = "Civilian"
  , ffreq = [(CIVILIAN_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
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

playerConvict = playerCivilian
  { fname = "Hunam Convict"
  , ffreq = [(CONVICT_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamConvict
  , fhasPointman = True  -- convicts organize better
  , finitUnderAI = True
  }

-- ** teamMonster

playerMonster = FactionKind
  { fname = "Monster Hive"
  , ffreq = [(MONSTER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
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
  { ffreq = [(MONSTER_ANTI, 1)]
  , fhasUI = True
  , finitUnderAI = False
  }
playerAntiMonsterCaptive = playerAntiMonster
  {ffreq = [(MONSTER_ANTI_CAPTIVE, 1)]
  , fneverEmpty = True
  }
-- More flavour and special backstory, but the same team.
playerMonsterTourist = playerAntiMonster
  { fname = "Monster Tourist Office"
  , ffreq = [(MONSTER_TOURIST, 1)]
  , fcanEscape = True
  , fneverEmpty = True  -- no spawning
  , fhiCondPoly = hiHeroMedium
  , finitDoctrine = TFollow  -- follow-the-guide, as tourists do
  , fspawnsFast = False  -- on a trip, so no spawning
  , finitUnderAI = False
  }
playerMonsterTouristPassive = playerMonsterTourist
  { ffreq = [(MONSTER_TOURIST_PASSIVE, 1)]
  , fhasUI = False
  , finitUnderAI = True
  }
playerMonsterCaptive = playerMonster
  { ffreq = [(MONSTER_CAPTIVE, 1)]
  , fneverEmpty = True
  }
playerMonsterCaptiveNarrating = playerAntiMonsterCaptive
  { ffreq = [(MONSTER_CAPTIVE_NARRATING, 1)]
  , fhasUI = True
  }

-- ** teamAnimal

playerAnimal = FactionKind
  { fname = "Animal Kingdom"
  , ffreq = [(ANIMAL_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
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
playerAnimalMagnificent = playerAnimal
  { fname = "Animal Magnificent Specimen Variety"
  , ffreq = [(ANIMAL_MAGNIFICENT, 1)]
  , fneverEmpty = True
  }
playerAnimalExquisite = playerAnimal
  { fname = "Animal Exquisite Herds and Packs Galore"
  , ffreq = [(ANIMAL_EXQUISITE, 1)]
  , fteam = teamHorror
      -- in the same mode as @playerAnimalMagnificent@, so borrow
      -- identity from horrors to avoid a clash
  , fneverEmpty = True
  }
playerAnimalCaptive = playerAnimal
  { ffreq = [(ANIMAL_CAPTIVE, 1)]
  , fneverEmpty = True
  }
playerAnimalNarrating = playerAnimal
  { ffreq = [(ANIMAL_NARRATING, 1)]
  , fhasUI = True
  }
playerAnimalMagnificentNarrating = playerAnimalMagnificent
  { ffreq = [(ANIMAL_MAGNIFICENT_NARRATING, 1)]
  , fhasPointman = True
  , fhasUI = True
  , finitUnderAI = False
  }
playerAnimalCaptiveNarrating = playerAnimalCaptive
  { ffreq = [(ANIMAL_CAPTIVE_NARRATING, 1)]
  , fhasUI = True
  }

-- ** teamHorror, not much of a continuity intended, but can't be ignored

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
playerHorror = FactionKind
  { fname = "Horror Den"
  , ffreq = [(HORROR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
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
