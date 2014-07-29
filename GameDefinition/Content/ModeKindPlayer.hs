-- | Definition of basic players for LambdaHack.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian, playerMonster, playerAnimal
  , playerHorror
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Content.ModeKind

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAnimal, playerHorror :: Player

playerHero = Player
  { fname = "Adventurer Party"
  , fgroup = "hero"
  , fskillsLeader = allSkills
  , fskillsOther  = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhasNumbers = True
  , fhasGender = True
  , fentryLevel = -1
  , finitialActors = 3
  , fhasLeader = True
  , fisAI = False
  , fhasUI = True
  }

playerAntiHero = playerHero
  { fisAI = True
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhasNumbers = False
  , fhasGender = True
  , fentryLevel = -1
  , finitialActors = 3
  , fhasLeader = False  -- unorganized
  , fisAI = True
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Monster Hive"
  , fgroup = "monster"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , fentryLevel = -3
  , finitialActors = 5
  , fhasLeader = True
  , fisAI = True
  , fhasUI = False
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsLeader = animalSkills
  , fskillsOther  = animalSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , fentryLevel = -2
  , finitialActors = 3
  , fhasLeader = False
  , fisAI = True
  , fhasUI = False
  }

playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , fentryLevel = -1
  , finitialActors = 0
  , fhasLeader = False
  , fisAI = True
  , fhasUI = False
  }

meleeAdjacent, _meleeAndRanged, animalSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

allSkills = unitSkills
