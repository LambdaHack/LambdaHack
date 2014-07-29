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
  { playerName = "Adventurer Party"
  , playerFaction = "hero"
  , ffreq         = [("hero", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = meleeAdjacent
  , playerIsSpawn = False
  , playerIsHero = True
  , playerEntry = -1
  , playerInitial = 3
  , playerLeader = True
  , playerAI = False
  , playerUI = True
  }

playerAntiHero = playerHero
  { playerAI = True
  , playerUI = False
  }

playerCivilian = Player
  { playerName = "Civilian Crowd"
  , playerFaction = "civilian"
  , ffreq         = [("civilian", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills  -- not coordinated by any leadership
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = -1
  , playerInitial = 3
  , playerLeader = False  -- unorganized
  , playerAI = True
  , playerUI = False
  }

playerMonster = Player
  { playerName = "Monster Hive"
  , playerFaction = "monster"
  , ffreq         = [("monster", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = -3
  , playerInitial = 5
  , playerLeader = True
  , playerAI = True
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , ffreq         = [("animal", 1)]
  , fSkillsLeader = animalSkills
  , fSkillsOther  = animalSkills
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = -2
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , ffreq         = [("horror", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = -1
  , playerInitial = 0
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

meleeAdjacent, _meleeAndRanged, animalSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

allSkills = unitSkills
