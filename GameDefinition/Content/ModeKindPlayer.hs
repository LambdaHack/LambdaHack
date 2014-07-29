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
  , ffreq = [("hero", 1)]
  , fskillsLeader = allSkills
  , fskillsOther  = meleeAdjacent
  , fisSpawn = False
  , fisHero = True
  , fentry = -1
  , finitial = 3
  , fleader = True
  , fisAI = False
  , fisUI = True
  }

playerAntiHero = playerHero
  { fisAI = True
  , fisUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , ffreq = [("civilian", 1)]
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills  -- not coordinated by any leadership
  , fisSpawn = False
  , fisHero = False
  , fentry = -1
  , finitial = 3
  , fleader = False  -- unorganized
  , fisAI = True
  , fisUI = False
  }

playerMonster = Player
  { fname = "Monster Hive"
  , fgroup = "monster"
  , ffreq = [("monster", 1)]
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = True
  , fisHero = False
  , fentry = -3
  , finitial = 5
  , fleader = True
  , fisAI = True
  , fisUI = False
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , ffreq = [("animal", 1)]
  , fskillsLeader = animalSkills
  , fskillsOther  = animalSkills
  , fisSpawn = True
  , fisHero = False
  , fentry = -2
  , finitial = 3
  , fleader = False
  , fisAI = True
  , fisUI = False
  }

playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , ffreq = [("horror", 1)]
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = False
  , fisHero = False
  , fentry = -1
  , finitial = 0
  , fleader = False
  , fisAI = True
  , fisUI = False
  }

meleeAdjacent, _meleeAndRanged, animalSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

allSkills = unitSkills
