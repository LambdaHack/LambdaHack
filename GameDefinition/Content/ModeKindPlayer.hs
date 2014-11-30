-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerSoldier, playerAntiHero, playerCivilian, playerMonster
  , playerMobileMonster,  playerAntiMonster, playerAnimal, playerMobileAnimal
  , playerHorror
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

playerHero, playerSoldier, playerAntiHero, playerCivilian, playerMonster, playerMobileMonster, playerAntiMonster, playerAnimal, playerMobileAnimal, playerHorror :: Player Dice

playerHero = Player
  { fname = "Adventurer Party"
  , fgroup = "hero"
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhasNumbers = True
  , fhasGender = True
  , ftactic = TExplore
  , fentryLevel = -1
  , finitialActors = 3
  , fleaderMode = LeaderUI $ AutoLeader False False
  , fhasUI = True
  }

playerSoldier = playerHero
  { fname = "Armed Adventurer Party"
  , fgroup = "soldier"
  }

playerAntiHero = playerHero
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsOther = unitSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhasNumbers = False
  , fhasGender = True
  , ftactic = TPatrol
  , fentryLevel = -1
  , finitialActors = d 2 + 1
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Monster Hive"
  , fgroup = "monster"
  , fskillsOther = unitSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TExplore
  , fentryLevel = -3
  , finitialActors = 1  -- an explorer, not a nose, ensured via irarity
  , fleaderMode = LeaderAI $ AutoLeader True True
  , fhasUI = False
  }

playerMobileMonster = playerMonster

playerAntiMonster = playerMonster
  { fhasUI = True
  , fleaderMode = LeaderUI $ AutoLeader True True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsOther = unitSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TRoam  -- can't pick up, so no point exploring
  , fentryLevel = -1  -- fun from the start; don't cumulate with monsters
  , finitialActors = 1 + d 2
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

playerMobileAnimal = playerAnimal
  { fgroup = "mobile animal" }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a duel game between two hero players land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
-- Actors that can be summoned should have "horror" in their @ifreq@ set.
playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsOther = unitSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TPatrol  -- disoriented
  , fentryLevel = -3
  , finitialActors = 0
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

minusHundred, meleeAdjacent, _meleeAndRanged :: Skills

-- To make sure weak items can't override move-only-leader, etc.
minusHundred = EM.fromList $ zip [minBound..maxBound] [-100, -100..]

meleeAdjacent = addSkills minusHundred
                $ EM.fromList $ zip [AbWait, AbMelee] [101, 101..]

-- Melee and reaction fire.
_meleeAndRanged = addSkills minusHundred
                  $ EM.fromList $ zip [AbWait, AbMelee, AbProject] [101, 101..]
