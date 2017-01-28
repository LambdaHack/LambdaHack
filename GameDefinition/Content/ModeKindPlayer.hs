-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian
  , playerMonster, playerAntiMonster, playerAnimal
  , playerHorror
  , hiHero, hiDweller, hiRaid
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAntiMonster, playerAnimal, playerHorror :: Player Dice

playerHero = Player
  { fname = "Explorer Party"
  , fgroup = "hero"
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHero
  , fhasNumbers = True
  , fhasGender = True
  , ftactic = TExplore
  , fentryLevel = -1
  , finitialActors = [(3, "hero")]
  , fleaderMode = LeaderUI $ AutoLeader False False
  , fhasUI = True
  }

playerAntiHero = playerHero
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = hiDweller
  , fhasNumbers = False
  , fhasGender = True
  , ftactic = TPatrol
  , fentryLevel = -1
  , finitialActors = [(d 2 + 1, "civilian")]
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Monster Hive"
  , fgroup = "monster"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TExplore
  , fentryLevel = -4
  , finitialActors = [(1, "scout monster"), (3, "monster")]
  , fleaderMode =
      -- No point changing leader on level, since all move and they
      -- don't follow the leader.
      LeaderAI $ AutoLeader True True
  , fhasUI = False
  }

playerAntiMonster = playerMonster
  { fhasUI = True
  , fleaderMode = LeaderUI $ AutoLeader True True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TRoam  -- can't pick up, so no point exploring
  , fentryLevel = -1  -- fun from the start to avoid empty initial level
  , finitialActors = [(1 + d 2, "animal")]
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
-- Actors that can be summoned should have "horror" in their @ifreq@ set.
playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TPatrol  -- disoriented
  , fentryLevel = -3
  , finitialActors = []
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

victoryOutcomes :: [Outcome]
victoryOutcomes = [Conquer, Escape]

hiHero, hiDweller, hiRaid :: HiCondPoly

-- Heroes rejoice in loot.
hiHero = [ ( [(HiLoot, 1)]
           , [minBound..maxBound] )
         , ( [(HiConst, 1000), (HiLoss, -100)]
           , victoryOutcomes )
         ]

-- Spawners or skirmishers get no points from loot, but try to kill
-- all opponents fast or at least hold up for long.
hiDweller = [ ( [(HiConst, 1000)]  -- no loot
              , victoryOutcomes )
            , ( [(HiConst, 1000), (HiLoss, -10)]
              , victoryOutcomes )
            , ( [(HiBlitz, -100)]
              , victoryOutcomes )
            , ( [(HiSurvival, 100)]
              , [minBound..maxBound] \\ victoryOutcomes )
            ]

hiRaid = [ ( [(HiLoot, 1)]
           , [minBound..maxBound] )
         , ( [(HiConst, 100)]
           , victoryOutcomes )
         ]
