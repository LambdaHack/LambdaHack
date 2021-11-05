-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian
  , playerMonster, playerAntiMonster, playerAnimal
  , playerHorror, playerMonsterTourist, playerHunamConvict
  , playerAnimalMagnificent, playerAnimalExquisite
  , hiHeroShort, hiHeroMedium, hiHeroLong, hiDweller
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Content.ItemKindActor
import           Content.ItemKindOrgan
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Definition.Ability

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAntiMonster, playerAnimal, playerHorror, playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player

playerHero = Player
  { fname = "Explorer"
  , fgroups = [HERO]
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHeroLong
  , fhasGender = True
  , finitDoctrine = TExplore
  , fleaderMode = Just $ AutoLeader False False
  , fhasUI = True
  , funderAI = False
  }

playerAntiHero = playerHero
  { fleaderMode = Just $ AutoLeader True False
  , fhasUI = False
  , funderAI = True
  }

playerCivilian = Player
  { fname = "Civilian"
  , fgroups = [HERO, CIVILIAN]
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = hiHeroMedium
  , fhasGender = True
  , finitDoctrine = TPatrol
  , fleaderMode = Nothing  -- unorganized
  , fhasUI = False
  , funderAI = True
  }

playerMonster = Player
  { fname = "Monster Hive"
  , fgroups = [MONSTER, MOBILE_MONSTER]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TExplore
  , fleaderMode =
      -- No point changing leader on level, since all move and they
      -- don't follow the leader.
      Just $ AutoLeader True True
  , fhasUI = False
  , funderAI = True
  }

playerAntiMonster = playerMonster
  { fleaderMode = Just $ AutoLeader True True
  , fhasUI = True
  , funderAI = False
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroups = [ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, SCAVENGER]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TRoam  -- can't pick up, so no point exploring
  , fleaderMode = Nothing
  , fhasUI = False
  , funderAI = True
  }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
playerHorror = Player
  { fname = "Horror Den"
  , fgroups = [IK.HORROR]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasGender = False
  , finitDoctrine = TPatrol  -- disoriented
  , fleaderMode = Nothing
  , fhasUI = False
  , funderAI = True
  }

playerMonsterTourist =
  playerAntiMonster
    { fname = "Monster Tourist Office"
    , fcanEscape = True
    , fneverEmpty = True  -- no spawning
    , fhiCondPoly = hiHeroMedium
    , finitDoctrine = TFollow  -- follow-the-guide, as tourists do
    , fleaderMode = Just $ AutoLeader False False
    , funderAI = False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict"
                 , fleaderMode = Just $ AutoLeader True False
                 , funderAI = True }

playerAnimalMagnificent =
  playerAnimal { fname = "Animal Magnificent Specimen Variety"
               , fneverEmpty = True }

playerAnimalExquisite =
  playerAnimal { fname = "Animal Exquisite Herds and Packs Galore"
               , fneverEmpty = True }

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
