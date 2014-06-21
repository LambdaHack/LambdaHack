-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for LambdaHack.
module Content.FactionKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.FactionKind

cdefs :: ContentDef FactionKind
cdefs = ContentDef
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = validateFactionKind
  , content =
      [hero, civilian, monster, animal, horror]
  }
hero,        civilian, monster, animal, horror :: FactionKind

hero = FactionKind
  { fsymbol        = '1'
  , fname          = "hero"
  , ffreq          = [("hero", 1)]
  , fSkillsLeader = allAbilities
  , fSkillsOther  = meleeAdjacent
  }

civilian = FactionKind
  { fsymbol        = '@'
  , fname          = "civilian"
  , ffreq          = [("civilian", 1)]
  , fSkillsLeader = allAbilities
  , fSkillsOther  = allAbilities  -- not coordinated by any leadership
  }

monster = FactionKind
  { fsymbol        = 'm'
  , fname          = "monster"
  , ffreq          = [("monster", 1), ("summon", 60)]
  , fSkillsLeader = allAbilities
  , fSkillsOther  = allAbilities
  }

animal = FactionKind
  { fsymbol        = 'd'
  , fname          = "animal"
  , ffreq          = [("animal", 1), ("summon", 40)]
  , fSkillsLeader = animalAbility
  , fSkillsOther  = animalAbility
  }

horror = FactionKind
  { fsymbol        = 'h'
  , fname          = "horror"
  , ffreq          = [("horror", 1), ("summon", 100)]
  , fSkillsLeader = allAbilities
  , fSkillsOther  = allAbilities
  }


_noAbility, meleeAdjacent, _meleeAndRanged, animalAbility, allAbilities :: Skills

_noAbility = EM.empty

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1..]

animalAbility =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1..]

allAbilities = unitSkills
