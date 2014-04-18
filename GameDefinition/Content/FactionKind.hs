-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for LambdaHack.
module Content.FactionKind ( cdefs ) where

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
      [hero, monster, animal, horror]
  }
hero,        monster, animal, horror :: FactionKind

hero = FactionKind
  { fsymbol        = '@'
  , fname          = "hero"
  , ffreq          = [("hero", 1)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = meleeAdjacent
  }

monster = FactionKind
  { fsymbol        = 'm'
  , fname          = "monster"
  , ffreq          = [("monster", 1), ("summon", 50)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = allAbilities
  }

animal = FactionKind
  { fsymbol        = 'd'
  , fname          = "animal"
  , ffreq          = [("animal", 1), ("summon", 50)]
  , fAbilityLeader = animalAbility
  , fAbilityOther  = animalAbility
  }

horror = FactionKind
  { fsymbol        = 'h'
  , fname          = "horror"
  , ffreq          = [("horror", 1), ("summon", 50)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = allAbilities
  }


_noAbility, meleeAdjacent, _meleeAndRanged, animalAbility, allAbilities :: [Ability]

_noAbility = []

meleeAdjacent = [AbWait, AbMelee]

_meleeAndRanged = [AbWait, AbMelee, AbProject]  -- melee and reaction fire

animalAbility = [AbMove, AbMelee, AbWait, AbTrigger]

allAbilities = [minBound..maxBound]
