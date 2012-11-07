-- | AI strategies for LambdaHack.
module Content.StrategyKind ( cdefs ) where

import Game.LambdaHack.Ability
import Game.LambdaHack.CDefs
import Game.LambdaHack.Content.StrategyKind

cdefs :: CDefs StrategyKind
cdefs = CDefs
  { getSymbol = ssymbol
  , getName = sname
  , getFreq = sfreq
  , validate = svalidate
  , content =
      [noAbility, onlyMissile, meleeAndMissile, fullAbility]
  }
noAbility,        onlyMissile, meleeAndMissile, fullAbility :: StrategyKind

noAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "noAbility"
  , sfreq      = [("noAbility", 1)]
  , sabilities = []
  }

onlyMissile = StrategyKind
  { ssymbol    = '@'
  , sname      = "onlyMissile"
  , sfreq      = [("onlyMissile", 1)]
  , sabilities = [Track]
  }

meleeAndMissile = StrategyKind
  { ssymbol    = '@'
  , sname      = "meleeAndMissile"
  , sfreq      = [("meleeAndMissile", 1)]
  , sabilities = [Melee, Track]
  }

fullAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "fullAbility"
  , sfreq      = [("fullAbility", 1)]
  , sabilities = [minBound..maxBound]
  }
