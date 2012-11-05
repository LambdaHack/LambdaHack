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
      [noAbility, onlyMelee, fullAbility]
  }
noAbility,        onlyMelee, fullAbility :: StrategyKind

noAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "noAbility"
  , sfreq      = [("noAbility", 1)]
  , sabilities = []
  }

onlyMelee = StrategyKind
  { ssymbol    = '@'
  , sname      = "onlyMelee"
  , sfreq      = [("onlyMelee", 1)]
  , sabilities = [Melee]
  }

fullAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "fullAbility"
  , sfreq      = [("fullAbility", 1)]
  , sabilities = [minBound..maxBound]
  }
