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
      [noAbility, onlyFollowTrack, meleeAdjacent, meleeAndRanged, fullAbility]
  }
noAbility,        onlyFollowTrack, meleeAdjacent, meleeAndRanged, fullAbility :: StrategyKind

noAbility = StrategyKind  -- not even projectiles will fly
  { ssymbol    = '@'
  , sname      = "noAbility"
  , sfreq      = [("noAbility", 1)]
  , sabilities = []
  }

onlyFollowTrack = StrategyKind  -- projectiles enabled
  { ssymbol    = '@'
  , sname      = "onlyFollowTrack"
  , sfreq      = [("onlyFollowTrack", 1)]
  , sabilities = [Track]
  }

meleeAdjacent = StrategyKind
  { ssymbol    = '@'
  , sname      = "meleeAdjacent"
  , sfreq      = [("meleeAdjacent", 1)]
  , sabilities = [Melee, Track]
  }

meleeAndRanged = StrategyKind  -- melee and reaction fire
  { ssymbol    = '@'
  , sname      = "meleeAndRanged"
  , sfreq      = [("meleeAndRanged", 1)]
  , sabilities = [Melee, Ranged, Track]
  }

fullAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "fullAbility"
  , sfreq      = [("fullAbility", 1)]
  , sabilities = [minBound..maxBound]
  }
