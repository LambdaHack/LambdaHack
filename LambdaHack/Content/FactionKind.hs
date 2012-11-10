-- | Game factions (heroes, enemies, NPCs, etc.) for LambdaHack.
module Content.FactionKind ( cdefs ) where

import Game.LambdaHack.CDefs
import Game.LambdaHack.Content.FactionKind

cdefs :: CDefs FactionKind
cdefs = CDefs
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = fvalidate
  , content =
      [hero, monster]
  }
hero,        monster :: FactionKind

hero = FactionKind
  { fsymbol     = '@'
  , fname       = "hero"
  , ffreq       = [("hero", 1), ("playable", 50)]
  , fAiSelected = "noAbility"  -- no AI, fully manual control
  , fAiIdle     = "meleeAdjacent"
  , fenemy      = ["monster"]
  , fally       = []
  }

monster = FactionKind
  { fsymbol     = 'm'
  , fname       = "monster"
  , ffreq       = [("monster", 1), ("playable", 50), ("spawn", 1)]
  , fAiSelected = "fullAbility"
  , fAiIdle     = "fullAbility"
  , fenemy      = ["hero"]
  , fally       = []
  }
