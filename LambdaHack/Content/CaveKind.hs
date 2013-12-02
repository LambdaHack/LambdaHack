-- | Cave layouts for LambdaHack.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random as Random
import Game.LambdaHack.Content.CaveKind

cdefs :: ContentDef CaveKind
cdefs = ContentDef
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = cvalidate
  , content =
      [rogue, arena, empty, noise, combat]
  }
rogue,        arena, empty, noise, combat :: CaveKind

rogue = CaveKind
  { csymbol       = '$'
  , cname         = "A maze of twisty passages"
  , cfreq         = [("dng", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = rollDiceXY [(3, 2)] [(1, 2), (2, 1)]
  , cminPlaceSize = rollDiceXY [(2, 2), (2, 1)] [(4, 1)]
  , cmaxPlaceSize = rollDiceXY [(fst normalLevelBound, 1)]
                               [(snd normalLevelBound, 1)]
  , cdarkChance   = rollDeep (1, 54) (0, 0)
  , cauxConnects  = 1%3
  , cvoidChance   = 1%5
  , cnonVoidMin   = 5
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = rollDice 7 2
  , cdefTile        = "fillerWall"
  , ccorridorTile   = "darkCorridor"
  , cfillerTile     = "fillerWall"
  , cdarkLegendTile = "darkLegend"
  , clitLegendTile  = "litLegend"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Underground city"
  , cfreq         = [("dng", 30), ("caveArena", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(2, 2)]
  , cminPlaceSize = rollDiceXY [(2, 2), (3, 1)] [(4, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 60)
  , cvoidChance   = 1%3
  , cnonVoidMin   = 2
  , chidden       = 9
  , citemNum      = rollDice 5 2  -- few rooms
  , cdefTile      = "arenaSet"
  , ccorridorTile = "path"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = rollDiceXY [(1, 2)] [(1, 1)]
  , cminPlaceSize = rollDiceXY [(10, 1)] [(10, 1)]
  , cmaxPlaceSize = rollDiceXY [(fst normalLevelBound * 3 `div` 5, 1)]
                               [(snd normalLevelBound * 3 `div` 5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 80)
  , cauxConnects  = 1
  , cvoidChance   = 0
  , cnonVoidMin   = 0
  , cminStairDist = 50
  , chidden       = 100
  , citemNum      = rollDice 8 2  -- whole floor strewn with treasure
  , cdefTile      = "emptySet"
  , ccorridorTile = "emptySet"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(1, 2), (1, 1)]
  , cminPlaceSize = rollDiceXY [(3, 2), (2, 1)] [(5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 40)
  , cvoidChance   = 0
  , cnonVoidMin   = 0
  , chidden       = 6
  , citemNum      = rollDice 4 2  -- fewer rooms
  , cdefTile      = "noiseSet"
  , ccorridorTile = "path"
  }
combat = rogue
  { csymbol       = 'C'
  , cname         = "Combat arena"
  , cfreq         = [("caveCombat", 1)]
  , cgrid         = rollDiceXY [(2, 2), (3, 1)] [(1, 2), (2, 1)]
  , cminPlaceSize = rollDiceXY [(3, 1)] [(3, 1)]
  , cmaxPlaceSize = rollDiceXY [(5, 1)] [(5, 1)]
  , cdarkChance   = intToDeep 100
  , chidden       = 2
  , cauxConnects  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , citemNum      = rollDice 12 2
  , cdefTile      = "combatSet"
  , ccorridorTile = "arenaSet"
  }
