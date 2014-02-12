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
  , validate = validateCaveKind
  , content =
      [rogue, arena, empty, noise, combat, battle]
  }
rogue,        arena, empty, noise, combat, battle :: CaveKind

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
  , cnightChance  = intToDeep 100
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = rollDice 7 2
  , citemFreq     = [(70, "useful"), (30, "treasure")]
  , cdefTile        = "fillerWall"
  , cdarkCorTile    = "floorCorridorDark"
  , clitCorTile     = "floorCorridorLit"
  , cfillerTile     = "fillerWall"
  , couterFenceTile = "basic outer fence"
  , clegendDarkTile = "legendDark"
  , clegendLitTile  = "legendLit"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Underground city"
  , cfreq         = [("dng", 30), ("caveArena", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(2, 2)]
  , cminPlaceSize = rollDiceXY [(2, 2), (3, 1)] [(4, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 60)
  , cnightChance  = intToDeep 0
  , cmaxVoid      = 1%3
  , chidden       = 1000
  , citemNum      = rollDice 5 2  -- few rooms
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "trailLit"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = rollDiceXY [(1, 2), (1, 1)] [(1, 1)]
  , cminPlaceSize = rollDiceXY [(10, 1)] [(10, 1)]
  , cmaxPlaceSize = rollDiceXY [(fst normalLevelBound * 3 `div` 5, 1)]
                               [(snd normalLevelBound * 3 `div` 5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 80)
  , cnightChance  = intToDeep 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , citemNum      = rollDice 8 2  -- whole floor strewn with treasure
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(1, 2), (1, 1)]
  , cminPlaceSize = rollDiceXY [(3, 2), (2, 1)] [(5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 40)
  , cnightChance  = rollDeep (1, 40) (1, 40)
  , cmaxVoid      = 0
  , chidden       = 1000
  , citemNum      = rollDice 4 2  -- fewer rooms
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
combat = rogue
  { csymbol       = 'C'
  , cname         = "Combat arena"
  , cfreq         = [("caveCombat", 1)]
  , cgrid         = rollDiceXY [(2, 2), (3, 1)] [(1, 2), (2, 1)]
  , cminPlaceSize = rollDiceXY [(3, 1)] [(3, 1)]
  , cmaxPlaceSize = rollDiceXY [(5, 1)] [(5, 1)]
  , cdarkChance   = intToDeep 100
  , cnightChance  = rollDeep (1, 67) (0, 0)
  , chidden       = 1000
  , cauxConnects  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , citemNum      = rollDice 12 2
  , citemFreq     = [(100, "useful")]
  , cdefTile      = "combatSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "floorArenaLit"
  }
battle = combat  -- TODO: actors can get stuck forever among trees
  { csymbol       = 'B'
  , cname         = "Battle arena"
  , cfreq         = [("caveBattle", 1)]
  , cdefTile      = "battleSet"
  }
