-- | Cave layouts for LambdaHack.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
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
  , cgrid         = DiceXY (3 * d 2) (d 2 + 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 2) 4
  , cmaxPlaceSize = DiceXY (intToDice $ fst normalLevelBound)
                           (intToDice $ snd normalLevelBound)
  , cdarkChance   = d 54
  , cnightChance  = 100
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = 7 * d 2
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
  , cgrid         = DiceXY (2 * d 2) (2 * d 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 4
  , cdarkChance   = d 80 + dl 60
  , cnightChance  = 0
  , cmaxVoid      = 1%3
  , chidden       = 1000
  , citemNum      = 5 * d 2  -- few rooms
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "trailLit"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2 + 1) 1
  , cminPlaceSize = DiceXY 10 10
  , cmaxPlaceSize = DiceXY (intToDice $ fst normalLevelBound * 3 `div` 5)
                           (intToDice $ snd normalLevelBound * 3 `div` 5)
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , citemNum      = 8 * d 2  -- whole floor strewn with treasure
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = DiceXY (2 * d 2) (d 2 + 1)
  , cminPlaceSize = DiceXY (3 * d 2 + 2) 5
  , cdarkChance   = d 80 + dl 40
  , cnightChance  = d 40 + dl 40
  , cmaxVoid      = 0
  , chidden       = 1000
  , citemNum      = 4 * d 2  -- fewer rooms
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
combat = rogue
  { csymbol       = 'C'
  , cname         = "Combat arena"
  , cfreq         = [("caveCombat", 1)]
  , cgrid         = DiceXY (2 * d 2 + 3) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkChance   = 100
  , cnightChance  = d 67
  , chidden       = 1000
  , cauxConnects  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , citemNum      = 12 * d 2
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
