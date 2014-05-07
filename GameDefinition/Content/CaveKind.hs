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
      [rogue, arena, empty, noise, battle, skirmish, ambush]
  }
rogue,        arena, empty, noise, battle, skirmish, ambush :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "A maze of twisty passages"
  , cfreq         = [("dng", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = DiceXY (3 * d 2) (d 2 + 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 2) 4
  , cmaxPlaceSize = DiceXY (intToDice $ fst normalLevelBound)
                           (intToDice $ snd normalLevelBound)
  , cdarkChance   = d 54 + dl 20
  , cnightChance  = 100
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = 7 * d 2
  , citemFreq     = [(70, "useful"), (30, "treasure")]
  , cplaceFreq    = [(100, "rogue")]
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
  { csymbol       = 'E'
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
  { csymbol       = 'N'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = DiceXY (2 * d 2) (d 2 + 1)
  , cminPlaceSize = DiceXY (3 * d 2 + 2) 5
  , cdarkChance   = d 80 + dl 40
  , cnightChance  = d 60
  , cmaxVoid      = 0
  , chidden       = 1000
  , citemNum      = 4 * d 2  -- fewer rooms
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
battle = rogue  -- TODO: actors can get stuck forever among rocks
  { csymbol       = 'B'
  , cname         = "Battle field"
  , cfreq         = [("caveBattle", 1)]
  , cgrid         = DiceXY (2 * d 2 + 3) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkChance   = 0
  , cnightChance  = 100
  , chidden       = 1000
  , cauxConnects  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , citemNum      = 12 * d 2
  , citemFreq     = [(100, "useful")]
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "floorArenaLit"
  }
skirmish = battle
  { csymbol       = 'S'
  , cname         = "Skirmish arena"
  , cfreq         = [("caveSkirmish", 1)]
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdefTile      = "skirmishSet"
  }
ambush = battle
  { csymbol       = 'M'
  , cname         = "Ambush scene"
  , cfreq         = [("caveAmbush", 1)]
  , citemNum      = 24 * d 2
  , cplaceFreq    = [(70, "ambush"), (30, "rogue")]
  , cdefTile      = "ambushSet"
  }
