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
  , cmaxPlaceSize = DiceXY 15 10
  , cdarkChance   = d 54 + dl 20
  , cnightChance  = 51
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = 8 * d 2
  , citemFreq     = [(70, "useful"), (30, "treasure")]
  , cplaceFreq    = [(100, "rogue")]
  , cpassable     = False
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
  , citemNum      = 6 * d 2  -- few rooms
  , cpassable     = True
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
  , cmaxPlaceSize = DiceXY 24 12
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , citemNum      = 4 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 10), ("caveNoise", 1)]
  , cgrid         = DiceXY 3 3
  , cminPlaceSize = DiceXY 8 4
  , cmaxPlaceSize = DiceXY 24 12
  , cnightChance  = d 100
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , chidden       = 1000
  , citemNum      = 10 * d 2  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [(50, "noise"), (50, "rogue")]
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
battle = rogue
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
  , cdoorChance   = 1%10
  , copenChance   = 9%10
  , citemNum      = 12 * d 2
  , citemFreq     = [(100, "useful")]
  , cpassable     = True
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
  , cdoorChance   = 1
  , copenChance   = 0
  , cplaceFreq    = [(50, "skirmish"), (50, "rogue")]
  , cdefTile      = "skirmishSet"
  }
ambush = battle
  { csymbol       = 'M'
  , cname         = "Ambush scene"
  , cfreq         = [("caveAmbush", 1)]
  , citemNum      = 24 * d 2
  , cplaceFreq    = [(100, "ambush")]
  , cdefTile      = "ambushSet"
  }
