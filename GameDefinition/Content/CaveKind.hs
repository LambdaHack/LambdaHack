-- | Cave layouts.
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
  , validateSingle = validateSingleCaveKind
  , validateAll = validateAllCaveKind
  , content =
      [rogue, arena, empty, noise, shallow1rogue, battle, skirmish, ambush, safari1, safari2, safari3, boardgame]
  }
rogue,        arena, empty, noise, shallow1rogue, battle, skirmish, ambush, safari1, safari2, safari3, boardgame :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "A maze of twisty passages"
  , cfreq         = [("campaign random", 100), ("caveRogue", 1)]
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
  , cactorCoeff   = 15  -- the maze requires time to explore
  , cactorFreq    = [("monster", 40), ("animal", 60)]
  , citemNum      = 10 * d 2
  , citemFreq     = [("useful", 50), ("treasure", 50)]
  , cplaceFreq    = [("rogue", 100)]
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
  , cname         = "Underground library"
  , cfreq         = [("campaign random", 50), ("caveArena", 1)]
  , cgrid         = DiceXY (2 * d 2) (2 * d 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 4
  , cdarkChance   = d 60 + dl 60
  , cnightChance  = d 60 + dl 60 -- trails provide enough light for fun stealth
  , cnightChance  = 0
  , cmaxVoid      = 1%4
  , chidden       = 1000
  , cactorCoeff   = 10
  , cactorFreq    = [("monster", 70), ("animal", 30)]
  , citemNum      = 9 * d 2  -- few rooms
  , citemFreq     = [("useful", 20), ("treasure", 30), ("any scroll", 50)]
  , cpassable     = True
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Tall cavern"
  , cfreq         = [("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2 + 1) 1
  , cminPlaceSize = DiceXY 10 10
  , cmaxPlaceSize = DiceXY 24 12
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , cactorCoeff   = 8
  , cactorFreq    = [("monster", 2), ("animal", 8), ("immobileVents", 90)]
      -- The geysers on lvl 3 act like HP resets. They are needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. The items are not reset, even if the are bad, which provides
      -- enough of a continuity. Gyesers on lvl 3 are not OP and can't be
      -- abused, because they spawn less and less often, they don't heal over
      -- max HP and they expire naturally.
  , citemNum      = 7 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Leaky, burrowed sediment"
  , cfreq         = [("campaign random", 20), ("caveNoise", 1)]
  , cgrid         = DiceXY (2 + d 2) 3
  , cminPlaceSize = DiceXY 12 5
  , cmaxPlaceSize = DiceXY 24 12
  , cnightChance  = d 80 + dl 80
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , chidden       = 1000
  , cactorCoeff   = 20  -- the maze requires time to explore
  , cactorFreq    = [("monster", 80), ("animal", 20)]
  , citemNum      = 12 * d 2  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [("noise", 100)]
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
shallow1rogue = rogue
  { csymbol       = 'D'
  , cname         = "Entrance to the dungeon"
  , cfreq         = [("shallow random 1", 100)]
  , cdarkChance   = 0
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq rogue
  , citemNum      = 15 * d 2  -- lure them in with loot
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  }
battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'B'
  , cname         = "Old battle ground"
  , cfreq         = [("caveBattle", 1)]
  , cgrid         = DiceXY (2 * d 2 + 1) 3
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkChance   = 0
  , cnightChance  = 100
  , cmaxVoid      = 0
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
skirmish = rogue  -- many random solid tiles, to break LOS, since it's a day
  { csymbol       = 'S'
  , cname         = "Sunny woodland"
  , cfreq         = [("caveSkirmish", 1)]
  , cgrid         = DiceXY (2 * d 2 + 2) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("skirmish", 60), ("rogue", 40)]
  , cpassable     = True
  , cdefTile      = "skirmishSet"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
ambush = rogue  -- lots of lights, to give a chance to snipe
  { csymbol       = 'M'
  , cname         = "Public garden at night"
  , cfreq         = [("caveAmbush", 1)]
  , cgrid         = DiceXY (2 * d 2 + 3) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkChance   = 0
  , cnightChance  = 100
  , cauxConnects  = 1
  , cdoorChance   = 1%10
  , copenChance   = 9%10
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 22 * d 2
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("ambush", 100)]
  , cpassable     = True
  , cdefTile      = "ambushSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
safari1 = ambush {cfreq = [("caveSafari1", 1)]}
safari2 = battle {cfreq = [("caveSafari2", 1)]}
safari3 = skirmish {cfreq = [("caveSafari3", 1)]}
boardgame = CaveKind
  { csymbol       = 'B'
  , cname         = "A boardgame"
  , cfreq         = [("caveBoardgame", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = DiceXY 1 1
  , cminPlaceSize = DiceXY 10 10
  , cmaxPlaceSize = DiceXY 10 10
  , cdarkChance   = 0
  , cnightChance  = 0
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , cminStairDist = 0
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 0
  , cactorFreq    = []
  , citemNum      = 0
  , citemFreq     = []
  , cplaceFreq    = [("boardgame", 1)]
  , cpassable     = False
  , cdefTile        = "fillerWall"
  , cdarkCorTile    = "floorCorridorDark"
  , clitCorTile     = "floorCorridorLit"
  , cfillerTile     = "fillerWall"
  , couterFenceTile = "basic outer fence"
  , clegendDarkTile = "legendDark"
  , clegendLitTile  = "legendLit"
  }
