{-# LANGUAGE OverloadedStrings #-}
-- | Cave layouts for LambdaHack.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.ContentDef
import Game.LambdaHack.Misc
import Game.LambdaHack.Random as Random

cdefs :: ContentDef CaveKind
cdefs = ContentDef
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = cvalidate
  , content =
      [rogue, arena, empty, noise]
  }
rogue,        arena, empty, noise :: CaveKind

rogue = CaveKind
  { csymbol       = '$'
  , cname         = "A maze of twisty passages"
  , cfreq         = [("dng", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = RollDiceXY (RollDice 2 3, RollDice 2 2)
  , cminPlaceSize = RollDiceXY (RollDice 2 2, RollDice 2 1)
  , cdarkChance   = (RollDice 1 54, RollDice 0 0)
  , cauxConnects  = 1%3
  , cvoidChance   = 1%4
  , cnonVoidMin   = 4
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chiddenChance = 1%25
  , citemNum      = RollDice 7 2
  , cdefTile    = "fillerWall"
  , ccorridorTile   = "darkCorridor"
  , cfillerTile     = "fillerWall"
  , cdarkLegendTile = "darkLegend"
  , clitLegendTile  = "litLegend"
  , chiddenTile     = "hidden"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Underground city"
  , cfreq         = [("dng", 30), ("caveArena", 1)]
  , cgrid         = RollDiceXY (RollDice 2 2, RollDice 2 2)
  , cminPlaceSize = RollDiceXY (RollDice 3 2, RollDice 2 1)
  , cdarkChance   = (RollDice 1 80, RollDice 1 60)
  , cvoidChance   = 1%3
  , cnonVoidMin   = 2
  , citemNum      = RollDice 4 2  -- few rooms
  , cdefTile  = "floorArenaLit"
  , ccorridorTile = "path"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = RollDiceXY (RollDice 2 2, RollDice 1 2)
  , cminPlaceSize = RollDiceXY (RollDice 4 3, RollDice 4 1)
  , cdarkChance   = (RollDice 1 80, RollDice 1 80)
  , cauxConnects  = 1
  , cvoidChance   = 3%4
  , cnonVoidMin   = 1
  , cminStairDist = 50
  , citemNum      = RollDice 8 2  -- whole floor strewn with treasure
  , cdefTile  = "floorRoomLit"
  , ccorridorTile = "floorRoomLit"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = RollDiceXY (RollDice 2 2, RollDice 1 2)
  , cminPlaceSize = RollDiceXY (RollDice 4 2, RollDice 4 1)
  , cdarkChance   = (RollDice 1 80, RollDice 1 40)
  , cvoidChance   = 0
  , cnonVoidMin   = 0
  , citemNum      = RollDice 4 2  -- few rooms
  , cdefTile  = "noiseSet"
  , ccorridorTile = "path"
  }
