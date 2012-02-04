-- | Cave layouts for LambdaHack.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Misc

cdefs :: Content.CDefs CaveKind
cdefs = Content.CDefs
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
  , chiddenChance = 1%5
  , citemNum      = RollDice 5 2
  , cdefTile      = "fillerWall"
  , ccorTile      = "darkCorridor"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Underground city"
  , cfreq         = [("dng", 20), ("caveArena", 1)]
  , cgrid         = RollDiceXY (RollDice 2 2, RollDice 2 2)
  , cminPlaceSize = RollDiceXY (RollDice 3 2, RollDice 2 1)
  , cdarkChance   = (RollDice 1 80, RollDice 1 60)
  , cvoidChance   = 1%3
  , cnonVoidMin   = 2
  , citemNum      = RollDice 3 2  -- few rooms
  , cdefTile      = "floorArenaLit"
  , ccorTile      = "path"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = RollDiceXY (RollDice 2 2, RollDice 1 2)
  , cminPlaceSize = RollDiceXY (RollDice 4 3, RollDice 4 1)
  , cdarkChance   = (RollDice 1 80, RollDice 1 80)
  , cvoidChance   = 1%2
  , cnonVoidMin   = 0
  , citemNum      = RollDice 6 2  -- all floor strewn with treasure
  , cdefTile      = "floorRoomLit"
  , ccorTile      = "floorRoomLit"
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
  , citemNum      = RollDice 3 2  -- few rooms
  , cdefTile      = "noiseSet"
  , ccorTile      = "path"
  }
