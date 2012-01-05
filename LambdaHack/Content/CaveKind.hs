module Content.CaveKind ( cdefs ) where

import Data.Ratio

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Geometry
import Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.CaveKind

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
  , cgrid         = (RollDice 3 2, RollDice 2 2)
  , cminRoomSize  = (RollDice 2 2, RollDice 2 1)
  , cdarkChance   = (RollDice 1 53, RollDice 25 1)
  , cauxConnects  = 1%3
  , croomChance   = 2%3
  , cminStairDist = 30
  , cdoorChance   = 3%4
  , copenChance   = 1%10
  , csecretChance = 1%4
  , citemNum      = RollDice 5 2
  , cdefTile      = "fillerWall"
  , ccorTile      = "darkCorridor"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Underground city"
  , cfreq         = [("dng", 20), ("caveArena", 1)]
  , cgrid         = (RollDice 2 2, RollDice 2 2)
  , cminRoomSize  = (RollDice 3 2, RollDice 2 1)
  , croomChance   = 1%2
  , cdefTile      = "floorArenaLit"
  , ccorTile      = "path"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Tall cavern"
  , cfreq         = [("dng", 20), ("caveEmpty", 1)]
  , cgrid         = (RollDice 2 2, RollDice 1 2)
  , cminRoomSize  = (RollDice 4 3, RollDice 4 1)
  , croomChance   = 1%2
  , cdefTile      = "floorRoomLit"
  , ccorTile      = "floorRoomLit"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Glittering cave"
  , cfreq         = [("dng", 20), ("caveNoise", 1)]
  , cgrid         = (RollDice 2 2, RollDice 1 2)
  , cminRoomSize  = (RollDice 4 2, RollDice 4 1)
  , cdarkChance   = intToQuad 100
  , croomChance   = 1
  , cdefTile      = "noiseSet"
  , ccorTile      = "path"
  }
