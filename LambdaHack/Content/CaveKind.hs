module Content.CaveKind ( cdefs ) where

import Data.Ratio

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Geometry
import Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Tile as Tile

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
  , cname         = "caveRogue"
  , cfreq         = 100
  , cdesc         = "A maze of twisty passages"
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = (RollDice 3 2, RollDice 2 2)
  , cminRoomSize  = (RollDice 2 2, RollDice 2 1)
  , cdarkChance   = \ d -> Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2)
  , cauxConnects  = \ (x, y) -> (x * y) `div` 3
  , cvoidRooms    = \ (x, y) -> Random.randomR (0, (x * y) `div` 3)
  , cminStairDist = 30
  , cdoorChance   = Random.chance $ 2%3
  , copenChance   = Random.chance $ 1%10
  , csecretChance = Random.chance $ 1%4
  , citemNum      = RollDice 5 2
  , cdefTile      = Tile.wallP
  , ccorTile      = \ t -> tsymbol t == '#' && Tile.floorCorridorDarkP t
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "caveArena"
  , cfreq         = 20
  , cdesc         = "Underground city"
  , cgrid         = (RollDice 2 2, RollDice 2 2)
  , cminRoomSize  = (RollDice 3 2, RollDice 2 1)
  , cvoidRooms    = \ (x, y) -> Random.randomR (0, (x * y) `div` 2)
  , cdefTile      = \ t -> tsymbol t == '.' && Tile.floorCorridorLitP t
  , ccorTile      = Tile.floorSpecialP
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "caveEmpty"
  , cfreq         = 20
  , cdesc         = "Tall cavern"
  , cgrid         = (RollDice 2 2, RollDice 1 2)
  , cminRoomSize  = (RollDice 4 3, RollDice 4 1)
  , cvoidRooms    = \ (x, y) -> Random.randomR (max 0 (x * y - 3),
                                               max 0 (x * y - 1))
  , cdefTile      = Tile.floorRoomLitP
  , ccorTile      = Tile.floorRoomLitP
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "caveNoise"
  , cfreq         = 20
  , cdesc         = "Glittering cave"
  , cgrid         = (RollDice 2 2, RollDice 1 2)
  , cminRoomSize  = (RollDice 4 2, RollDice 4 1)
  , cdarkChance   = \ _ -> return True
  , cvoidRooms    = \ _ -> return 0
  , cdefTile      = \ t -> tsymbol t == 'O' && tfeature t == [F.Special] ||
                          tsymbol t == '.' && Tile.floorCorridorLitP t
  , ccorTile      = Tile.floorSpecialP
  }
