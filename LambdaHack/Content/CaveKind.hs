module Content.CaveKind ( cdefs ) where

import Data.Ratio
import qualified Data.List as L

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Tile

cdefs :: Content.CDefs CaveKind
cdefs = Content.CDefs
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = cvalidate
  , content =
      [rogue, arena, empty, noise, largeNoise]
  }
rogue,        arena, empty, noise, largeNoise :: CaveKind

rogue = CaveKind
  { csymbol           = '$'
  , cname             = "caveRogue"
  , cfreq             = 80
  , cxsize            = fst normalLevelBound + 1
  , cysize            = snd normalLevelBound + 1
  , levelGrid         = do
                          x <- Random.randomR (3, 5)
                          y <- Random.randomR (2, 4)
                          return (x, y)
  , minRoomSize       = return (2, 2)
  , darkRoomChance    = \ d -> Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2)
  , border            = 2
  , extraConnects     = \ (x, y) -> (x * y) `div` 3
  , noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 3)
  , minStairsDistance = 30
  , doorChance        = Random.chance $ 2%3
  , doorOpenChance    = Random.chance $ 1%10
  , doorSecretChance  = Random.chance $ 1%4
  , csecretStrength   = (7, 2)
  , citemNum          = (5, 2)
  , clayout           = CaveRogue
  , defTile           = \ t -> tsymbol t == '#' && L.null (tfeature t)
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [] [F.Exit, F.Lit, F.Special] t
  }
arena = rogue
  { csymbol           = 'A'
  , cname             = "caveArena"
  , cfreq             = 20
  , clayout           = CaveRogue
  , defTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Exit] [F.Special] t
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Special] [F.Exit] t
  }
empty = rogue
  { csymbol           = '.'
  , cname             = "caveEmpty"
  , cfreq             = 20
  , clayout           = CaveEmpty
  , defTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit] [F.Exit, F.Special] t
  }
noise = rogue
  { csymbol           = '!'
  , cname             = "caveNoise"
  , cfreq             = 20
  , clayout           = CaveRogue
  , defTile           = \ t -> tsymbol t == '#' && L.null (tfeature t)
                               || (tsymbol t == '.'
                                  && kindHas [F.Lit, F.Exit] [F.Special] t)
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Special] [F.Exit] t
  }
largeNoise = noise
  { csymbol           = 'L'
  , cname             = "caveLargeNoise"
  , cfreq             = 0  -- experimental
  , cxsize            = 231
  , cysize            = 77
  }
