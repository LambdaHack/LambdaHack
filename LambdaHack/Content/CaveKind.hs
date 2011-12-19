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
  , levelGrid         = ((2, 4), (2, 2))
  , minRoomSize       = ((2, 2), (2, 1))
  , darkRoomChance    = \ d -> Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2)
  , extraConnects     = \ (x, y) -> (x * y) `div` 3
  , noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 3)
  , minStairsDistance = 30
  , doorChance        = Random.chance $ 2%3
  , doorOpenChance    = Random.chance $ 1%10
  , doorSecretChance  = Random.chance $ 1%4
  , csecretStrength   = (7, 2)
  , citemNum          = (5, 2)
  , defTile           = \ t -> tsymbol t == '#' && L.null (tfeature t)
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [] [F.Lit, F.Special, F.Boring] t
  }
arena = rogue
  { csymbol           = 'A'
  , cname             = "caveArena"
  , cfreq             = 20
  , levelGrid         = ((2, 3), (2, 2))
  , minRoomSize       = ((2, 3), (2, 1))
  , noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 2)
  , defTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit] [F.Special, F.Boring] t
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Special] [F.Boring] t
  }
empty = rogue
  { csymbol           = '.'
  , cname             = "caveEmpty"
  , cfreq             = 20
  , levelGrid         = ((1, 3), (1, 2))
  , minRoomSize       = ((2, 4), (2, 3))
  , noRooms           = \ (x, y) -> Random.randomR (max 0 (x * y - 3),
                                                    max 0 (x * y - 1))
  , defTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Boring] [F.Special] t
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Boring] [F.Special] t
  }
noise = rogue
  { csymbol           = '!'
  , cname             = "caveNoise"
  , cfreq             = 20
  , levelGrid         = ((2, 2), (1, 3))
  , minRoomSize       = ((2, 2), (2, 2))
  , darkRoomChance    = \ _ -> return True
  , noRooms           = \ _ -> return 0
  , defTile           = \ t -> tsymbol t == '#' && L.null (tfeature t)
                               || (tsymbol t == '.'
                                  && kindHas [F.Lit] [F.Special, F.Boring] t)
  , corTile           = \ t -> tsymbol t == '.'
                               && kindHas [F.Lit, F.Special] [F.Boring] t
  }
largeNoise = noise
  { csymbol           = 'L'
  , cname             = "caveLargeNoise"
  , cfreq             = 0  -- experimental
  , cxsize            = 231
  , cysize            = 77
  }
