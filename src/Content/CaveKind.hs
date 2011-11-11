module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), CaveLayout(..)
  ) where

import Data.Ratio

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Random as Random

data CaveKind = CaveKind
  { cxsize            :: X
  , cysize            :: Y
  , levelGrid         :: Random.Rnd (X, Y)
  , minRoomSize       :: Random.Rnd (X ,Y)
  , darkRoomChance    :: Int -> Random.Rnd Bool  -- TODO: use RollQuad instead, etc.
  , border            :: Int         -- must be at least 2!
  , extraConnects     :: (X, Y) -> Int
      -- relative to grid (in fact a range, because of duplicate connects)
  , noRooms           :: (X, Y) -> Random.Rnd Int
      -- range, relative to grid
  , minStairsDistance :: Int
  , doorChance        :: Random.Rnd Bool
  , doorOpenChance    :: Random.Rnd Bool
  , doorSecretChance  :: Random.Rnd Bool
  , csecretStrength   :: Random.RollDice
  , citemNum          :: Random.RollDice
  , clayout           :: CaveLayout
  , cfreq             :: Int
  }

instance Content.Content CaveKind where
  getFreq = cfreq
  content =
    [rogue, empty, noise, largeNoise]

rogue,      empty, noise, largeNoise:: CaveKind

-- TODO: express those using many fine-graned parameters instead
data CaveLayout = CaveRogue | CaveEmpty | CaveNoise deriving Eq

rogue = CaveKind
  { cxsize            = fst normalLevelBound + 1
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
  , cfreq             = 80
  }
empty = rogue
  { clayout           = CaveEmpty
  , cfreq             = 20
  }
noise = rogue
  { clayout           = CaveNoise
  , cfreq             = 0  -- stairs may be blocked, so only for the last level
  }
largeNoise = noise
  { cxsize            = 231
  , cysize            = 77
  , cfreq             = 0  -- experimental
  }
