module Content.CaveKind (CaveKind(..)                      , defaultCaveKind, normalLevelBound) where

import Data.Ratio

import qualified Content.Content
import Geometry
import Random

instance Content.Content.Content CaveKind where
  getFreq = undefined  --afreq
  content = []
--    [hero, eye, fastEye, nose]

--hero,      eye, fastEye, nose :: CaveKind

data CaveKind = CaveKind
  { levelGrid         :: Rnd (X, Y)
  , minRoomSize       :: Rnd (X ,Y)
  , darkRoomChance    :: Rnd Bool
  , border            :: Int         -- must be at least 2!
  , levelBound        :: (X, Y)      -- lower right point; TODO: change to size or rename to 'bound'
  , extraConnects     :: (X, Y) -> Int
      -- relative to grid (in fact a range, because of duplicate connects)
  , noRooms           :: (X, Y) -> Rnd Int
      -- range, relative to grid
  , minStairsDistance :: Int         -- must not be too large
  , doorChance        :: Rnd Bool
  , doorOpenChance    :: Rnd Bool
  , doorSecretChance  :: Rnd Bool
  , doorSecretMax     :: Int
  , nrItems           :: Rnd Int     -- range
  , depth             :: Int         -- general indicator of difficulty
  }

normalLevelBound :: (X, Y)
normalLevelBound = (79, 22)

defaultCaveKind :: Int -> CaveKind
defaultCaveKind d =
  CaveKind {
    levelGrid         = do
                          x <- Random.randomR (3, 5)
                          y <- Random.randomR (2, 4)
                          return (x, y),
    minRoomSize       = return (2, 2),
    darkRoomChance    = Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2),
    border            = 2,
    levelBound        = normalLevelBound,
    extraConnects     = \ (x, y) -> (x * y) `div` 3,
    noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 3),
    minStairsDistance = 30,
    doorChance        = Random.chance $ 2%3,
    doorOpenChance    = Random.chance $ 1%10,
    doorSecretChance  = Random.chance $ 1%4,
    doorSecretMax     = 15,
    nrItems           = Random.randomR (5, 10),
    depth             = d
  }

_largeCaveKind :: Int -> CaveKind
_largeCaveKind d =
  (defaultCaveKind d) {
    levelGrid         = return (10, 7),
    levelBound        = (231, 77),
    extraConnects     = const 10
  }
