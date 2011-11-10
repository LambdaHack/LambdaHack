module Content.CaveKind (CaveKind(..)) where

import Data.Ratio

import qualified Content.Content
import Geometry
import Random

instance Content.Content.Content CaveKind where
  getFreq = undefined  --afreq
  content =
    [defaultCaveKind, largeCaveKind]

defaultCaveKind,      largeCaveKind :: CaveKind

data CaveKind = CaveKind
  { cxsize    :: X
  , cysize    :: Y
  , levelGrid         :: Rnd (X, Y)
  , minRoomSize       :: Rnd (X ,Y)
  , darkRoomChance    :: Int -> Rnd Bool
  , border            :: Int         -- must be at least 2!
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
  }

defaultCaveKind :: CaveKind
defaultCaveKind =
  CaveKind {
    cxsize = fst normalLevelBound + 1,
    cysize = snd normalLevelBound + 1,
    levelGrid         = do
                          x <- Random.randomR (3, 5)
                          y <- Random.randomR (2, 4)
                          return (x, y),
    minRoomSize       = return (2, 2),
    darkRoomChance    = \ d -> Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2),
    border            = 2,
    extraConnects     = \ (x, y) -> (x * y) `div` 3,
    noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 3),
    minStairsDistance = 30,
    doorChance        = Random.chance $ 2%3,
    doorOpenChance    = Random.chance $ 1%10,
    doorSecretChance  = Random.chance $ 1%4,
    doorSecretMax     = 15,
    nrItems           = Random.randomR (5, 10)
  }

largeCaveKind :: CaveKind
largeCaveKind =
  defaultCaveKind {
    cxsize = 231,
    cysize = 77,
    levelGrid         = return (10, 7),
    extraConnects     = const 10
  }
