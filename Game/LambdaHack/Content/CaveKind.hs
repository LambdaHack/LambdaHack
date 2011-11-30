module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), CaveLayout(..)
  ) where

import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.TileKind

data CaveKind = CaveKind
  { csymbol           :: Char
  , cname             :: String
  , cfreq             :: Int
  , cxsize            :: X
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
  , defTile           :: TileKind -> Bool
  }

-- TODO: express those using many fine-graned parameters instead
data CaveLayout = CaveRogue | CaveEmpty | CaveNoise deriving Eq
