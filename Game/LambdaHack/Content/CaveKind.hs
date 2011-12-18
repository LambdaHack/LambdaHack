module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.TileKind

data CaveKind = CaveKind
  { csymbol           :: Char
  , cname             :: String
  , cfreq             :: Int
  , cxsize            :: X
  , cysize            :: Y
  , levelGrid         :: (Random.RollDice, Random.RollDice)
  , minRoomSize       :: (Random.RollDice, Random.RollDice)
  , darkRoomChance    :: Int -> Random.Rnd Bool  -- TODO: use RollQuad instead, etc.
  , border            :: Int         -- must be at least 2!
  , extraConnects     :: (X, Y) -> Int
      -- ^ relative to grid (in fact a range, because of duplicate connects)
  , noRooms           :: (X, Y) -> Random.Rnd Int
      -- ^ range, relative to grid
  , minStairsDistance :: Int
  , doorChance        :: Random.Rnd Bool
  , doorOpenChance    :: Random.Rnd Bool
  , doorSecretChance  :: Random.Rnd Bool
  , csecretStrength   :: Random.RollDice
  , citemNum          :: Random.RollDice
  , defTile           :: TileKind -> Bool
  , corTile           :: TileKind -> Bool
  }

instance Show CaveKind where
  show _ = "A cave kind specification." -- TODO

-- | Catch caves with not enough space for all the rooms.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{..} ->
  let maxGridX = Random.maxDice $ fst levelGrid
      maxGridY = Random.maxDice $ snd levelGrid
      maxRoomSizeX = Random.maxDice $ fst minRoomSize
      maxRoomSizeY = Random.maxDice $ snd minRoomSize
  in maxGridX * (2 * border + maxRoomSizeX) > cxsize ||
     maxGridY * (2 * border + maxRoomSizeY) > cysize)
