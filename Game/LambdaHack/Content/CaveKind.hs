module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.TileKind

data CaveKind = CaveKind
  { csymbol       :: Char
  , cname         :: String
  , cfreq         :: Int
  , cdesc         :: String  -- ^ cave description for the player
  , cxsize        :: X
  , cysize        :: Y
  , cgrid         :: Random.RollDiceXY
  , cminRoomSize  :: Random.RollDiceXY
  , cdarkChance   :: Int -> Random.Rnd Bool  -- TODO: use RollQuad instead, etc.
  , cauxConnects  :: Rational    -- ^ not exact, because of duplications
  , croomChance   :: Random.Chance
  , cminStairDist :: Int
  , cdoorChance   :: Random.Chance
  , copenChance   :: Random.Chance
  , csecretChance :: Random.Chance
  , citemNum      :: Random.RollDice
  , cdefTile      :: TileKind -> Bool
  , ccorTile      :: TileKind -> Bool
  }

instance Show CaveKind where
  show _ = "A cave kind specification." -- TODO

-- | Catch caves with not enough space for all the rooms.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{..} ->
  let maxGridX = Random.maxDice $ fst cgrid
      maxGridY = Random.maxDice $ snd cgrid
      maxRoomSizeX = Random.maxDice $ fst cminRoomSize
      maxRoomSizeY = Random.maxDice $ snd cminRoomSize
      xborder = if maxGridX == 1 then 5 else 3
      yborder = if maxGridX == 1 then 5 else 3
  in length cdesc <= 25
     && (maxGridX * (xborder + maxRoomSizeX) + 1 > cxsize ||
         maxGridY * (yborder + maxRoomSizeY) + 1 > cysize))
