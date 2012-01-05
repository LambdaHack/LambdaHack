module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry
import Game.LambdaHack.Random
import Game.LambdaHack.Content.TileKind

data CaveKind = CaveKind
  { csymbol       :: Char
  , cname         :: String
  , cfreq         :: Int
  , cdesc         :: String      -- ^ cave description for the player
  , cxsize        :: X
  , cysize        :: Y
  , cgrid         :: RollDiceXY
  , cminRoomSize  :: RollDiceXY
  , cdarkChance   :: RollQuad
  , cauxConnects  :: Rational    -- ^ not exact, because of duplications
  , croomChance   :: Chance
  , cminStairDist :: Int
  , cdoorChance   :: Chance
  , copenChance   :: Chance
  , csecretChance :: Chance
  , citemNum      :: RollDice
  , cdefTile      :: TileKind -> Bool
  , ccorTile      :: TileKind -> Bool
  }

instance Show CaveKind where
  show _ = "A cave kind specification." -- TODO

-- | Catch caves with not enough space for all the rooms.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{..} ->
  let maxGridX = maxDice $ fst cgrid
      maxGridY = maxDice $ snd cgrid
      maxRoomSizeX = maxDice $ fst cminRoomSize
      maxRoomSizeY = maxDice $ snd cminRoomSize
      xborder = if maxGridX == 1 then 5 else 3
      yborder = if maxGridX == 1 then 5 else 3
  in length cdesc <= 25
     && (maxGridX * (xborder + maxRoomSizeX) + 1 > cxsize ||
         maxGridY * (yborder + maxRoomSizeY) + 1 > cysize))
