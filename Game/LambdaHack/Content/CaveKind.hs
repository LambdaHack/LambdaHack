module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry
import Game.LambdaHack.Random

data CaveKind = CaveKind
  { csymbol       :: Char
  , cname         :: String      -- ^ short cave description for the player
  , cfreq         :: [(String, Int)]  -- ^ frequency within groups
  , cxsize        :: X
  , cysize        :: Y
  , cgrid         :: RollDiceXY
  , cminPlaceSize :: RollDiceXY
  , cdarkChance   :: RollQuad
  , cauxConnects  :: Rational    -- ^ not exact, because of duplications
  , cplaceChance  :: Chance
  , cminStairDist :: Int
  , cdoorChance   :: Chance
  , copenChance   :: Chance
  , csecretChance :: Chance
  , citemNum      :: RollDice
  , cdefTile      :: String
  , ccorTile      :: String
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Catch caves with not enough space for all the places.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{..} ->
  let maxGridX = maxDice $ fst cgrid
      maxGridY = maxDice $ snd cgrid
      maxPlaceSizeX = maxDice $ fst cminPlaceSize
      maxPlaceSizeY = maxDice $ snd cminPlaceSize
      xborder = if maxGridX == 1 then 5 else 3
      yborder = if maxGridX == 1 then 5 else 3
  in length cname <= 25
     && (maxGridX * (xborder + maxPlaceSizeX) + 1 > cxsize ||
         maxGridY * (yborder + maxPlaceSizeY) + 1 > cysize))
