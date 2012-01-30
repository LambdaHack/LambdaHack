-- | The type of cave layout kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L

import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.Misc

-- | Parameters for the generation of dungeon levels.
data CaveKind = CaveKind
  { csymbol       :: Char        -- ^ a symbol
  , cname         :: String      -- ^ short description
  , cfreq         :: Freqs       -- ^ frequency within groups
  , cxsize        :: X           -- ^ X size of the whole cave
  , cysize        :: Y           -- ^ Y size of the whole cave
  , cgrid         :: RollDiceXY  -- ^ the dimensions of the grid of places
  , cminPlaceSize :: RollDiceXY  -- ^ minimal size of places
  , cdarkChance   :: RollDeep    -- ^ the chance a place is dark
  , cauxConnects  :: Rational    -- ^ a proportion of extra connections
  , cvoidChance   :: Chance      -- ^ the chance of not creating a place
  , cnonVoidMin   :: Int         -- ^ extra places, may overlap except two
  , cminStairDist :: Int         -- ^ minimal distance between stairs
  , cdoorChance   :: Chance      -- ^ the chance of a door in an opening
  , copenChance   :: Chance      -- ^ if there's a door, is it open?
  , chiddenChance :: Chance      -- ^ if not open, is it hidden?
  , citemNum      :: RollDice    -- ^ the number of items in the cave
  , cdefTile      :: String      -- ^ the default cave tile group name
  , ccorTile      :: String      -- ^ the cave corridor tile group name
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{ cgrid = RollDiceXY (gx, gy)
                                , cminPlaceSize = RollDiceXY (mx, my)
                                , ..
                                } ->
  let (maxGridX, maxGridY) = (maxDice gx, maxDice gy)
      (maxPlaceSizeX, maxPlaceSizeY) = (maxDice mx,  maxDice my)
      xborder = if maxGridX == 1 then 5 else 3
      yborder = if maxGridX == 1 then 5 else 3
  in length cname <= 25
     && (maxGridX * (xborder + maxPlaceSizeX) + 1 > cxsize ||
         maxGridY * (yborder + maxPlaceSizeY) + 1 > cysize))
