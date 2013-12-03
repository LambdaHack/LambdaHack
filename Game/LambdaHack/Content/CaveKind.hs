-- | The type of cave layout kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate
  ) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random

-- | Parameters for the generation of dungeon levels.
data CaveKind = CaveKind
  { csymbol         :: !Char        -- ^ a symbol
  , cname           :: !Text        -- ^ short description
  , cfreq           :: !Freqs       -- ^ frequency within groups
  , cxsize          :: !X           -- ^ X size of the whole cave
  , cysize          :: !Y           -- ^ Y size of the whole cave
  , cgrid           :: !RollDiceXY  -- ^ the dimensions of the grid of places
  , cminPlaceSize   :: !RollDiceXY  -- ^ minimal size of places
  , cmaxPlaceSize   :: !RollDiceXY  -- ^ maximal size of places
  , cdarkChance     :: !RollDeep    -- ^ the chance a place is dark
  , cauxConnects    :: !Rational    -- ^ a proportion of extra connections
  , cvoidChance     :: !Chance      -- ^ the chance of not creating a place
  , cnonVoidMin     :: !Int         -- ^ extra places, may overlap except two
  , cminStairDist   :: !Int         -- ^ minimal distance between stairs
  , cdoorChance     :: !Chance      -- ^ the chance of a door in an opening
  , copenChance     :: !Chance      -- ^ if there's a door, is it open?
  , chidden         :: !Int         -- ^ if not open, hidden one in n times
  , citemNum        :: !RollDice    -- ^ the number of items in the cave
  , cdefTile        :: !Text        -- ^ the default cave tile group name
  , ccorridorTile   :: !Text        -- ^ the cave corridor tile group name
  , cfillerTile     :: !Text        -- ^ the filler wall group name
  , cdarkLegendTile :: !Text        -- ^ the dark place plan legend ground name
  , clitLegendTile  :: !Text        -- ^ the lit place plan legend ground name
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- TODO: check many things, e.g., if all items and actors fit in the dungeon.
-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen.
cvalidate :: [CaveKind] -> [CaveKind]
cvalidate = L.filter (\ CaveKind{ cgrid
                                , cminPlaceSize
                                , cmaxPlaceSize
                                , ..
                                } ->
  let (maxGridX, maxGridY) = maxDiceXY cgrid
      (minMinSizeX, minMinSizeY) = minDiceXY cminPlaceSize
      (maxMinSizeX, maxMinSizeY) = maxDiceXY cminPlaceSize
      (minMaxSizeX, minMaxSizeY) = minDiceXY cmaxPlaceSize
      xborder = if maxGridX == 1 then 3 else 1
      yborder = if maxGridX == 1 then 3 else 1
  in T.length cname > 25
     || minMinSizeX < 1
     || minMinSizeY < 1
     || minMaxSizeX < maxMinSizeX
     || minMaxSizeY < maxMinSizeY
     || maxGridX * (maxMinSizeX + xborder) >= cxsize
     || maxGridY * (maxMinSizeY + yborder) >= cysize)
