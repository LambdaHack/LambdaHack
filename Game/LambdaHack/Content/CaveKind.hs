-- | The type of cave layout kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), validateCaveKind
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

-- | Parameters for the generation of dungeon levels.
data CaveKind = CaveKind
  { csymbol         :: !Char         -- ^ a symbol
  , cname           :: !Text         -- ^ short description
  , cfreq           :: !Freqs        -- ^ frequency within groups
  , cxsize          :: !X            -- ^ X size of the whole cave
  , cysize          :: !Y            -- ^ Y size of the whole cave
  , cgrid           :: !Dice.DiceXY  -- ^ the dimensions of the grid of places
  , cminPlaceSize   :: !Dice.DiceXY  -- ^ minimal size of places
  , cmaxPlaceSize   :: !Dice.DiceXY  -- ^ maximal size of places
  , cdarkChance     :: !Dice.Dice    -- ^ the chance a place is dark
  , cnightChance    :: !Dice.Dice    -- ^ the chance the cave is dark
  , cauxConnects    :: !Rational     -- ^ a proportion of extra connections
  , cmaxVoid        :: !Rational     -- ^ at most this proportion of rooms void
  , cminStairDist   :: !Int          -- ^ minimal distance between stairs
  , cdoorChance     :: !Chance       -- ^ the chance of a door in an opening
  , copenChance     :: !Chance       -- ^ if there's a door, is it open?
  , chidden         :: !Int          -- ^ if not open, hidden one in n times
  , citemNum        :: !Dice.Dice    -- ^ the number of items in the cave
  , citemFreq       :: ![(Int, Text)]  -- ^ item groups to consider
  , cdefTile        :: !Text         -- ^ the default cave tile group name
  , cdarkCorTile    :: !Text         -- ^ the dark cave corridor tile group name
  , clitCorTile     :: !Text         -- ^ the lit cave corridor tile group name
  , cfillerTile     :: !Text         -- ^ the filler wall group name
  , couterFenceTile :: !Text         -- ^ the outer fence wall group name
  , clegendDarkTile :: !Text         -- ^ the dark place plan legend group name
  , clegendLitTile  :: !Text         -- ^ the lit place plan legend group name
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- TODO: check many things, e.g., if all items and actors fit in the dungeon.
-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen.
validateCaveKind :: [CaveKind] -> [CaveKind]
validateCaveKind = filter (\ CaveKind{..} ->
  let (maxGridX, maxGridY) = Dice.maxDiceXY cgrid
      (minMinSizeX, minMinSizeY) = Dice.minDiceXY cminPlaceSize
      (maxMinSizeX, maxMinSizeY) = Dice.maxDiceXY cminPlaceSize
      (minMaxSizeX, minMaxSizeY) = Dice.minDiceXY cmaxPlaceSize
      -- If there is at most one room, we need extra borders for a passage,
      -- but if there may be more rooms, we have that space, anyway,
      -- because multiple rooms take more space than borders.
      xborder = if maxGridX == 1 || couterFenceTile /= "basic outer fence"
                then 2
                else 0
      yborder = if maxGridY == 1 || couterFenceTile /= "basic outer fence"
                then 2
                else 0
  in T.length cname > 25
     || cxsize < 7
     || cysize < 7
     || minMinSizeX < 1
     || minMinSizeY < 1
     || minMaxSizeX < maxMinSizeX
     || minMaxSizeY < maxMinSizeY
     || maxGridX * (maxMinSizeX + 1) + xborder >= cxsize
     || maxGridY * (maxMinSizeY + 1) + yborder >= cysize)
