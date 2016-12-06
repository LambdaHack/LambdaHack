-- | The type of cave layout kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), validateSingleCaveKind, validateAllCaveKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)

-- | Parameters for the generation of dungeon levels.
-- Warning: for efficiency, avoid embedded items in any of the common tiles.
data CaveKind = CaveKind
  { csymbol         :: !Char         -- ^ a symbol
  , cname           :: !Text         -- ^ short description
  , cfreq           :: !(Freqs CaveKind)  -- ^ frequency within groups
  , cxsize          :: !X            -- ^ X size of the whole cave
  , cysize          :: !Y            -- ^ Y size of the whole cave
  , cgrid           :: !Dice.DiceXY  -- ^ the dimensions of the grid of places
  , cminPlaceSize   :: !Dice.DiceXY  -- ^ minimal size of places; for merging
  , cmaxPlaceSize   :: !Dice.DiceXY  -- ^ maximal size of places
  , cdarkChance     :: !Dice.Dice    -- ^ the chance a place is dark
  , cnightChance    :: !Dice.Dice    -- ^ the chance the cave is dark
  , cauxConnects    :: !Rational     -- ^ a proportion of extra connections
  , cvoidChance     :: !Chance       -- ^ chance of eligible place becoming void
  , cminStairDist   :: !Int          -- ^ minimal distance between stairs
  , cextraStairs    :: !Dice.Dice    -- ^ extra stairs on top of from above
  , cdoorChance     :: !Chance       -- ^ the chance of a door in an opening
  , copenChance     :: !Chance       -- ^ if there's a door, is it open?
  , chidden         :: !Int          -- ^ if not open, hidden one in n times
  , cactorCoeff     :: !Int          -- ^ the lower, the more monsters spawn
  , cactorFreq      :: !(Freqs ItemKind)  -- ^ actor groups to consider
  , citemNum        :: !Dice.Dice    -- ^ the number of items in the cave
  , citemFreq       :: !(Freqs ItemKind)
                                     -- ^ item groups to consider
  , cplaceFreq      :: !(Freqs PlaceKind)
                                     -- ^ place groups to consider
  , cpassable       :: !Bool         -- ^ are passable default tiles permitted
  , cdefTile        :: !(GroupName TileKind)  -- ^ the default cave tile
  , cdarkCorTile    :: !(GroupName TileKind)  -- ^ the dark cave corridor tile
  , clitCorTile     :: !(GroupName TileKind)  -- ^ the lit cave corridor tile
  , cfillerTile     :: !(GroupName TileKind)  -- ^ the filler wall
  , couterFenceTile :: !(GroupName TileKind)  -- ^ the outer fence wall
  , clegendDarkTile :: !(GroupName TileKind)  -- ^ the dark place plan legend
  , clegendLitTile  :: !(GroupName TileKind)  -- ^ the lit place plan legend
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen. Etc.
validateSingleCaveKind :: CaveKind -> [Text]
validateSingleCaveKind CaveKind{..} =
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
  in [ "cname longer than 25" | T.length cname > 25 ]
     ++ [ "cxsize < 7" | cxsize < 7 ]
     ++ [ "cysize < 7" | cysize < 7 ]
     ++ [ "minMinSizeX < 1" | minMinSizeX < 1 ]
     ++ [ "minMinSizeY < 1" | minMinSizeY < 1 ]
     ++ [ "minMaxSizeX < maxMinSizeX" | minMaxSizeX < maxMinSizeX ]
     ++ [ "minMaxSizeY < maxMinSizeY" | minMaxSizeY < maxMinSizeY ]
     ++ [ "cxsize too small"  -- we check that size one less would fit
        | maxGridX * maxMinSizeX + xborder >= cxsize ]
     ++ [ "cysize too small"  -- we check that size one less would fit
        | maxGridY * maxMinSizeY + yborder >= cysize ]

-- | Validate all cave kinds.
-- Note that names don't have to be unique: we can have several variants
-- of a cave with a given name.
validateAllCaveKind :: [CaveKind] -> [Text]
validateAllCaveKind lk =
  if any (maybe False (> 0) . lookup "campaign random" . cfreq) lk
  then []
  else ["no cave defined for \"campaign random\""]
