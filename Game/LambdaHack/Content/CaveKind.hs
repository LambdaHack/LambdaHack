-- | The type of cave kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), validateSingleCaveKind, validateAllCaveKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.TileKind (TileKind)

-- | Parameters for the generation of dungeon levels.
-- Warning: for efficiency, avoid embedded items in any of the common tiles.
data CaveKind = CaveKind
  { csymbol         :: Char             -- ^ a symbol
  , cname           :: Text             -- ^ short description
  , cfreq           :: Freqs CaveKind   -- ^ frequency within groups
  , cxsize          :: X                -- ^ X size of the whole cave
  , cysize          :: Y                -- ^ Y size of the whole cave
  , cgrid           :: Dice.DiceXY
      -- ^ the dimensions of the grid of places
  , cminPlaceSize   :: Dice.DiceXY      -- ^ minimal size of places; for merging
  , cmaxPlaceSize   :: Dice.DiceXY      -- ^ maximal size of places
  , cdarkChance     :: Dice.Dice        -- ^ the chance a place is dark
  , cnightChance    :: Dice.Dice        -- ^ the chance the cave is dark
  , cauxConnects    :: Rational         -- ^ a proportion of extra connections
  , cmaxVoid        :: Rational
      -- ^ at most this proportion of rooms may be void
  , cminStairDist   :: Int              -- ^ minimal distance between stairs
  , cextraStairs    :: Dice.Dice        -- ^ extra stairs on top of from above
  , cdoorChance     :: Chance           -- ^ the chance of a door in an opening
  , copenChance     :: Chance           -- ^ if there's a door, is it open?
  , chidden         :: Int              -- ^ if not open, hidden one in n times
  , cactorCoeff     :: Int              -- ^ the lower, the more monsters spawn
  , cactorFreq      :: Freqs ItemKind   -- ^ actor groups to consider
  , citemNum        :: Dice.Dice        -- ^ the number of items in the cave
  , citemFreq       :: Freqs ItemKind   -- ^ item groups to consider
  , cplaceFreq      :: Freqs PlaceKind  -- ^ place groups to consider
  , cpassable       :: Bool
      -- ^ are passable default tiles permitted
  , cdefTile        :: GroupName TileKind  -- ^ the default cave tile
  , cdarkCorTile    :: GroupName TileKind  -- ^ the dark cave corridor tile
  , clitCorTile     :: GroupName TileKind  -- ^ the lit cave corridor tile
  , cfillerTile     :: GroupName TileKind  -- ^ the filler wall
  , couterFenceTile :: GroupName TileKind  -- ^ the outer fence wall
  , clegendDarkTile :: GroupName TileKind  -- ^ the dark place plan legend
  , clegendLitTile  :: GroupName TileKind  -- ^ the lit place plan legend
  , cescapeGroup    :: Maybe (GroupName PlaceKind)  -- ^ escape, if any
  , cstairFreq      :: Freqs PlaceKind
      -- ^ place groups to consider for stairs; in this case the rarity
      --   of items in the group does not affect group choice
  , cdesc           :: Text                -- ^ cave description
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen. Etc.
validateSingleCaveKind :: CaveKind -> [Text]
validateSingleCaveKind CaveKind{..} =
  let (minGridX, minGridY) = Dice.minDiceXY cgrid
      (maxGridX, maxGridY) = Dice.maxDiceXY cgrid
      (minMinSizeX, minMinSizeY) = Dice.minDiceXY cminPlaceSize
      (maxMinSizeX, maxMinSizeY) = Dice.maxDiceXY cminPlaceSize
      (minMaxSizeX, minMaxSizeY) = Dice.minDiceXY cmaxPlaceSize
      xborder = if couterFenceTile /= "basic outer fence" then 2 else 0
      yborder = if couterFenceTile /= "basic outer fence" then 2 else 0
  in [ "cname longer than 25" | T.length cname > 25 ]
     ++ [ "cxsize < 7" | cxsize < 7 ]
     ++ [ "cysize < 7" | cysize < 7 ]
     ++ [ "minGridX < 1" | minGridX < 1 ]
     ++ [ "minGridY < 1" | minGridY < 1 ]
     ++ [ "minMinSizeX < 1" | minMinSizeX < 1 ]
     ++ [ "minMinSizeY < 1" | minMinSizeY < 1 ]
     ++ [ "minMaxSizeX < maxMinSizeX" | minMaxSizeX < maxMinSizeX ]
     ++ [ "minMaxSizeY < maxMinSizeY" | minMaxSizeY < maxMinSizeY ]
     ++ [ "cxsize too small"
        | maxGridX * (maxMinSizeX - 4) + xborder >= cxsize ]
     ++ [ "cysize too small"
        | maxGridY * maxMinSizeY + yborder >= cysize ]
     ++ [ "cextraStairs < 0" | Dice.minDice cextraStairs < 0 ]
     ++ [ "chidden < 0" | chidden < 0 ]
     ++ [ "cactorCoeff < 0" | cactorCoeff < 0 ]
     ++ [ "citemNum < 0" | Dice.minDice citemNum < 0 ]

-- | Validate all cave kinds.
-- Note that names don't have to be unique: we can have several variants
-- of a cave with a given name.
validateAllCaveKind :: [CaveKind] -> [Text]
validateAllCaveKind lk =
  if any (maybe False (> 0) . lookup "default random" . cfreq) lk
  then []
  else ["no cave defined for \"default random\""]
