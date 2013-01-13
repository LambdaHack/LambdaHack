{-# LANGUAGE DeriveDataTypeable #-}
-- | The type of cave layout kinds.
module Game.LambdaHack.Content.CaveKind
  ( CaveKind(..), cvalidate, LevelId(..)
  ) where

import Data.Binary
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import Game.LambdaHack.Misc
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random

-- | Parameters for the generation of dungeon levels.
data CaveKind = CaveKind
  { csymbol         :: Char        -- ^ a symbol
  , cname           :: Text        -- ^ short description
  , cfreq           :: Freqs       -- ^ frequency within groups
  , cxsize          :: X           -- ^ X size of the whole cave
  , cysize          :: Y           -- ^ Y size of the whole cave
  , cgrid           :: RollDiceXY  -- ^ the dimensions of the grid of places
  , cminPlaceSize   :: RollDiceXY  -- ^ minimal size of places
  , cdarkChance     :: RollDeep    -- ^ the chance a place is dark
  , cauxConnects    :: Rational    -- ^ a proportion of extra connections
  , cvoidChance     :: Chance      -- ^ the chance of not creating a place
  , cnonVoidMin     :: Int         -- ^ extra places, may overlap except two
  , cminStairDist   :: Int         -- ^ minimal distance between stairs
  , cdoorChance     :: Chance      -- ^ the chance of a door in an opening
  , copenChance     :: Chance      -- ^ if there's a door, is it open?
  , chiddenChance   :: Chance      -- ^ if not open, is it hidden?
  , citemNum        :: RollDice    -- ^ the number of items in the cave
  , cdefTile        :: Text      -- ^ the default cave tile group name
  , ccorridorTile   :: Text      -- ^ the cave corridor tile group name
  , cfillerTile     :: Text      -- ^ the filler wall group name
  , cdarkLegendTile :: Text      -- ^ the dark place plan legend ground name
  , clitLegendTile  :: Text      -- ^ the lit place plan legend ground name
  , chiddenTile     :: Text      -- ^ the hidden tiles ground name
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
  in T.length cname <= 25
     && (maxGridX * (xborder + maxPlaceSizeX) + 1 > cxsize ||
         maxGridY * (yborder + maxPlaceSizeY) + 1 > cysize))

-- TODO: will probably be used to say in which dungeon branches
-- a given cave can be generated.
-- | Level ids are, for now, ordered linearly by depth.
newtype LevelId = LambdaCave Int
  deriving (Show, Eq, Ord, Typeable)

instance Binary LevelId where
  put (LambdaCave n) = put n
  get = fmap LambdaCave get
