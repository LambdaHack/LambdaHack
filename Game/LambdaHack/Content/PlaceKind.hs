-- | The type of kinds of rooms, halls and passages.
module Game.LambdaHack.Content.PlaceKind
  ( PlaceKind(..), Cover(..), Fence(..)
  , validateSinglePlaceKind, validateAllPlaceKind
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Misc

-- | Parameters for the generation of small areas within a dungeon level.
data PlaceKind = PlaceKind
  { psymbol   :: !Char          -- ^ a symbol
  , pname     :: !Text          -- ^ short description
  , pfreq     :: !Freqs         -- ^ frequency within groups
  , prarity   :: !Rarity        -- ^ rarity on given depths
  , pcover    :: !Cover         -- ^ how to fill whole place based on the corner
  , pfence    :: !Fence         -- ^ whether to fence place with solid border
  , ptopLeft  :: ![Text]        -- ^ plan of the top-left corner of the place
  , poverride :: ![(Char, GroupName)]  -- ^ legend override
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | A method of filling the whole area (except for CVerbatim, which is just
-- placed in the middle of the area) by transforming a given corner.
data Cover =
    CAlternate  -- ^ reflect every other corner, overlapping 1 row and column
  | CStretch    -- ^ fill symmetrically 4 corners and stretch their borders
  | CReflect    -- ^ tile separately and symmetrically quarters of the place
  | CVerbatim   -- ^ just build the given interior, without filling the area
  deriving (Show, Eq)

-- | The choice of a fence type for the place.
data Fence =
    FWall   -- ^ put a solid wall fence around the place
  | FFloor  -- ^ leave an empty space, like the rooms floor
  | FGround -- ^ leave an empty space, like the caves ground
  | FNone   -- ^ skip the fence and fill all with the place proper
  deriving (Show, Eq)

-- TODO: Verify that places are fully accessible from any entrace on the fence
-- that is at least 4 tiles distant from the edges, if the place is big enough,
-- (unless the place has FNone fence, in which case the entrance is
-- at the outer tiles of the place).
-- TODO: (spans multiple contents) Check that all symbols in place plans
-- are present in the legend.
-- | Catch invalid place kind definitions. In particular, verify that
-- the top-left corner map is rectangular and not empty.
validateSinglePlaceKind :: PlaceKind -> [Text]
validateSinglePlaceKind PlaceKind{..} =
  let dxcorner = case ptopLeft of
        [] -> 0
        l : _ -> T.length l
  in [ "top-left corner empty" | dxcorner == 0 ]
     ++ [ "top-left corner not rectangular"
        | any (/= dxcorner) (map T.length ptopLeft) ]
     ++ validateRarity prarity

-- | Validate all place kinds. Currently always valid.
validateAllPlaceKind :: [PlaceKind] -> [Text]
validateAllPlaceKind _ = []
