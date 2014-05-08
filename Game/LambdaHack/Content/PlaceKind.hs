-- | The type of kinds of rooms, halls and passages.
module Game.LambdaHack.Content.PlaceKind
  ( PlaceKind(..), Cover(..), Fence(..), validatePlaceKind
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Misc

-- | Parameters for the generation of small areas within a dungeon level.
data PlaceKind = PlaceKind
  { psymbol   :: !Char      -- ^ a symbol
  , pname     :: !Text      -- ^ short description
  , pfreq     :: !Freqs     -- ^ frequency within groups
  , pcover    :: !Cover     -- ^ how to fill whole place based on the corner
  , pfence    :: !Fence     -- ^ whether to fence the place with solid border
  , ptopLeft  :: ![Text]    -- ^ plan of the top-left corner of the place
  , poverride :: ![(Char, Text)]  -- ^ legend override, ignoring tile symbol
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | A method of filling the whole area (except for CVerbatim, which is just
-- placed in the middle of the area), by transforming a given corner.
data Cover =
    CAlternate   -- ^ reflect every other corner, overlapping 1 row and column
  | CStretch     -- ^ fill symmetrically 4 corners and stretch their borders
  | CReflect     -- ^ tile separately and symmetrically quarters of the place
  | CVerbatim    -- ^ just build the given interior, without filling the area
  deriving (Show, Eq)

-- | The choice of a fence type for the place.
data Fence =
    FWall   -- ^ put a solid wall fence around the place
  | FFloor  -- ^ leave an empty floor space around the place
  | FNone   -- ^ skip the fence and fill all with the place proper
  deriving (Show, Eq)

-- TODO: Verify that places are fully accessible from any entrace on the fence
-- that is at least 4 tiles distant from the edges, if the place is big enough,
-- (unless the place has FNone fence, in which case the entrance is
-- at the outer tiles of the place).
-- TODO: Check that all symbols in place plans are present in the legend.
-- TODO: Add a field with tile group to be used as the legend.
-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Verify that the top-left corner map is rectangular and not empty.
validatePlaceKind :: [PlaceKind] -> [PlaceKind]
validatePlaceKind = filter (\ PlaceKind{..} ->
  let dxcorner = case ptopLeft of
        [] -> 0
        l : _ -> T.length l
  in dxcorner /= 0 && any (/= dxcorner) (map T.length ptopLeft))
