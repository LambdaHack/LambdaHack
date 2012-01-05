module Game.LambdaHack.Content.PlaceKind
  ( PlaceKind(..), Cover(..), Fence(..), rvalidate
  ) where

import qualified Data.List as L

data PlaceKind = PlaceKind
  { rsymbol  :: Char
  , rname    :: String
  , rfreq    :: Int
  , rcover   :: Cover     -- ^ how to fill whole place based on the corner
  , rfence   :: Fence     -- ^ whether to fence the place with solid border
  , rtopLeft :: [String]  -- ^ plan of the top-left corner of the place
  }
  deriving Show

data Cover =
    CTile     -- ^ tile the corner plan, cutting off at the right and bottom
  | CStretch  -- ^ fill symmetrically all corners and stretch their borders
  | CReflect  -- ^ tile separately and symmetrically the quarters of the place
  deriving (Show, Eq)

data Fence =
    FWall   -- ^ put a solid wall fence around the place
  | FFloor  -- ^ leave an empty floor space around the place
  | FNone   -- ^ skip the fence and fill all with the place proper
  deriving (Show, Eq)

-- | Verify that the top-left corner map is rectangular and not empty.
-- TODO: Verify that places are fully accessible from any entrace on the fence
-- that is at least 4 tiles distant from the edges, if the place is big enough,
-- (unless the place has FNone fence, in which case the entrance is
-- at the outer tiles of the place).
-- TODO: Check that all symbols in place plans are covered in tile content.
rvalidate :: [PlaceKind] -> [PlaceKind]
rvalidate = L.filter (\ PlaceKind{..} ->
  let dxcorner = case rtopLeft of [] -> 0 ; l : _ -> L.length l
  in dxcorner /= 0 && L.any (/= dxcorner) (L.map L.length rtopLeft))
