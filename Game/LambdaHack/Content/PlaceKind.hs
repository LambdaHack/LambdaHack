-- | The type of kinds of rooms, halls and passages.
module Game.LambdaHack.Content.PlaceKind
  ( PlaceKind(..), makeDef
  , Cover(..), Fence(..)
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.ContentData
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.TileKind (TileKind)

-- | Parameters for the generation of small areas within a dungeon level.
data PlaceKind = PlaceKind
  { psymbol   :: Char          -- ^ a symbol
  , pname     :: Text          -- ^ short description
  , pfreq     :: Freqs PlaceKind  -- ^ frequency within groups
  , prarity   :: Rarity        -- ^ rarity on given depths
  , pcover    :: Cover         -- ^ how to fill whole place based on the corner
  , pfence    :: Fence         -- ^ whether to fence place with solid border
  , ptopLeft  :: [Text]        -- ^ plan of the top-left corner of the place
  , poverride :: [(Char, GroupName TileKind)]  -- ^ legend override
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | A method of filling the whole area (except for CVerbatim and CMirror,
-- which are just placed in the middle of the area) by transforming
-- a given corner.
data Cover =
    CAlternate  -- ^ reflect every other corner, overlapping 1 row and column
  | CStretch    -- ^ fill symmetrically 4 corners and stretch their borders
  | CReflect    -- ^ tile separately and symmetrically quarters of the place
  | CVerbatim   -- ^ just build the given interior, without filling the area
  | CMirror     -- ^ build the given interior in one of 4 mirrored variants
  deriving (Show, Eq)

-- | The choice of a fence type for the place.
data Fence =
    FWall   -- ^ put a solid wall fence around the place
  | FFloor  -- ^ leave an empty space, like the rooms floor
  | FGround -- ^ leave an empty space, like the caves ground
  | FNone   -- ^ skip the fence and fill all with the place proper
  deriving (Show, Eq)

-- | Catch invalid place kind definitions. In particular, verify that
-- the top-left corner map is rectangular and not empty.
validateSingle :: PlaceKind -> [Text]
validateSingle PlaceKind{..} =
  let dxcorner = case ptopLeft of
        [] -> 0
        l : _ -> T.length l
  in [ "top-left corner empty" | dxcorner == 0 ]
     ++ [ "top-left corner not rectangular"
        | any (/= dxcorner) (map T.length ptopLeft) ]
     ++ validateRarity prarity

-- | Validate all place kinds. Currently always valid.
validateAll :: [PlaceKind] -> [Text]
validateAll _ = []

makeDef :: [PlaceKind] -> ContentData PlaceKind
makeDef = makeContentData pname validateSingle validateAll pfreq
