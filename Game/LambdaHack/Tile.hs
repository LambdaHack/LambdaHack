-- | Operations concerning dungeon level tiles.
--
-- Unlike for many other content types, there is no type @Tile@,
-- of particular concrete tiles in the dungeon,
-- corresponding to 'TileKind' (the type of kinds of terrain tiles).
-- This is because the tiles are too numerous and there's not enough
-- storage space for a well-rounded @Tile@ type, on one hand,
-- and on the other hand, tiles are accessed
-- too often in performance critical code
-- to try to compress their representation and/or recompute them.
-- Instead, of defining a @Tile@ type, we express various properties
-- of concrete tiles by arrays or sparse IntMaps, as appropriate.
module Game.LambdaHack.Tile
  ( SecretStrength(..), SmellTime(..)
  , kindHasFeature, kindHas, hasFeature
  , isClear, isLit, similar, canBeHidden
  ) where

import qualified Data.List as L
import Data.Binary

import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Misc

-- | The type of secrecy strength of hidden terrain tiles (e.g., doors).
newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

-- | The last time a hero left a smell in a given tile. To be used
-- by monsters that hunt by smell.
newtype SmellTime = SmellTime{smelltime :: Time} deriving Show
instance Binary SmellTime where
  put = put . smelltime
  get = fmap SmellTime get

-- | Whether a tile kind has the given feature.
kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` tfeature t

-- | Whether a tile kind has all features of the first set
-- and no features of the second.
kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (`kindHasFeature` t) yes
                   && not (L.any (`kindHasFeature` t) no)

-- | Whether a tile kind (specified by its id) has the given feature.
hasFeature :: Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> Bool
hasFeature Kind.Ops{okind} f t =
  kindHasFeature f (okind t)

-- | Whether a tile does not block vision.
-- Essential for efficiency of "FOV", hence tabulated.
isClear :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isClear Kind.Ops{ospeedup = Kind.TileSpeedup{isClearTab}} = isClearTab

-- | Whether a tile is lit on its own.
-- Essential for efficiency of "Perception", hence tabulated.
isLit :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isLit Kind.Ops{ospeedup = Kind.TileSpeedup{isLitTab}} = isLitTab

-- | The player can't tell one tile from the other.
similar :: TileKind -> TileKind -> Bool
similar t u =
  tsymbol t == tsymbol u &&
  tname   t == tname   u &&
  tcolor  t == tcolor  u &&
  tcolor2 t == tcolor2 u

-- | The player can't tell if the tile is hidden or not.
canBeHidden :: Kind.Ops TileKind -> TileKind -> Bool
canBeHidden Kind.Ops{ofoldrWithKey} t =
  let sim _ s acc = acc || kindHasFeature F.Hidden s && similar t s
  in ofoldrWithKey sim False
