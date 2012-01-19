module Game.LambdaHack.Tile
  ( SecretStrength(..), SmellTime(..)
  , unknownId, kindHasFeature, kindHas, hasFeature
  , isClear, isLit, similar, canBeHidden
  ) where

import qualified Data.List as L
import Data.Binary

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.PointXY

-- | There is not type Tile, of particular concrete tiles in the dungeon,
-- corresponding corresponding to type TileKind, of kinds of terrain tiles.
-- This is because the tiles are too numerous to make
-- the type complex and big enough, on one hand,
-- and accessed too often in performance critial code to try to compress
-- and recompute the values, on the other hand. Instead, various properties
-- of concrete tiles are expressed by arrays or sparse IntMaps, as required.

-- | The type of secrecy strength of hidden terrain tiles (e.g., doors).
newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

newtype SmellTime = SmellTime{smelltime :: Time} deriving Show
instance Binary SmellTime where
  put = put . smelltime
  get = fmap SmellTime get

unknownId :: Kind.Ops TileKind -> Kind.Id TileKind
unknownId Kind.Ops{ouniqGroup} = ouniqGroup "unknown space"

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` tfeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature :: Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> Bool
hasFeature Kind.Ops{okind} f t =
  kindHasFeature f (okind t)

-- | Does not block vision. Essential for efficiency of FOV, hence tabulated.
isClear :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isClear Kind.Ops{ospeedup = isClearTab : _} = isClearTab
isClear Kind.Ops{ospeedup} = assert `failure` L.length ospeedup

-- | Is lit on its own. Essential for efficiency of Perception, hence tabulated.
isLit :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isLit Kind.Ops{ospeedup = _ : isLitTab : _} = isLitTab
isLit Kind.Ops{ospeedup} = assert `failure` L.length ospeedup

-- | The player can't one tile from the other.
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
