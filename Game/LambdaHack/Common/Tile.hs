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
-- of concrete tiles by arrays or sparse EnumMaps, as appropriate.
--
-- Actors at normal speed (2 m/s) take one turn to move one tile (1 m by 1 m).
module Game.LambdaHack.Common.Tile
  ( SmellTime
  , kindHasFeature, kindHas, hasFeature
  , isClear, isLit, isExplorable, similar, speedup
  , changeTo, hiddenAs
  ) where

import qualified Data.Array.Unboxed as A
import qualified Data.List as L

import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time

-- | The last time a hero left a smell in a given tile. To be used
-- by monsters that hunt by smell.
type SmellTime = Time

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

-- | Whether a tile can be explored, possibly yielding a treasure
-- or a hidden message. We exclude doors and hidden features.
isExplorable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isExplorable cops tk =
  not (hasFeature cops F.Closable tk)
  && (isClear cops tk
      || hasFeature cops F.Walkable tk)

-- | The player can't tell one tile from the other.
similar :: TileKind -> TileKind -> Bool
similar t u =
  tsymbol t == tsymbol u &&
  tname   t == tname   u &&
  tcolor  t == tcolor  u &&
  tcolor2 t == tcolor2 u

speedup :: Bool -> Kind.Ops TileKind -> Kind.Speedup TileKind
speedup allClear Kind.Ops{ofoldrWithKey, obounds} =
  let createTab :: (TileKind -> Bool) -> A.UArray (Kind.Id TileKind) Bool
      createTab p =
        let f _ k acc = p k : acc
            clearAssocs = ofoldrWithKey f []
        in A.listArray obounds clearAssocs
      tabulate :: (TileKind -> Bool) -> Kind.Id TileKind -> Bool
      tabulate p = (createTab p A.!)
      isClearTab | allClear = tabulate $ not . kindHasFeature F.Impenetrable
                 | otherwise = tabulate $ kindHasFeature F.Clear
      isLitTab   = tabulate $ kindHasFeature F.Lit
  in Kind.TileSpeedup {isClearTab, isLitTab}

changeTo :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
changeTo Kind.Ops{okind, opick} t =
  let getTo (F.ChangeTo group) _ = Just group
      getTo _ acc = acc
  in case foldr getTo Nothing (tfeature (okind t)) of
       Nothing    -> return t
       Just group -> opick group (const True)

hiddenAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Kind.Id TileKind
hiddenAs Kind.Ops{okind, ouniqGroup} t =
  let getTo (F.HiddenAs group) _ = Just group
      getTo _ acc = acc
  in case foldr getTo Nothing (tfeature (okind t)) of
       Nothing    -> t
       Just group -> ouniqGroup group
