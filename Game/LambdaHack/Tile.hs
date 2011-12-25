module Game.LambdaHack.Tile
  ( wallId, floorRoomLitId, floorRoomDarkId, doorOpenId, doorClosedId
  , doorSecretId, stairsUpId, stairsDownId, unknownId, kindHasFeature
  , kindHas, hasFeature, isClear, isLit
  ) where

import qualified Data.List as L

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random

wallId, floorRoomLitId, floorRoomDarkId, doorOpenId, doorClosedId, doorSecretId, stairsUpId, stairsDownId :: Kind.Ops TileKind -> Rnd (Kind.Id TileKind)
wallId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '#' && L.null (tfeature t)
floorRoomLitId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '.' && kindHas [F.Lit, F.Boring] [F.Special] t
floorRoomDarkId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '.' && kindHas [F.Boring] [F.Lit, F.Special] t
doorOpenId Kind.Ops{opick} = opick $ kindHasFeature F.Closable
doorClosedId Kind.Ops{opick} = opick $ kindHasFeature F.Openable
doorSecretId Kind.Ops{opick} = opick $ kindHasFeature F.Hidden
stairsUpId Kind.Ops{opick} = opick $ kindHasFeature F.Climbable
stairsDownId Kind.Ops{opick} = opick $ kindHasFeature F.Descendable

unknownId :: Kind.Ops TileKind -> Kind.Id TileKind
unknownId Kind.Ops{ouniqName} = ouniqName "unknown space"

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
