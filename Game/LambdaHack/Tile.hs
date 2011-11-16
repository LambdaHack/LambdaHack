module Game.LambdaHack.Tile where

import qualified Data.List as L
import Data.Binary

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Geometry

newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

-- TODO: remove this file

-- wallId, openingId, floorLightId, floorDarkId, unknownId, doorOpenId, doorClosedId, doorSecretId, stairsUpId, stairsDownId :: Kind.Ops TileKind -> Kind.Id TileKind
wallId Kind.Ops{ogetId} = ogetId (\ t -> tsymbol t == '#' && (L.null $ tfeature t))
openingId Kind.Ops{ogetId} = ogetId (\ t -> tsymbol t == '.' && kindHasFeature F.Exit t)
floorLightId Kind.Ops{ogetId} =
  ogetId (\ t -> tsymbol t == '.' && kindHas [F.Lit] [F.Exit] t)
floorDarkId Kind.Ops{ogetId} =
  ogetId (\ t -> tsymbol t == '.' && kindHas [] [F.Exit, F.Lit] t)
unknownId Kind.Ops{ogetId} = ogetId ((== ' ') . tsymbol)
doorOpenId Kind.Ops{ogetId} = ogetId (kindHasFeature F.Closable)
doorClosedId Kind.Ops{ogetId} = ogetId (kindHasFeature F.Openable)
doorSecretId Kind.Ops{ogetId} = ogetId (kindHasFeature F.Hidden)
stairsUpId Kind.Ops{opick} = opick $ kindHasFeature F.Climbable
stairsDownId Kind.Ops{opick} = opick $ kindHasFeature F.Descendable

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
canBeSecretDoor cops@Kind.Ops{okind} t =
  let u = okind t
      s = okind (doorSecretId cops)
  in tsymbol u == tsymbol s &&
     tname u == tname s &&
     tcolor u == tcolor s &&
     tcolor2 u == tcolor2 s

isUnknown :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isUnknown cops t = t == unknownId cops

isOpening :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isOpening cops t = t == openingId cops

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` tfeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature ::Kind.Ops TileKind ->  F.Feature -> Kind.Id TileKind -> Bool
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

-- | Does not block land movement.
isWalkable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isWalkable cops = hasFeature cops F.Walkable

-- | Provides an exit from a room.
isExit :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isExit cops = hasFeature cops F.Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isBoring Kind.Ops{okind} t =
  let fs = tfeature (okind t)
      optional = [F.Exit, F.Lit]
      mandatory = [F.Walkable, F.Clear]
  in fs L.\\ optional `L.elem` L.permutations mandatory
