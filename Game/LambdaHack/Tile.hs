module Game.LambdaHack.Tile where

import qualified Data.List as L
import Data.Binary

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Geometry
import Game.LambdaHack.Random

newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

-- TODO: remove this file

wallId, floorRoomLitId, floorRoomDarkId, doorOpenId, doorClosedId, doorSecretId, stairsUpId, stairsDownId :: Kind.Ops TileKind -> Rnd (Kind.Id TileKind)
wallId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '#' && L.null (tfeature t)
floorRoomLitId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '.' && kindHas [F.Lit, F.Boring] [F.Special] t
floorRoomDarkId Kind.Ops{opick} =
  opick $ \ t -> tsymbol t == '.' && kindHas [F.Boring] [F.Lit, F.Special] t
doorOpenId Kind.Ops{opick} = opick $ kindHasFeature F.Closable
doorClosedId Kind.Ops{opick} = opick $ kindHasFeature F.Openable
doorSecretId Kind.Ops{opick} = opick isdoorSecretKind
stairsUpId Kind.Ops{opick} = opick $ kindHasFeature F.Climbable
stairsDownId Kind.Ops{opick} = opick $ kindHasFeature F.Descendable

unknownId :: Kind.Ops TileKind -> Kind.Id TileKind
unknownId Kind.Ops{ouniqSymbol} = ouniqSymbol ' '

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
canBeSecretDoor Kind.Ops{okind, ofoldrWithKey} t =
  let u = okind t
      similar _ s acc = acc ||
        isdoorSecretKind s &&
        tsymbol u == tsymbol s &&
        tname u == tname s &&
        tcolor u == tcolor s &&
        tcolor2 u == tcolor2 s
  in ofoldrWithKey similar False

isdoorSecretKind :: TileKind -> Bool
isdoorSecretKind = kindHasFeature F.Hidden

isdoorSecretId :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isdoorSecretId Kind.Ops{okind} = isdoorSecretKind . okind

isUnknownKind :: TileKind -> Bool
isUnknownKind = (== ' ') . tsymbol

isUnknownId :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isUnknownId Kind.Ops{okind} = isUnknownKind . okind

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` tfeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature ::Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> Bool
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
