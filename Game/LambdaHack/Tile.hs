module Game.LambdaHack.Tile
  ( wallP, floorRoomLitP, floorRoomDarkP
  , floorCorridorLitP, floorCorridorDarkP, floorSpecialP
  , wallId, floorRoomLitId, floorRoomDarkId, stairsUpId, stairsDownId, unknownId
  , kindHasFeature, kindHas, hasFeature, isClear, isLit
  , similar, canBeHidden, changeTo
  ) where

import qualified Data.List as L

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random

wallP, floorRoomLitP, floorRoomDarkP, floorCorridorLitP, floorCorridorDarkP, floorSpecialP :: TileKind -> Bool
wallP t            = tfeature t == []
floorRoomLitP      = kindHas [F.Walkable, F.Clear, F.Lit, F.Boring]
                             [F.Special]
floorRoomDarkP     = kindHas [F.Walkable, F.Clear, F.Boring]
                             [F.Lit, F.Special]
floorCorridorLitP  = kindHas [F.Walkable, F.Clear, F.Lit]
                             [F.Exit, F.Special, F.Boring]
floorCorridorDarkP = kindHas [F.Walkable, F.Clear]
                             [F.Lit, F.Exit, F.Special, F.Boring]
floorSpecialP      = kindHas [F.Walkable, F.Clear, F.Lit, F.Special]
                             [F.Exit, F.Boring]

wallId, floorRoomLitId, floorRoomDarkId, stairsUpId, stairsDownId :: Kind.Ops TileKind -> Rnd (Kind.Id TileKind)
wallId Kind.Ops{opick} = opick wallP
floorRoomLitId Kind.Ops{opick} = opick floorRoomLitP
floorRoomDarkId Kind.Ops{opick} = opick floorRoomDarkP
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

changeTo :: Kind.Ops TileKind -> String -> Rnd (Kind.Id TileKind)
changeTo Kind.Ops{opick} name =
  let p tk = kindHasFeature (F.ChangeFrom name) tk
  in opick p
