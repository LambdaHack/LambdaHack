-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/kosmikus/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( fullscan
  ) where

import qualified Data.List as L

import Game.LambdaHack.Config
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.Server.Fov.Common
import qualified Game.LambdaHack.Server.Fov.Digital as Digital
import qualified Game.LambdaHack.Server.Fov.Permissive as Permissive
import qualified Game.LambdaHack.Server.Fov.Shadow as Shadow
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Vector
import Game.LambdaHack.VectorXY

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use, passed in the second argument, is set in the config file.
fullscan :: Kind.Ops TileKind  -- ^ tile content, determines clear tiles
         -> FovMode            -- ^ scanning mode
         -> Point              -- ^ position of the spectacor
         -> Level              -- ^ the map that is scanned
         -> [Point]
fullscan cotile fovMode loc Level{lxsize, ltile} =
  case fovMode of
    Shadow ->
      L.concatMap (\ tr -> map tr (Shadow.scan (isCl . tr) 1 (0, 1))) tr8
    Permissive ->
      L.concatMap (\ tr -> map tr (Permissive.scan (isCl . tr))) tr4
    Digital r ->
      L.concatMap (\ tr -> map tr (Digital.scan r (isCl . tr))) tr4
    Blind ->
      let radiusOne = 1
      in L.concatMap (\ tr -> map tr (Digital.scan radiusOne (isCl . tr))) tr4
 where
  isCl :: Point -> Bool
  isCl = Tile.isClear cotile . (ltile Kind.!)

  trV xy = shift loc $ toVector lxsize $ VectorXY xy

  -- | The translation, rotation and symmetry functions for octants.
  tr8 :: [(Distance, Progress) -> Point]
  tr8 =
    [ \ (p, d) -> trV (  p,   d)
    , \ (p, d) -> trV (- p,   d)
    , \ (p, d) -> trV (  p, - d)
    , \ (p, d) -> trV (- p, - d)
    , \ (p, d) -> trV (  d,   p)
    , \ (p, d) -> trV (- d,   p)
    , \ (p, d) -> trV (  d, - p)
    , \ (p, d) -> trV (- d, - p)
    ]

  -- | The translation and rotation functions for quadrants.
  tr4 :: [Bump -> Point]
  tr4 =
    [ \ (B(x, y)) -> trV (  x, - y)  -- quadrant I
    , \ (B(x, y)) -> trV (  y,   x)  -- II (we rotate counter-clockwise)
    , \ (B(x, y)) -> trV (- x,   y)  -- III
    , \ (B(x, y)) -> trV (- y, - x)  -- IV
    ]
