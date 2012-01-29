-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/Mikolaj/Allure/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.FOV
  ( FovMode(..), fullscan
  ) where

import qualified Data.List as L

import Game.LambdaHack.FOV.Common
import qualified Game.LambdaHack.FOV.Digital as Digital
import qualified Game.LambdaHack.FOV.Permissive as Permissive
import qualified Game.LambdaHack.FOV.Shadow as Shadow
import Game.LambdaHack.Point
import Game.LambdaHack.VectorXY
import Game.LambdaHack.Vector
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Tile as Tile

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
-- | Field Of View scanning mode.
data FovMode =
    Shadow       -- ^ restrictive shadow casting
  | Permissive   -- ^ permissive FOV
  | Digital Int  -- ^ digital FOV with the given radius
  | Blind        -- ^ only feeling out adjacent tiles by touch
  deriving Show

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use, passed in the second argument, is set in the config file.
fullscan :: Kind.Ops TileKind  -- ^ tile content, determines clear tiles
         -> FovMode            -- ^ scanning mode
         -> Point              -- ^ location of the spectacor
         -> Level              -- ^ the map that is scanned
         -> [Point]
fullscan cotile fovMode loc Level{lxsize, lmap} =
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
  isCl = Tile.isClear cotile . (lmap Kind.!)

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
