-- | Field Of View scanning.
module Game.LambdaHack.FOV
  ( FovMode(..), fullscan
  ) where

import qualified Data.List as L

import Game.LambdaHack.FOV.Common
import qualified Game.LambdaHack.FOV.Digital as Digital
import qualified Game.LambdaHack.FOV.Permissive as Permissive
import qualified Game.LambdaHack.FOV.Shadow as Shadow
import Game.LambdaHack.Loc
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

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use, passed in the second argument, is set in the config file.
fullscan :: Kind.Ops TileKind  -- ^ tile content, determines clear tiles
         -> FovMode            -- ^ scanning mode
         -> Loc                -- ^ location of the spectacor
         -> Level              -- ^ the map that is scanned
         -> [Loc]
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
  isCl :: Loc -> Bool
  isCl = Tile.isClear cotile . (lmap Kind.!)

  trL = trLoc lxsize loc

  -- | The translation, rotation and symmetry functions for octants.
  tr8 :: [(Distance, Progress) -> Loc]
  tr8 =
    [ \ (p, d) -> trL (  p,   d)
    , \ (p, d) -> trL (- p,   d)
    , \ (p, d) -> trL (  p, - d)
    , \ (p, d) -> trL (- p, - d)
    , \ (p, d) -> trL (  d,   p)
    , \ (p, d) -> trL (- d,   p)
    , \ (p, d) -> trL (  d, - p)
    , \ (p, d) -> trL (- d, - p)
    ]

  -- | The translation and rotation functions for quadrants.
  tr4 :: [Bump -> Loc]
  tr4 =
    [ \ (B(x, y)) -> trL (  x, - y)  -- quadrant I
    , \ (B(x, y)) -> trL (  y,   x)  -- II (we rotate counter-clockwise)
    , \ (B(x, y)) -> trL (- x,   y)  -- III
    , \ (B(x, y)) -> trL (- y, - x)  -- IV
    ]
