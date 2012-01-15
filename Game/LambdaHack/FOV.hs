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
data FovMode = Shadow | Permissive | Digital Int | Blind

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use is set in the config file.
-- Press a command key in the game to cycle among the algorithms
-- and see a special visualization of their effects..
fullscan :: FovMode -> Loc -> Kind.Ops TileKind -> Level -> [Loc]
fullscan fovMode loc cops Level{lxsize, lmap} =
  case fovMode of
    Shadow ->
      L.concatMap (\ tr -> map tr (Shadow.scan (isCl . tr) 1 (0, 1))) tr8
    Permissive ->
      L.concatMap (\ tr -> map tr (Permissive.scan (isCl . tr))) tr4
    Digital r ->
      L.concatMap (\ tr -> map tr (Digital.scan r (isCl . tr))) tr4
    Blind ->  -- only feeling out adjacent tiles by touch
      let radiusOne = 1
      in L.concatMap (\ tr -> map tr (Digital.scan radiusOne (isCl . tr))) tr4
 where
  isCl :: Loc -> Bool
  isCl = Tile.isClear cops . (lmap Kind.!)

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
