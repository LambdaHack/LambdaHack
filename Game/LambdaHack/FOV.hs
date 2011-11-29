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
data FovMode = Shadow | Permissive Int | Digital Int | Blind

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use is set in the config file.
-- Press a command key in the game to cycle among the algorithms
-- and see a special visualization of their effects..
fullscan :: FovMode -> Loc -> Kind.Ops TileKind -> Level -> [Loc]
fullscan fovMode loc cops Level{lxsize, lmap} =
  case fovMode of
    Shadow ->  -- shadow casting with infinite range
      L.foldl' (\ acc tr -> Shadow.scan tr (isCl . tr) 1 (0, 1) acc)
        [] [tr0, tr1, tr2, tr3, tr4, tr5, tr6, tr7]
    Permissive r  ->  -- permissive with range r
      L.foldl' (\ acc tr -> Permissive.scan r tr (isCl . tr) acc)
        [] [qtr0, qtr1, qtr2, qtr3]
    Digital r ->  -- digital with range r
      L.foldl' (\ acc tr -> Digital.scan r tr (isCl . tr) acc)
        [] [qtr0, qtr1, qtr2, qtr3]
    Blind ->  -- only feeling out adjacent tiles by touch
      let radiusOne = 1
      in L.foldl' (\ acc tr -> Digital.scan radiusOne tr (isCl . tr) acc)
           [] [qtr0, qtr1, qtr2, qtr3]
 where
  isCl :: Loc -> Bool
  isCl = Tile.isClear cops . (lmap Kind.!)

  trL = trLoc lxsize

  -- | The translation, rotation and symmetry functions for octants.
  tr0, tr1, tr2, tr3, tr4, tr5, tr6, tr7 :: (Distance, Progress) -> Loc
  tr0 (p, d) = trL loc (  p,   d)
  tr1 (p, d) = trL loc (- p,   d)
  tr2 (p, d) = trL loc (  p, - d)
  tr3 (p, d) = trL loc (- p, - d)
  tr4 (p, d) = trL loc (  d,   p)
  tr5 (p, d) = trL loc (- d,   p)
  tr6 (p, d) = trL loc (  d, - p)
  tr7 (p, d) = trL loc (- d, - p)

  -- | The translation and rotation functions for quadrants.
  qtr0, qtr1, qtr2, qtr3 :: Bump -> Loc
  qtr0 (B(x, y)) = trL loc (  x, - y)  -- quadrant I
  qtr1 (B(x, y)) = trL loc (  y,   x)  -- II (we rotate counter-clockwise)
  qtr2 (B(x, y)) = trL loc (- x,   y)  -- III
  qtr3 (B(x, y)) = trL loc (- y, - x)  -- IV
