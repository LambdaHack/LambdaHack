module FOV (FovMode(..), fullscan) where

import qualified Data.Set as S
import qualified Data.List as L

import FOV.Common
import qualified FOV.Digital
import qualified FOV.Permissive
import qualified FOV.Shadow
import Geometry
import Level

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
data FovMode = Shadow | Permissive Int | Digital Int | Blind

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use is set in the config file.
-- Press a command key in the game to cycle among the algorithms
-- and see a special visualization of their effects..
fullscan :: FovMode -> Loc -> Level -> S.Set Loc
fullscan fovMode loc lvl =
  case fovMode of
    Shadow ->         -- shadow casting with infinite range
      S.unions $
      L.map (\ tr -> FOV.Shadow.scan (tr loc) lvl 1 (0,1))
        [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Permissive r  ->  -- permissive with range r
      S.unions $
      L.map (\ tr ->
              FOV.Permissive.scan r (tr loc) lvl 1
                (((B(1, 0), B(0, r+1)), [B(0, 1)]),
                 ((B(0, 1), B(r+1, 0)), [B(1, 0)])))
      [qtr0,qtr1,qtr2,qtr3]
    Digital r    ->  -- digital with range r
      S.unions $
      L.map (\ tr ->
              FOV.Digital.scan r (tr loc) lvl 1
                (((B(0, 1), B(r, -r)),  [B(0, 0)]),
                 ((B(0, 0), B(r, r+1)), [B(0, 1)])))
      [qtr0,qtr1,qtr2,qtr3]
    Blind        ->  -- only feeling out adjacent tiles by touch
      S.empty

-- | The translation, rotation and symmetry functions for octants.
tr0, tr1, tr2, tr3, tr4, tr5, tr6, tr7 :: Loc -> (Distance, Progress) -> Loc
tr0 loc (d, p) = trLoc loc (  p,   d)
tr1 loc (d, p) = trLoc loc (- p,   d)
tr2 loc (d, p) = trLoc loc (  p, - d)
tr3 loc (d, p) = trLoc loc (- p, - d)
tr4 loc (d, p) = trLoc loc (  d,   p)
tr5 loc (d, p) = trLoc loc (- d,   p)
tr6 loc (d, p) = trLoc loc (  d, - p)
tr7 loc (d, p) = trLoc loc (- d, - p)

-- | The translation and rotation functions for quadrants.
qtr0, qtr1, qtr2, qtr3 :: Loc -> Bump -> Loc
qtr0 loc (B(y, x)) = trLoc loc (  x, - y)  -- quadrant I
qtr1 loc (B(y, x)) = trLoc loc (  y,   x)  -- II (we rotate counter-clockwise)
qtr2 loc (B(y, x)) = trLoc loc (- x,   y)  -- III
qtr3 loc (B(y, x)) = trLoc loc (- y, - x)  -- IV
