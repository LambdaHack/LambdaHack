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
fullscan :: FovMode -> Loc -> LMap -> S.Set Loc
fullscan fovMode loc lm =
  case fovMode of
    Shadow ->         -- shadow casting with infinite range
      S.unions $
      L.map (\ tr -> FOV.Shadow.scan (tr loc) lm 1 (0,1))
        [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Permissive r  ->  -- permissive with range r
      S.unions $
      L.map (\ tr ->
              FOV.Permissive.scan r (tr loc) lm 1
                (((B(1, 0), B(0, r+1)), [B(0, 1)]),
                 ((B(0, 1), B(r+1, 0)), [B(1, 0)])))
      [qtr0,qtr1,qtr2,qtr3]
    Digital r    ->  -- digital with range r
      S.unions $
      L.map (\ tr ->
              FOV.Digital.scan r (tr loc) lm 1
                (((B(0, 1), B(r, -r)),  [B(0, 0)]),
                 ((B(0, 0), B(r, r+1)), [B(0, 1)])))
      [qtr0,qtr1,qtr2,qtr3]
    Blind        ->  -- only feeling out adjacent tiles by touch
      S.empty

-- | The translation, rotation and symmetry functions for octants.
tr0, tr1, tr2, tr3, tr4, tr5, tr6, tr7 :: Loc -> Loc -> Loc
tr0 (oy,ox) (d,p) = (oy + d,ox + p)
tr1 (oy,ox) (d,p) = (oy + d,ox - p)
tr2 (oy,ox) (d,p) = (oy - d,ox + p)
tr3 (oy,ox) (d,p) = (oy - d,ox - p)
tr4 (oy,ox) (d,p) = (oy + p,ox + d)
tr5 (oy,ox) (d,p) = (oy + p,ox - d)
tr6 (oy,ox) (d,p) = (oy - p,ox + d)
tr7 (oy,ox) (d,p) = (oy - p,ox - d)

-- | The translation and rotation functions for quadrants.
qtr0, qtr1, qtr2, qtr3 :: Loc -> Bump -> Loc
qtr0 (oy, ox) (B(y, x)) = (oy - y, ox + x)  -- quadrant I
qtr1 (oy, ox) (B(y, x)) = (oy + x, ox + y)  -- II (we rotate counter-clockwise)
qtr2 (oy, ox) (B(y, x)) = (oy + y, ox - x)  -- III
qtr3 (oy, ox) (B(y, x)) = (oy - x, ox - y)  -- IV
