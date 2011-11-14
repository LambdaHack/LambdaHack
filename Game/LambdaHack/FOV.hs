module Game.LambdaHack.FOV
  ( FovMode(..), fullscan
  ) where

import qualified Data.Set as S
import qualified Data.List as L

import Game.LambdaHack.FOV.Common
import qualified Game.LambdaHack.FOV.Digital as Digital
import qualified Game.LambdaHack.FOV.Permissive as Permissive
import qualified Game.LambdaHack.FOV.Shadow as Shadow
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Kind as Kind

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
data FovMode = Shadow | Permissive Int | Digital Int | Blind

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view. The Field of View
-- algorithm to use is set in the config file.
-- Press a command key in the game to cycle among the algorithms
-- and see a special visualization of their effects..
fullscan :: FovMode -> Loc -> Kind.COps -> Level -> S.Set Loc
fullscan fovMode loc scops lvl@Level{lxsize} =
  case fovMode of
    Shadow ->  -- shadow casting with infinite range
      S.unions $
      L.map (\ tr -> Shadow.scan tr scops lvl 1 (0,1))
        [tr0, tr1, tr2, tr3, tr4, tr5, tr6, tr7]
    Permissive r  ->  -- permissive with range r
      S.unions $
      L.map (\ tr -> Permissive.scan r tr scops lvl) [qtr0, qtr1, qtr2, qtr3]
    Digital r ->  -- digital with range r
      S.unions $
      L.map (\ tr -> Digital.scan r tr scops lvl) [qtr0, qtr1, qtr2, qtr3]
    Blind ->  -- only feeling out adjacent tiles by touch
      let radius = 1
      in S.unions $
         L.map (\ tr ->
                 Digital.scan radius tr scops lvl) [qtr0, qtr1, qtr2, qtr3]
 where
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
