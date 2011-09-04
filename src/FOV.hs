module FOV where

import Data.Set as S
import Data.List as L

import FOV.Common
import FOV.Digital
import FOV.Permissive
import FOV.Shadow
import Geometry
import Level

-- import Debug.Trace

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
data FovMode = Shadow | Permissive Int | Digital Int | Blind

-- Three Field of View algorithms. Press 'V' to cycle among them in the game.

-- The main FOV function.

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view.
fullscan :: FovMode -> Loc -> LMap -> Set Loc
fullscan fovMode loc lm =
  case fovMode of
    Shadow ->         -- shadow casting with infinite range
      S.unions $
      L.map (\ tr -> scan (tr loc) lm 1 (0,1))
        [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Permissive r  ->  -- permissive with range r
      S.unions $
      L.map (\ tr ->
              pscan r (tr loc) lm 1
                (((B(1, 0), B(0, r+1)), [B(0, 1)]),
                 ((B(0, 1), B(r+1, 0)), [B(1, 0)])))
      [qtr0,qtr1,qtr2,qtr3]
    Digital r    ->  -- digital with range r
      S.unions $
      L.map (\ tr ->
              dscan r (tr loc) lm 1
                (((B(0, 1), B(r, -r)),  [B(0, 0)]),
                 ((B(0, 0), B(r, r+1)), [B(0, 1)])))
      [qtr0,qtr1,qtr2,qtr3]
    Blind        ->  -- only feeling out adjacent tiles by touch
      S.empty
