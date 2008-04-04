module Perception where

import Data.Set as S

import Geometry
import State
import Level
import Monster
import FOV

data Perception =
  Perception { preachable :: Set Loc, pvisible :: Set Loc }

perception_ :: State -> Level -> Perception
perception_ (State { splayer = Monster { mloc = ploc } }) (Level { lmap = lmap }) =
  perception ploc lmap

perception :: Loc -> LMap -> Perception
perception ploc lmap =
  let
    reachable  = fullscan ploc lmap
    actVisible = S.filter (\ loc -> light (lmap `at` loc)) reachable
    pasVisible = S.filter (\ loc -> let (x,p) = passive (lmap `at` loc)
                                    in  any (\ d -> S.member (shift loc d) actVisible) p ||
                                        (not x && adjacent loc ploc))
                                    -- the above "not x" prevents walls from
                                    -- being visible from the outside when
                                    -- adjacent
                          reachable
    visible = S.union pasVisible actVisible
  in
    Perception reachable visible

