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
    actVisible = S.filter (\ loc -> light (lmap `at` loc)) reachable `S.union` S.singleton ploc
    pasVisible = S.filter (\ loc -> let p = passive (lmap `at` loc)
                                    in  any (\ d -> S.member (shift loc d) actVisible) p)
                          reachable
    dirVisible = S.filter (\ loc -> let p = perceptible (lmap `at` loc) :: [Dir]
                                    in  any (\ d -> shift loc d == ploc) p)
                          (S.fromList $ surroundings ploc)
    visible = S.unions [pasVisible, actVisible, dirVisible]
  in
    Perception reachable visible

