module Perception where

import Data.Set as S

import Geometry
import State
import Level
import Monster
import FOV

data Perception =
  Perception { preachable :: Set Loc, pvisible :: Set Loc }

perception_ :: State -> Perception
perception_ (State { splayer = Monster { mloc = ploc }, slevel = Level { lmap = lmap } }) =
  perception ploc lmap

perception :: Loc -> LMap -> Perception
perception ploc lmap =
  let
    -- This part is simple. "reachable" contains everything that is on an
    -- unblocked path from the player position.
    reachable  = fullscan Nothing ploc lmap
    -- In "actVisible", we store the locations that have light and are
    -- reachable. Furthermore, the player location itself is always
    -- visible.
    actVisible = S.filter (\ loc -> light (lmap `at` loc)) reachable `S.union` S.singleton ploc
    srnd       = S.fromList $ surroundings ploc
    -- In "dirVisible", we store locations in the surroundings that are
    -- perceptible from the current position.
    dirVisible = S.filter (\ loc -> let p = perceptible (lmap `at` loc) :: [Dir]
                                    in  any (\ d -> shift loc d == ploc) p)
                          (S.fromList $ surroundings ploc)
    ownVisible = S.union actVisible dirVisible
    -- Something is "pasVisible" if it is reachable passively visible from an
    -- "actVisible" location, *or* if it is in the surroundings and passively
    -- visible from a "dirVisible" location. (This is complicated, and I'd
    -- like to simplify it, but for now, it seems to at least do what I
    -- want.)
    pasVisible = S.filter (\ loc -> let p = passive (lmap `at` loc)
                                        dp = S.member loc srnd
                                        s = if dp then ownVisible else actVisible
                                    in  any (\ d -> S.member (shift loc d) s) p)
                          reachable
    visible = S.unions [pasVisible, actVisible, dirVisible]
  in
    Perception reachable visible
