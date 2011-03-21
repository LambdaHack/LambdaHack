module Perception where

import qualified Data.Set as S
import Data.List as L
import qualified Data.IntMap as IM

import Geometry
import State
import Level
import Movable
import FOV
import qualified Config

data Perception =
  Perception { preachable :: S.Set Loc, pvisible :: S.Set Loc }

-- The pplayer field is void if player not on the current level,
-- or if the player controls a blind monster (TODO. But perhaps only non-blind
-- monsters should be controllable?).
data Perceptions =
  Perceptions { pplayer :: Maybe Perception,
                pheroes :: IM.IntMap Perception,
                ptotal  :: Perception }

ptreachable :: Perceptions -> S.Set Loc
ptreachable = preachable . ptotal

ptvisible :: Perceptions -> S.Set Loc
ptvisible = pvisible . ptotal

perception_ :: State -> Perceptions
perception_ state@(State { slevel   = Level { lmap = lmap, lheroes = hs },
                           sconfig  = config,
                           ssensory = sensory }) =
  let mode   = Config.get config "engine" "fovMode"
      radius = Config.get config "engine" "fovRadius"
      fovMode =
        -- terrible, temporary hack
        case sensory of
          Vision 3 -> Digital radius
          Vision 2 -> Permissive radius
          Vision 1 -> Shadow
          _        ->
            -- this is not a hack
            case mode of
              "permissive" -> Permissive radius
              "digital"    -> Digital radius
              "shadow"     -> Shadow
              _            -> error $ "perception_: unknown mode: " ++ show mode

      pers = IM.map (\ pl -> perception fovMode (mloc pl) lmap) hs
      lpers = IM.elems pers
      pper = Nothing  -- TODO, and add it to ptotal, in case it's monster
      reachable = S.unions (L.map preachable lpers)
      visible = S.unions (L.map pvisible lpers)
  in  Perceptions { pplayer = pper,
                    pheroes = pers,
                    ptotal = Perception reachable visible }

perception :: FovMode -> Loc -> LMap -> Perception
perception fovMode ploc lmap =
  let
    -- This part is simple. "reachable" contains everything that is on an
    -- unblocked path from the hero position.
    reachable  = fullscan fovMode ploc lmap
    -- In "actVisible", we store the locations that have light and are
    -- reachable. Furthermore, the hero location itself is always visible.
    litVisible = S.filter (\ loc -> light (lmap `at` loc)) reachable
    actVisible = S.insert ploc litVisible
    srnd       = S.fromList $ surroundings ploc
    -- In "dirVisible", we store locations in the surroundings that are
    -- perceptible from the current position.
    dirVisible = S.filter (\ loc -> let p = perceptible (lmap `at` loc) :: [Dir]
                                    in  any (\ d -> shift loc d == ploc) p)
                          srnd
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
    -- A simpler way to make walls of lit rooms visible, at the cost of making
    -- them reflect light from all sides, also from corridors.
    -- Can be hacked around by checking for corridors in the condition below.
    -- The version in the comment assumes hero light has diameter 3, not 1,
    -- which looks a bit differently in dark rooms, revealing more walls.
    openSurroundings = S.filter (\ loc -> open (lmap `at` loc)) srnd
    openVisible = S.union actVisible openSurroundings
    simpleVisible =
      S.filter
        (\ loc -> S.member loc openVisible
                  || (reflects (lmap `at` loc)
                      && L.any
                           (\ l -> S.member l actVisible{-openVisible-})
                           (surroundings loc))
        ) (S.insert ploc reachable)
  in
    case fovMode of
      Shadow -> Perception reachable visible
      _      -> Perception reachable simpleVisible
