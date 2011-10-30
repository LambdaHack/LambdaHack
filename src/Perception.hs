module Perception where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad

import Geometry
import State
import Level
import Actor
import ActorState
import Content.ActorKind
import FOV
import qualified Config
import qualified Tile
import qualified Kind

data Perception = Perception
  { preachable :: S.Set Loc
  , pvisible :: S.Set Loc
  }

-- The pplayer field is void if player not on the current level,
-- or if the player controls a blind monster. Right now, the field is used only
-- for player-controlled monsters on the current level.
data Perceptions = Perceptions
  { pplayer :: Maybe Perception
  , pheroes :: IM.IntMap Perception
  , ptotal  :: Perception
  }

ptreachable, ptvisible :: Perceptions -> S.Set Loc
ptreachable = preachable . ptotal
ptvisible   = pvisible . ptotal

actorPrLoc :: (Perception -> S.Set Loc) ->
              ActorId -> Loc -> Perceptions -> Maybe ActorId -> Bool
actorPrLoc projection actor loc per pl =
  let tryHero = case actor of
                  AMonster _ -> Nothing
                  AHero i -> do
                    hper <- IM.lookup i (pheroes per)
                    return $ loc `S.member` projection hper
      tryPl   = do  -- the case for a monster under player control
                  guard $ Just actor == pl
                  pper <- pplayer per
                  return $ loc `S.member` projection pper
      tryAny  = tryHero `mplus` tryPl
  in  fromMaybe False tryAny  -- assume not visible, if no perception found

actorSeesLoc    :: ActorId -> Loc -> Perceptions -> Maybe ActorId -> Bool
actorSeesLoc    = actorPrLoc pvisible

actorReachesLoc :: ActorId -> Loc -> Perceptions -> Maybe ActorId -> Bool
actorReachesLoc = actorPrLoc preachable

-- Not quite correct if FOV not symmetric (Shadow).
actorReachesActor :: ActorId -> ActorId -> Loc -> Loc
                     -> Perceptions -> Maybe ActorId
                     -> Bool
actorReachesActor actor1 actor2 loc1 loc2 per pl =
  actorReachesLoc actor1 loc2 per pl ||
  actorReachesLoc actor2 loc1 per pl

perception_ :: State -> Perceptions
perception_ state@(State { splayer = pl,
                           slevel   = lvl@Level{lheroes = hs},
                           sconfig  = config,
                           ssensory = sensory }) =
  let mode   = Config.get config "engine" "fovMode"
      radius = let r = Config.get config "engine" "fovRadius"
               in if r < 1
                  then error $ "FOV radius is " ++ show r ++ ", should be >= 1"
                  else r
      fovMode m = if not $ bsight $ Kind.getKind $ akind m
                  then Blind
                  else
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
              _            -> error $ "Unknown FOV mode: " ++ show mode

      -- Perception for a player-controlled monster on the current level.
      pper = if isAMonster pl && memActor pl state
             then let m = getPlayerBody state
                  in Just $ perception (fovMode m) (aloc m) lvl
             else Nothing
      pers = IM.map (\ h -> perception (fovMode h) (aloc h) lvl) hs
      lpers = maybeToList pper ++ IM.elems pers
      reachable = S.unions (L.map preachable lpers)
      visible = S.unions (L.map pvisible lpers)
  in  Perceptions { pplayer = pper,
                    pheroes = pers,
                    ptotal = Perception reachable visible }

-- | Once we compute the reachable fields using FOV, it is possible
-- to compute what the hero can actually see.
perception :: FovMode -> Loc -> Level -> Perception
perception fovMode ploc lvl =
  let
    -- Reachable are all fields on an unblocked path from the hero position.
    reachable  = fullscan fovMode ploc lvl
    -- Everybody can see locations that are lit and are reachable.
    uniVisible = S.filter (\ loc -> Tile.isLit (lvl `at` loc)) reachable
    -- The hero is assumed to carry a light source, too.
    litVisible = S.insert ploc uniVisible
    -- Reachable fields adjacent to lit fields are visible, too.
    adjVisible =
      S.filter (L.any (`S.member` litVisible) . surroundings) reachable
    -- Visible fields are either lit or adjacent to lit.
    visible = S.union litVisible adjVisible
  in Perception reachable visible
