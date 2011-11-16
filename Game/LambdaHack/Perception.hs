module Game.LambdaHack.Perception where

import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad

import Game.LambdaHack.Loc
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.FOV
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.TileKind

data Perception = Perception
  { preachable :: IS.IntSet
  , pvisible :: IS.IntSet
  }

-- The pplayer field is void if player not on the current level,
-- or if the player controls a blind monster. Right now, the field is used only
-- for player-controlled monsters on the current level.
data Perceptions = Perceptions
  { pplayer :: Maybe Perception
  , pheroes :: IM.IntMap Perception
  , ptotal  :: Perception
  }

ptreachable, ptvisible :: Perceptions -> IS.IntSet
ptreachable = preachable . ptotal
ptvisible   = pvisible . ptotal

actorPrLoc :: (Perception -> IS.IntSet) ->
              ActorId -> Loc -> Perceptions -> Maybe ActorId -> Bool
actorPrLoc projection actor loc per pl =
  let tryHero = case actor of
                  AMonster _ -> Nothing
                  AHero i -> do
                    hper <- IM.lookup i (pheroes per)
                    return $ loc `IS.member` projection hper
      tryPl   = do  -- the case for a monster under player control
                  guard $ Just actor == pl
                  pper <- pplayer per
                  return $ loc `IS.member` projection pper
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

perception_ :: Kind.COps -> State -> Perceptions
perception_ Kind.COps{cotile, coactor=Kind.Ops{okind}}
            state@(State { splayer = pl
                         , sconfig  = config
                         , ssensory = sensory }) =
  let lvl@Level{lheroes = hs} = slevel state
      mode   = Config.get config "engine" "fovMode"
      radius = let r = Config.get config "engine" "fovRadius"
               in if r < 1
                  then error $ "FOV radius is " ++ show r ++ ", should be >= 1"
                  else r
      fovMode m = if not $ asight $ okind $ bkind m
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
                  in Just $ perception (fovMode m) (bloc m) cotile lvl
             else Nothing
      pers = IM.map (\ h -> perception (fovMode h) (bloc h) cotile lvl) hs
      lpers = maybeToList pper ++ IM.elems pers
      reachable = IS.unions (L.map preachable lpers)
      visible = IS.unions (L.map pvisible lpers)
  in  Perceptions { pplayer = pper,
                    pheroes = pers,
                    ptotal = Perception reachable visible }

-- | Once we compute the reachable fields using FOV, it is possible
-- to compute what the hero can actually see.
perception :: FovMode -> Loc -> Kind.Ops TileKind -> Level -> Perception
perception fovMode ploc cops lvl@Level{lxsize, lysize} =
  let
    -- Reachable are all fields on an unblocked path from the hero position.
    -- The player position is visible, but not reachable (e.g. for targeting).
    reachable  = IS.fromList $ fullscan fovMode ploc cops lvl
    -- Everybody can see locations that are lit and are reachable.
    uniVisible = IS.filter (\ loc -> Tile.isLit cops (lvl `at` loc)) reachable
    -- The hero is assumed to carry a light source, too.
    litVisible = IS.insert ploc uniVisible
    -- Reachable fields adjacent to lit fields are visible, too.
    adjVisible =
      IS.filter
        (L.any (`IS.member` litVisible) . surroundings lxsize lysize)
        reachable
    -- Visible fields are either lit or adjacent to lit.
    visible = IS.union litVisible adjVisible
  in Perception reachable visible
