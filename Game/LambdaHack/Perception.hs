{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Perception, levelPerception, totalVisible, actorSeesLoc
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.FOV
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import qualified Game.LambdaHack.Tile as Tile

newtype PerceptionReachable = PerceptionReachable
  { preachable :: IS.IntSet }

-- TOOD: if really needed, optimize by representing as a set of intervals.
newtype PerceptionVisible = PerceptionVisible
  { pvisible :: IS.IntSet }

-- | The type representing the perception of a faction on a level.
-- The total visibility holds the sum of FOVs of all actors
-- of a given faction on the level and servers only as a speedup.
data Perception = Perception
  { pactors :: IM.IntMap PerceptionVisible  -- ^ per actor
  , ptotal  :: PerceptionVisible            -- ^ sum for all actors
  }

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> IS.IntSet
totalVisible = pvisible . ptotal

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per aid pos = pos `IS.member` pvisible (pactors per IM.! aid)

-- | Calculate perception of the level.
levelPerception :: Kind.COps -> Config -> Maybe FovMode -> FactionId -> Level
                -> Perception
levelPerception cops@Kind.COps{cotile} Config{configFovMode} stryFov
                fid lvl@Level{lactor} =
  let hs = IM.filter (\m -> bfaction m == fid && not (bproj m)) lactor
      reas =
        IM.map (\h -> computeReachable cops configFovMode stryFov h lvl) hs
      lreas = map preachable $ IM.elems reas
      totalRea = PerceptionReachable $ IS.unions lreas
      -- TODO: Instead of giving the monster a light source, alter vision.
      lights = IS.fromList $ map bpos $ IM.elems hs
      totalVis = computeVisible cotile totalRea lvl lights
      f = PerceptionVisible . IS.intersection (pvisible totalVis) . preachable
  in Perception { pactors = IM.map f reas
                , ptotal  = totalVis }

-- | A position can be directly lit by an ambient shine or a weak, portable
-- light source, e.g,, carried by a hero. (Only lights of radius 0
-- are considered for now and it's assumed they do not reveal hero's position.
-- TODO: change this to be radius 1 noctovision and introduce stronger
-- light sources that show more but make the hero visible.)
-- A position is visible if it's reachable and either directly lit
-- or adjacent to one that is at once directly lit and reachable.
-- The last condition approximates being
-- on the same side of obstacles as the light source.
-- The approximation is not exact for multiple heroes, but the discrepancy
-- can be attributed to deduction based on combined vague visual hints,
-- e.g., if I don't see the reachable light seen by another hero,
-- there must be a wall in-between. Stray rays indicate doors,
-- moving shadows indicate monsters, etc.
computeVisible :: Kind.Ops TileKind -> PerceptionReachable
               -> Level -> IS.IntSet -> PerceptionVisible
computeVisible cops reachable@PerceptionReachable{preachable} lvl lights =
  let isV = isVisible cops reachable lvl lights
  in PerceptionVisible $ IS.filter isV preachable

-- TODO: this is calculated per-faction, not per-actor, but still optimize,
-- e.g., by partitioning preachable wrt litDirectly and then running
-- isVisible only over one of the parts, depending on which is smaller.
isVisible :: Kind.Ops TileKind -> PerceptionReachable
          -> Level -> IS.IntSet -> Point -> Bool
isVisible cotile PerceptionReachable{preachable}
          lvl@Level{lxsize, lysize} lights pos0 =
  let litDirectly loc = Tile.isLit cotile (lvl `at` loc)
                        || loc `IS.member` lights
      l_and_R loc = litDirectly loc && loc `IS.member` preachable
  in litDirectly pos0 || L.any l_and_R (vicinity lxsize lysize pos0)

-- | Reachable are all fields on an unblocked path from the hero position.
-- Actor's own position is considred reachable by him.
computeReachable :: Kind.COps -> FovMode -> Maybe FovMode
                 -> Actor -> Level -> PerceptionReachable
computeReachable Kind.COps{cotile, coactor=Kind.Ops{okind}}
                 configFovMode smarkVision actor lvl =
  let fovMode m =
        if not $ asight $ okind $ bkind m
        then Blind
        else case smarkVision of
          Just fm -> fm
          Nothing -> configFovMode
      ppos = bpos actor
  in PerceptionReachable $
       IS.insert ppos $ IS.fromList $ fullscan cotile (fovMode actor) ppos lvl
