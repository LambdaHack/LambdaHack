{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Pers, Perception
  , totalVisible, debugTotalReachable, dungeonPerception
  , actorReachesLoc, actorSeesLoc
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.FOV
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile

newtype PerceptionReachable = PerceptionReachable
  { preachable :: IS.IntSet
  }

newtype PerceptionVisible = PerceptionVisible
  { pvisible :: IS.IntSet
  }

-- | Perception indexed by faction identifier.
type Pers = IM.IntMap FactionPerception

-- | Perception of a single faction, indexed by level identifier.
type FactionPerception = M.Map LevelId Perception

-- | The type representing the perception of a faction on a level.
-- Actors of the same faction share visibility and only have separate
-- reachability.
data Perception = Perception
  { pactors :: IM.IntMap PerceptionReachable  -- ^ per actor
  , ptotal  :: PerceptionVisible              -- ^ sum for all actors
  }

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> IS.IntSet
totalVisible = pvisible . ptotal

-- | For debug only: the set of tiles reachable
-- (would be visible if lit) by at least one hero.
debugTotalReachable :: Perception -> IS.IntSet
debugTotalReachable per =
  let lpers = IM.elems $ pactors per
  in IS.unions (map preachable lpers)

-- | Check whether a position is within the visually reachable area
-- of the given actor (disregarding lighting).
actorReachesLoc :: ActorId -> Point -> Perception -> Bool
actorReachesLoc actor loc per =
  let tryHero = do
        hper <- IM.lookup actor $ pactors per
        return $ loc `IS.member` preachable hper
  in fromMaybe False tryHero  -- assume not visible, if no perception found

-- | Whether an actor can see a position.
actorSeesLoc :: Perception -> ActorId -> Point -> Bool
actorSeesLoc per source tpos =
  let reachable = actorReachesLoc source tpos per
      visible = tpos `IS.member` totalVisible per
  in reachable && visible

-- | Calculate the perception of all actors on the level.
dungeonPerception :: Kind.COps -> Config -> DebugModeSer -> State -> Pers
dungeonPerception cops sconfig sdebug s =
  let f fid _ = factionPerception cops sconfig sdebug s fid
  in IM.mapWithKey f $ sfaction s

-- | Calculate perception of the faction.
factionPerception :: Kind.COps -> Config -> DebugModeSer -> State -> FactionId
                  -> FactionPerception
factionPerception cops sconfig sdebug s fid =
  M.map (levelPerception cops sconfig sdebug fid) $ sdungeon s

-- | Calculate perception of the level.
levelPerception :: Kind.COps -> Config -> DebugModeSer -> FactionId -> Level -> Perception
levelPerception cops@Kind.COps{cotile} sconfig DebugModeSer{stryFov} fid
                lvl@Level{lactor} =
  let Config{configFovMode} = sconfig
      hs = IM.filter (\ m -> bfaction m == fid && not (bproj m)) lactor
      pers = IM.map (\ h ->
                      computeReachable cops configFovMode stryFov h lvl) hs
      poss = map bpos $ IM.elems hs
      lpers = IM.elems pers
      reachable = PerceptionReachable $ IS.unions (map preachable lpers)
      -- TODO: Instead of giving the monster a light source, alter vision.
      lights = IS.fromList poss
      visible = computeVisible cotile reachable lvl lights
  in Perception { pactors = pers
                , ptotal  = visible
                }

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
computeVisible cops reachable@PerceptionReachable{preachable} lvl lights' =
  let lights = IS.intersection lights' preachable  -- optimization
      isV = isVisible cops reachable lvl lights
  in PerceptionVisible $ IS.filter isV preachable

isVisible :: Kind.Ops TileKind -> PerceptionReachable
          -> Level -> IS.IntSet -> Point -> Bool
isVisible cotile PerceptionReachable{preachable}
               lvl@Level{lxsize, lysize} lights pos0 =
  let litDirectly loc = Tile.isLit cotile (lvl `at` loc)
                        || loc `IS.member` lights
      l_and_R loc = litDirectly loc && loc `IS.member` preachable
  in litDirectly pos0 || L.any l_and_R (vicinity lxsize lysize pos0)

-- | Reachable are all fields on an unbposked path from the hero position.
-- The player's own position is considred reachable by him.
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
