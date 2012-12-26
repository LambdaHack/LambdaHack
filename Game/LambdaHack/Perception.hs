{-# LANGUAGE OverloadedStrings #-}
-- | Actors perceiving other actors and the dungeon level.
module Game.LambdaHack.Perception
  ( Pers, Perception
  , totalVisible, debugTotalReachable, dungeonPerception
  , actorReachesLoc, actorReachesActor, actorSeesActor
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Maybe

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Dungeon
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

-- | The type representing the perception for all levels in the dungeon.
type Pers = [(LevelId, Perception)]

-- | The type representing the perception of all actors on the level.
-- Note: Heroes share visibility and only have separate reachability.
data Perception = Perception
  { pheroes :: IM.IntMap PerceptionReachable
  , ptotal  :: PerceptionVisible
  }

-- | The set of tiles visible by at least one hero.
totalVisible :: Perception -> IS.IntSet
totalVisible = pvisible . ptotal

-- | For debug only: the set of tiles reachable
-- (would be visible if lit) by at least one hero.
debugTotalReachable :: Perception -> IS.IntSet
debugTotalReachable per =
  let lpers = IM.elems (pheroes per)
  in IS.unions (map preachable lpers)

-- | Check whether a location is within the visually reachable area
-- of the given actor (disregarding lighting).
actorReachesLoc :: ActorId -> Point -> Perception -> Bool
actorReachesLoc actor loc per =
  let tryHero = do
        hper <- IM.lookup actor (pheroes per)
        return $ loc `IS.member` preachable hper
  in fromMaybe False tryHero  -- assume not visible, if no perception found

-- | Check whether an actor is within the visually reachable area
-- of the given actor (disregarding lighting).
actorReachesActor :: ActorId -> ActorId -> Point -> Point -> Perception
                  -> Bool
actorReachesActor actor1 actor2 loc1 loc2 per =
  actorReachesLoc actor1 loc2 per ||
  actorReachesLoc actor2 loc1 per

-- TODO: When the code for throwing, digital lines and lights is complete.
-- make this a special case of ActorSeesActor.
-- | Whether a monster can see a hero (@False@ if the target has
-- no perceptions, e.g., not a hero or a hero without perception, due
-- to being spawned on the same turn by a monster and then seen by another).
-- An approximation, to avoid computing FOV for the monster.
monsterSeesHero :: Kind.Ops TileKind -> Perception -> Level
                 -> ActorId -> ActorId -> Point -> Point -> Bool
monsterSeesHero cotile per lvl _source target sloc tloc =
  let rempty = PerceptionReachable IS.empty
      reachable@PerceptionReachable{preachable} =
        fromMaybe rempty $ IM.lookup target $ pheroes per
  in sloc `IS.member` preachable
     && isVisible cotile reachable lvl IS.empty tloc

-- | Whether an actor can see another. An approximation.
actorSeesActor :: Kind.Ops TileKind -> Perception -> Level
               -> ActorId -> ActorId -> Point -> Point -> Bool
actorSeesActor cotile per lvl source target sloc tloc =
  let heroReaches = actorReachesLoc source tloc per
      visByHeroes = tloc `IS.member` totalVisible per
      monsterSees = monsterSeesHero cotile per lvl source target sloc tloc
  in  heroReaches && visByHeroes || monsterSees

-- | Calculate the perception of all actors on the level.
dungeonPerception :: Kind.COps -> State -> Pers
dungeonPerception cops s@State{slid, sdungeon} =
  let lvlPer (ln, lvl) = (ln, levelPerception cops s lvl)
  in map lvlPer $ currentFirst slid sdungeon

-- | Calculate the perception of all actors on the level.
levelPerception :: Kind.COps -> State -> Level -> Perception
levelPerception cops@Kind.COps{cotile}
                State{ sconfig
                     , sfaction
                     , sdebug = DebugMode{smarkVision}
                     }
                lvl@Level{lactor} =
  let Config{configFovMode} = sconfig
      hs = IM.filter (\ m -> bfaction m == sfaction && not (bproj m)) lactor
      pers = IM.map (\ h ->
                      computeReachable cops configFovMode smarkVision h lvl) hs
      locs = map bloc $ IM.elems hs
      lpers = IM.elems pers
      reachable = PerceptionReachable $ IS.unions (map preachable lpers)
      -- TODO: Instead of giving the monster a light source, alter vision.
      lights = IS.fromList locs
      visible = computeVisible cotile reachable lvl lights
  in Perception { pheroes = pers
                , ptotal  = visible
                }

-- | A location can be directly lit by an ambient shine or a weak, portable
-- light source, e.g,, carried by a hero. (Only lights of radius 0
-- are considered for now and it's assumed they do not reveal hero's position.
-- TODO: change this to be radius 1 noctovision and introduce stronger
-- light sources that show more but make the hero visible.)
-- A location is visible if it's reachable and either directly lit
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
               lvl@Level{lxsize, lysize} lights loc0 =
  let litDirectly loc = Tile.isLit cotile (lvl `at` loc)
                        || loc `IS.member` lights
      l_and_R loc = litDirectly loc && loc `IS.member` preachable
  in litDirectly loc0 || L.any l_and_R (vicinity lxsize lysize loc0)

-- | Reachable are all fields on an unblocked path from the hero position.
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
      ploc = bloc actor
  in PerceptionReachable $
       IS.insert ploc $ IS.fromList $ fullscan cotile (fovMode actor) ploc lvl
