module ActorState where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe
import Control.Exception (assert)

import Geometry
import Actor
import Level
import Dungeon
import State

-- The operations with "Any", and those that use them, consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Error if not found.
findActorAnyLevel :: ActorId -> State -> Maybe (LevelName, Actor)
findActorAnyLevel actor state@(State { slevel   = lvl,
                                       sdungeon = Dungeon m }) =
  let chk lvl =
        fmap (\ m -> (lname lvl, m)) $
        case actor of
          AHero n    -> IM.lookup n (lheroes lvl)
          AMonster n -> IM.lookup n (lmonsters lvl)
  in  listToMaybe $ mapMaybe chk (lvl : M.elems m)

getPlayerBody :: State -> Actor
getPlayerBody state = snd $ fromMaybe (error "getPlayerBody") $
                      findActorAnyLevel (splayer state) state

-- | The list of actors and levels for all heroes in the dungeon.
-- Heroes from the current level go first.
allHeroesAnyLevel :: State -> [(ActorId, LevelName)]
allHeroesAnyLevel state =
  let Dungeon m = sdungeon state
      one (Level { lname = ln, lheroes = hs }) =
        L.map (\ (i, _) -> (AHero i, ln)) (IM.assocs hs)
  in  L.concatMap one (slevel state : M.elems m)

updateAnyActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateAnyActorBody actor f state =
  case findActorAnyLevel actor state of
    Just (ln, _) ->
      case actor of
        AHero n    -> updateAnyLevel (updateHeroes   $ IM.adjust f n) ln state
        AMonster n -> updateAnyLevel (updateMonsters $ IM.adjust f n) ln state
    Nothing -> error "updateAnyActorBody"

updateAnyLevel :: (Level -> Level) -> LevelName -> State -> State
updateAnyLevel f ln state@(State { slevel = level,
                                   sdungeon = Dungeon dng })
  | ln == lname level = updateLevel f state
  | otherwise = updateDungeon (const $ Dungeon $ M.adjust f ln dng) state

-- | Calculate the location of player's target.
targetToLoc :: S.Set Loc -> State -> Maybe Loc
targetToLoc visible state =
  case atarget (getPlayerBody state) of
    TLoc loc -> Just loc
    TCursor  ->
      if lname (slevel state) == clocLn (scursor state)
      then Just $ clocation (scursor state)
      else Nothing  -- cursor invalid: set at a different level
    TEnemy a _ll -> do
      guard $ memActor a state           -- alive and on the current level?
      let loc = aloc (getActor a state)
      guard $ S.member loc visible       -- visible?
      return loc

-- The operations below disregard levels other than the current.

-- | Checks if the actor is present on the current level.
memActor :: ActorId -> State -> Bool
memActor a (State { slevel = lvl }) =
  case a of
    AHero n    -> IM.member n (lheroes lvl)
    AMonster n -> IM.member n (lmonsters lvl)

-- | Gets actor body from the current level. Error if not found.
getActor :: ActorId -> State -> Actor
getActor a (State { slevel = lvl }) =
  case a of
    AHero n    -> lheroes   lvl IM.! n
    AMonster n -> lmonsters lvl IM.! n

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.delete n))
    AMonster n -> updateLevel (updateMonsters (IM.delete n))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.insert n m))
    AMonster n -> updateLevel (updateMonsters (IM.insert n m))

levelHeroList, levelMonsterList :: State -> [Actor]
levelHeroList    (State { slevel = Level { lheroes   = hs } }) = IM.elems hs
levelMonsterList (State { slevel = Level { lmonsters = ms } }) = IM.elems ms

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: Loc -> State -> Maybe ActorId
locToActor loc state =
  let l = locToActors loc state
  in assert (L.length l <= 1) $
       listToMaybe l

locToActors :: Loc -> State -> [ActorId]
locToActors loc state =
  getIndex (lmonsters, AMonster) ++ getIndex (lheroes, AHero)
    where
      getIndex (projection, injection) =
        let l  = IM.assocs $ projection $ slevel state
            im = L.filter (\ (_i, m) -> aloc m == loc) l
        in  fmap (injection . fst) im
