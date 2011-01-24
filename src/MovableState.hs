module MovableState where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe

import Geometry
import Movable
import Level
import State
import Perception

-- The operations with "Any", and those that use them, consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Error if not found.
findActorAnyLevel :: Actor -> State -> Maybe (LevelName, Movable)
findActorAnyLevel actor state@(State { slevel   = lvl,
                                       sdungeon = Dungeon m }) =
  let chk lvl =
        fmap (\ m -> (lname lvl, m)) $
        case actor of
          AHero n    -> IM.lookup n (lheroes lvl)
          AMonster n -> IM.lookup n (lmonsters lvl)
  in  listToMaybe $ mapMaybe chk (lvl : M.elems m)

getPlayerBody :: State -> Movable
getPlayerBody state = snd $ fromMaybe (error "getPlayerBody") $
                      findActorAnyLevel (splayer state) state

-- | The list of actors and levels for all heroes in the dungeon.
-- Heroes from the current level go first.
allHeroesAnyLevel :: State -> [(Actor, LevelName)]
allHeroesAnyLevel state =
  let Dungeon m = sdungeon state
      one (Level { lname = ln, lheroes = hs }) =
        L.map (\ (i, _) -> (AHero i, ln)) (IM.assocs hs)
  in  L.concatMap one (slevel state : M.elems m)

updateAnyActorBody :: Actor -> (Movable -> Movable) ->  State -> State
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
targetToLoc :: State -> Perception -> Maybe Loc
targetToLoc state per =
  case mtarget (getPlayerBody state) of
    TLoc loc -> Just loc
    TCursor  ->
      if lname (slevel state) == clocLn (scursor state)
      then Just $ clocation (scursor state)
      else Nothing  -- cursor invalid: set at a different level
    TEnemy a -> do
      (ln, m) <- findActorAnyLevel a state  -- is target alive?
      guard $ ln == lname (slevel state)    -- is target on current level?
      let loc = mloc m
      guard $ S.member loc (pvisible per)   -- is target visible?
      return loc

-- The operations below disregard levels other than the current.

-- | Gets actor body from the current level. Error if not found.
getActor :: State -> Actor -> Movable
getActor (State { slevel = lvl }) a =
  case a of
    AHero n    -> lheroes   lvl IM.! n
    AMonster n -> lmonsters lvl IM.! n

-- | Removes the actor, if present, from the current level.
deleteActor :: Actor -> State -> State
deleteActor a =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.delete n))
    AMonster n -> updateLevel (updateMonsters (IM.delete n))

-- | Add actor to the current level.
insertActor :: Actor -> Movable -> State -> State
insertActor a m =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.insert n m))
    AMonster n -> updateLevel (updateMonsters (IM.insert n m))

levelHeroList :: State -> [Movable]
levelHeroList (State { slevel = Level { lheroes = hs } }) = IM.elems hs

levelMonsterList :: State -> [Movable]
levelMonsterList (State { slevel = Level { lmonsters = ms } }) = IM.elems ms

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: State -> Loc -> Maybe Actor
locToActor state loc =
  getIndex (lmonsters, AMonster) `mplus` getIndex (lheroes, AHero)
    where
      getIndex (projection, injection) =
        let l  = IM.assocs $ projection $ slevel state
            im = L.find (\ (_i, m) -> mloc m == loc) l
        in  fmap (injection . fst) im
