module ActorState where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe

import Utils.Assert
import Loc
import Actor
import Level
import Dungeon
import State
import WorldLoc

-- The operations with "Any", and those that use them, consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Fails if not found.
findActorAnyLevel :: ActorId -> State -> (LevelId, Actor)
findActorAnyLevel actor state@State{slid, sdungeon} =
  assert (not (absentHero actor state) `blame` actor) $
  let chk (ln, lvl) =
        fmap (\ m -> (ln, m)) $
        case actor of
          AHero n    -> IM.lookup n (lheroes lvl)
          AMonster n -> IM.lookup n (lmonsters lvl)
  in case mapMaybe chk (currentFirst slid sdungeon) of
    []    -> assert `failure` actor
    res:_ -> res  -- checking if res is unique would break laziness

-- | Checks whether an actor is a hero, but not a member of the party.
absentHero :: ActorId -> State -> Bool
absentHero a State{sparty} =
  case a of
    AHero n    -> IS.notMember n sparty
    AMonster _ -> False

getPlayerBody :: State -> Actor
getPlayerBody state =
  let pl = splayer state
  in snd $ findActorAnyLevel pl state

-- | The list of actors and levels for all heroes in the dungeon.
allHeroesAnyLevel :: State -> [(ActorId, LevelId)]
allHeroesAnyLevel State{slid, sdungeon} =
  let one (ln, Level{lheroes}) =
        L.map (\ (i, _) -> (AHero i, ln)) (IM.assocs lheroes)
  in L.concatMap one (currentFirst slid sdungeon)

updateAnyActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateAnyActorBody actor f state =
  let (ln, _) = findActorAnyLevel actor state
  in case actor of
       AHero n    -> updateAnyLevel (updateHeroes   $ IM.adjust f n) ln state
       AMonster n -> updateAnyLevel (updateMonsters $ IM.adjust f n) ln state

updateAnyLevel :: (Level -> Level) -> LevelId -> State -> State
updateAnyLevel f ln state@State{slid, sdungeon}
  | ln == slid = updateLevel f state
  | otherwise = updateDungeon (const $ adjust f ln sdungeon) state

-- | Calculate the location of player's target.
targetToLoc :: S.Set Loc -> State -> Maybe Loc
targetToLoc visible state =
  case atarget (getPlayerBody state) of
    TLoc loc -> Just loc
    TCursor  ->
      if slid state == clocLn (scursor state)
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
memActor a state =
  case a of
    AHero n    -> IM.member n (lheroes (slevel state))
    AMonster n -> IM.member n (lmonsters (slevel state))

-- | Gets actor body from the current level. Error if not found.
getActor :: ActorId -> State -> Actor
getActor a state =
  case a of
    AHero n    -> lheroes   (slevel state) IM.! n
    AMonster n -> lmonsters (slevel state) IM.! n

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

-- | Removes a player from the current level and party list.
deletePlayer :: State -> State
deletePlayer state@State{splayer, sparty} =
  let s = deleteActor splayer state
  in case splayer of
    AHero n    -> s{sparty = IS.delete n sparty}
    AMonster _ -> s

levelHeroList, levelMonsterList :: State -> [Actor]
levelHeroList    state = IM.elems $ lheroes   $ slevel state
levelMonsterList state = IM.elems $ lmonsters $ slevel state

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: Loc -> State -> Maybe ActorId
locToActor loc state =
  let l = locToActors loc state
  in assert (L.length l <= 1 `blame` l) $
     listToMaybe l

locToActors :: Loc -> State -> [ActorId]
locToActors loc state =
  getIndex (lmonsters, AMonster) ++ getIndex (lheroes, AHero)
    where
      getIndex (projection, injection) =
        let l  = IM.assocs $ projection $ slevel state
            im = L.filter (\ (_i, m) -> aloc m == loc) l
        in  fmap (injection . fst) im
