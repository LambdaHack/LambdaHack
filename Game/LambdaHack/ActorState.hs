-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.ActorState where

import Control.Monad
import qualified Data.List as L
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Char as Char

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Point
import Game.LambdaHack.Actor
import Game.LambdaHack.Level
import Game.LambdaHack.Dungeon
import Game.LambdaHack.State
import Game.LambdaHack.Item
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F

-- TODO: currently it's false for player-controlled monsters.
-- When it's no longer, rewrite the places where it matters.
-- | Checks whether an actor identifier represents a hero.
isAHero :: State -> ActorId -> Bool
isAHero s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bparty actor == 0

-- The operations with "Any", and those that use them,
-- consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Fails if not found.
findActorAnyLevel :: ActorId -> State -> (LevelId, Actor, [Item])
findActorAnyLevel actor State{slid, sdungeon} =
  let chk (ln, lvl) =
        let (m, mi) = case actor of
              AHero n    -> (IM.lookup n (lheroes lvl),
                             IM.lookup n (lheroItem lvl))
              AMonster n -> (IM.lookup n (lmonsters lvl),
                             IM.lookup n (lmonItem lvl))
        in fmap (\ a -> (ln, a, fromMaybe [] mi)) m
  in case mapMaybe chk (currentFirst slid sdungeon) of
    []      -> assert `failure` actor
    res : _ -> res  -- checking if res is unique would break laziness

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (Int, Actor)
tryFindActor State{slid, sdungeon} p =
  let chk (_ln, lvl) =
        (L.find (p . snd) $ IM.assocs $ lheroes lvl) `mplus`
        (L.find (p . snd) $ IM.assocs $ lmonsters lvl)
  in case mapMaybe chk (currentFirst slid sdungeon) of
    []      -> Nothing
    res : _ -> Just res

getPlayerBody :: State -> Actor
getPlayerBody s@State{splayer} =
  let (_, actor, _) = findActorAnyLevel splayer s
  in actor

getPlayerItem :: State -> [Item]
getPlayerItem s@State{splayer} =
  let (_, _, items) = findActorAnyLevel splayer s
  in items

-- | The list of actors and their levels for all heroes in the dungeon.
allHeroesAnyLevel :: State -> [(ActorId, LevelId)]
allHeroesAnyLevel State{slid, sdungeon} =
  let one (ln, Level{lheroes}) =
        L.map (\ (i, _) -> (AHero i, ln)) (IM.assocs lheroes)
  in L.concatMap one (currentFirst slid sdungeon)

updateAnyActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateAnyActorBody actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
  in case actor of
       AHero n    -> updateAnyLevel (updateHeroes   $ IM.adjust f n) ln state
       AMonster n -> updateAnyLevel (updateMonsters $ IM.adjust f n) ln state

updateAnyActorItem :: ActorId -> ([Item] -> [Item]) -> State -> State
updateAnyActorItem actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
      g Nothing   = Just $ f []
      g (Just is) = Just $ f is
  in case actor of
       AHero n    -> updateAnyLevel (updateHeroItem $ IM.alter g n) ln state
       AMonster n -> updateAnyLevel (updateMonItem  $ IM.alter g n) ln state

updateAnyLevel :: (Level -> Level) -> LevelId -> State -> State
updateAnyLevel f ln s@State{slid, sdungeon}
  | ln == slid = updateLevel f s
  | otherwise = updateDungeon (const $ adjust f ln sdungeon) s

-- | Calculate the location of player's target.
targetToLoc :: IS.IntSet -> State -> Maybe Point
targetToLoc visible s@State{slid, scursor} =
  case btarget (getPlayerBody s) of
    TLoc loc -> Just loc
    TCursor  ->
      if slid == clocLn scursor
      then Just $ clocation scursor
      else Nothing  -- cursor invalid: set at a different level
    TEnemy a _ll -> do
      guard $ memActor a s           -- alive and on the current level?
      let loc = bloc (getActor a s)
      guard $ IS.member loc visible  -- visible?
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

-- | Gets actor's items from the current level. Empty list, if not found.
getActorItem :: ActorId -> State -> [Item]
getActorItem a state =
  fromMaybe [] $
  case a of
    AHero n    -> IM.lookup n (lheroItem (slevel state))
    AMonster n -> IM.lookup n (lmonItem  (slevel state))

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a =
  case a of
    AHero n ->
      updateLevel (updateHeroes (IM.delete n) . updateHeroItem (IM.delete n))
    AMonster n ->
      updateLevel (updateMonsters (IM.delete n) . updateMonItem (IM.delete n))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.insert n m))
    AMonster n -> updateLevel (updateMonsters (IM.insert n m))

-- | Removes a player from the current level and party list.
deletePlayer :: State -> State
deletePlayer s@State{splayer} = deleteActor splayer s

levelHeroList, levelMonsterList :: State -> [Actor]
levelHeroList    state = IM.elems $ lheroes   $ slevel state
levelMonsterList state = IM.elems $ lmonsters $ slevel state

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: Point -> State -> Maybe ActorId
locToActor loc state =
  let l = locToActors loc state
  in assert (L.length l <= 1 `blame` l) $
     listToMaybe l

locToActors :: Point -> State -> [ActorId]
locToActors loc state =
  getIndex (lmonsters, AMonster) ++ getIndex (lheroes, AHero)
 where
  getIndex (projection, injection) =
    let l  = IM.assocs $ projection $ slevel state
        im = L.filter (\ (_i, m) -> bloc m == loc) l
    in fmap (injection . fst) im

nearbyFreeLoc :: Kind.Ops TileKind -> Point -> State -> Point
nearbyFreeLoc cotile start state =
  let lvl@Level{lxsize, lysize} = slevel state
      hs = levelHeroList state
      ms = levelMonsterList state
      locs = start : L.nub (concatMap (vicinity lxsize lysize) locs)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && loc `notElem` L.map bloc (hs ++ ms)
  in fromMaybe (assert `failure` "too crowded map") $ L.find good locs

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: Kind.Ops ItemKind -> State -> Int
calculateTotal coitem s =
  L.sum $ L.map (itemPrice coitem) $ L.concat $ IM.elems $ lheroItem $ slevel s

-- Adding heroes

tryFindHeroK :: State -> Int -> Maybe Int
tryFindHeroK s k =
  let c | k == 0          = Nothing
        | k > 0 && k < 10 = Just $ Char.intToDigit k
        | otherwise       = assert `failure` k
  in fmap fst $ tryFindActor s ((== c) . bsymbol)

-- | Create a new hero on the current level, close to the given location.
addHero :: Kind.COps -> Point -> State -> State
addHero Kind.COps{coactor, cotile} ploc state@State{scounter} =
  let config = sconfig state
      bHP = Config.get config "heroes" "baseHP"
      loc = nearbyFreeLoc cotile ploc state
      n = case L.elemIndex Nothing $ map (tryFindHeroK state) [0..9] of
        Nothing -> 10
        Just k -> k
      symbol = if n < 1 || n > 9 then Nothing else Just $ Char.intToDigit n
      name = findHeroName config n
      startHP = bHP `div` min 5 (n + 1)
      m = template
            (heroKindId coactor) symbol (Just name) startHP loc heroParty
      state' = state { scounter = scounter + 1 }
  in updateLevel (updateHeroes (IM.insert scounter m)) state'

-- | Create a set of initial heroes on the current level, at location ploc.
initialHeroes :: Kind.COps -> Point -> State -> State
initialHeroes cops ploc state =
  let k = 1 + Config.get (sconfig state) "heroes" "extraHeroes"
  in iterate (addHero cops ploc) state !! k

-- Adding monsters

-- | Create a new monster in the level, at a given position
-- and with a given actor kind and HP.
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Point -> State
           -> State
addMonster cotile mk hp ploc state@State{scounter} = do
  let loc = nearbyFreeLoc cotile ploc state
      m = template mk Nothing Nothing hp loc monsterParty
      state' = state {scounter = scounter + 1}
  updateLevel (updateMonsters (IM.insert scounter m)) state'
