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
import Game.LambdaHack.PointXY
import Game.LambdaHack.Vector
import Game.LambdaHack.Actor
import Game.LambdaHack.Level
import Game.LambdaHack.Dungeon
import Game.LambdaHack.State
import Game.LambdaHack.Grammar
import Game.LambdaHack.Item
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Time

-- | Checks whether an actor identifier represents a hero.
isProjectile :: State -> ActorId -> Bool
isProjectile s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bproj actor

-- TODO: currently it's false for player-controlled monsters.
-- When it's no longer, rewrite the places where it matters.
-- | Checks whether an actor identifier represents a hero.
isAHero :: State -> ActorId -> Bool
isAHero s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bfaction actor == sfaction s && not (bproj actor)

-- | Checks whether an actor identifier represents a monster.
isAMonster :: State -> ActorId -> Bool
isAMonster s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bfaction actor /= sfaction s && not (bproj actor)

-- TODO: move to TileState if ever created.
-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: State -> Time
smellTimeout s =
  let smellTurns = Config.get (sconfig s) "monsters" "smellTimeout"
  in timeScale timeTurn smellTurns

-- The operations with "Any", and those that use them,
-- consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Fails if not found.
findActorAnyLevel :: ActorId -> State -> (LevelId, Actor, [Item])
findActorAnyLevel actor State{slid, sdungeon} =
  let chk (ln, lvl) =
        let (m, mi) = (IM.lookup actor (lactor lvl),
                       IM.lookup actor (linv lvl))
        in fmap (\ a -> (ln, a, fromMaybe [] mi)) m
  in case mapMaybe chk (currentFirst slid sdungeon) of
    []      -> assert `failure` actor
    res : _ -> res  -- checking if res is unique would break laziness

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor State{slid, sdungeon} p =
  let chk (_ln, lvl) = L.find (p . snd) $ IM.assocs $ lactor lvl
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
allHeroesAnyLevel :: State -> [ActorId]
allHeroesAnyLevel State{slid, sdungeon, sfaction} =
  let one (_, lvl) = L.map fst (heroAssocs sfaction lvl)
  in L.concatMap one (currentFirst slid sdungeon)

updateAnyActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateAnyActorBody actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
  in updateAnyLevel (updateActorDict $ IM.adjust f actor) ln state

updateAnyActorItem :: ActorId -> ([Item] -> [Item]) -> State -> State
updateAnyActorItem actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
      g Nothing   = Just $ f []
      g (Just is) = Just $ f is
  in updateAnyLevel (updateInv $ IM.alter g actor) ln state

updateAnyLevel :: (Level -> Level) -> LevelId -> State -> State
updateAnyLevel f ln s@State{slid, sdungeon}
  | ln == slid = updateLevel f s
  | otherwise = updateDungeon (const $ adjust f ln sdungeon) s

-- | Calculate the location of player's target.
targetToLoc :: IS.IntSet -> State -> Point -> Maybe Point
targetToLoc visible s@State{slid, scursor} aloc =
  case btarget (getPlayerBody s) of
    TLoc loc -> Just loc
    TPath [] -> Nothing
    TPath (dir:_) -> Just $ shift aloc dir
    TCursor ->
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
-- The order of argument here and in other functions is set to allow
--
-- > b <- gets (memActor a)
memActor :: ActorId -> State -> Bool
memActor a state = IM.member a (lactor (slevel state))

-- | Gets actor body from the current level. Error if not found.
getActor :: ActorId -> State -> Actor
getActor a state = lactor (slevel state) IM.! a

-- | Gets actor's items from the current level. Empty list, if not found.
getActorItem :: ActorId -> State -> [Item]
getActorItem a state = fromMaybe [] $ IM.lookup a (linv (slevel state))

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a =
  updateLevel (updateActorDict (IM.delete a) . updateInv (IM.delete a))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m = updateLevel (updateActorDict (IM.insert a m))

-- | Removes a player from the current level.
deletePlayer :: State -> State
deletePlayer s@State{splayer} = deleteActor splayer s

-- TODO: unify, rename
heroAssocs, hostileAssocs, dangerousAssocs
  :: Kind.Id FactionKind -> Level -> [(ActorId, Actor)]
heroAssocs sfaction lvl =
  filter (\ (_, m) -> bfaction m == sfaction && not (bproj m)) $
    IM.toList $ lactor lvl
hostileAssocs sfaction lvl =
  filter (\ (_, m) -> bfaction m /= sfaction && not (bproj m)) $
    IM.toList $ lactor lvl
dangerousAssocs sfaction lvl =
  filter (\ (_, m) -> bfaction m /= sfaction) $
    IM.toList $ lactor lvl

heroList, hostileList, dangerousList :: State -> [Actor]
heroList state@State{sfaction} =
  filter (\ m -> bfaction m == sfaction && not (bproj m)) $
    IM.elems $ lactor $ slevel state
hostileList state@State{sfaction} =
  filter (\ m -> bfaction m /= sfaction && not (bproj m)) $
    IM.elems $ lactor $ slevel state
dangerousList state@State{sfaction} =
  filter (\ m -> bfaction m /= sfaction) $
    IM.elems $ lactor $ slevel state

factionAssocs :: [Kind.Id FactionKind] -> Level -> [(ActorId, Actor)]
factionAssocs l lvl =
  filter (\ (_, m) -> bfaction m `elem` l) $ IM.toList $ lactor lvl

factionList :: [Kind.Id FactionKind] -> State -> [Actor]
factionList l s =
  filter (\ m -> bfaction m `elem` l) $ IM.elems $ lactor $ slevel s

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: Point -> State -> Maybe ActorId
locToActor loc state =
  let l = locToActors loc state
  in assert (L.length l <= 1 `blame` l) $
     listToMaybe l

locToActors :: Point -> State -> [ActorId]
locToActors loc state =
    let l  = IM.assocs $ lactor $ slevel state
        im = L.filter (\ (_i, m) -> bloc m == loc) l
    in fmap fst im

nearbyFreeLoc :: Kind.Ops TileKind -> Point -> State -> Point
nearbyFreeLoc cotile start state =
  let lvl@Level{lxsize, lysize, lactor} = slevel state
      locs = start : L.nub (concatMap (vicinity lxsize lysize) locs)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && unoccupied (IM.elems lactor) loc
  in fromMaybe (assert `failure` "too crowded map") $ L.find good locs

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: Kind.Ops ItemKind -> State -> ([Item], Int)
calculateTotal coitem s =
  let ha = factionAssocs [sfaction s] $ slevel s
      heroInv = L.concat $ catMaybes $
                  L.map ( \ (k, _) -> IM.lookup k $ linv $ slevel s) ha
  in (heroInv, L.sum $ L.map (itemPrice coitem) heroInv)

foesAdjacent :: X -> Y -> Point -> [Actor] -> Bool
foesAdjacent lxsize lysize loc foes =
  let vic = IS.fromList $ vicinity lxsize lysize loc
      lfs = IS.fromList $ L.map bloc foes
  in not $ IS.null $ IS.intersection vic lfs

-- Adding heroes

tryFindHeroK :: State -> Int -> Maybe ActorId
tryFindHeroK s k =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = assert `failure` k
  in fmap fst $ tryFindActor s ((== Just c) . bsymbol)

-- | Create a new hero on the current level, close to the given location.
addHero :: Kind.COps -> Point -> State -> State
addHero Kind.COps{coactor, cotile} ploc state@State{scounter, sfaction} =
  let config = sconfig state
      bHP = Config.get config "heroes" "baseHP"
      loc = nearbyFreeLoc cotile ploc state
      freeHeroK = L.elemIndex Nothing $ map (tryFindHeroK state) [0..9]
      n = fromMaybe 10 freeHeroK
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName config n
      startHP = bHP `div` min 5 (n + 1)
      m = template (heroKindId coactor) (Just symbol) (Just name)
                   startHP loc (stime state) sfaction False
      cstate = state { scounter = scounter + 1 }
  in updateLevel (updateActorDict (IM.insert scounter m)) cstate

-- | Create a set of initial heroes on the current level, at location ploc.
initialHeroes :: Kind.COps -> Point -> State -> State
initialHeroes cops ploc state =
  let k = 1 + Config.get (sconfig state) "heroes" "extraHeroes"
  in iterate (addHero cops ploc) state !! k

-- Adding monsters

-- | Create a new monster in the level, at a given position
-- and with a given actor kind and HP.
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Point
           -> Kind.Id FactionKind -> Bool -> State -> State
addMonster cotile mk hp ploc bfaction bproj state@State{scounter} = do
  let loc = nearbyFreeLoc cotile ploc state
      m = template mk Nothing Nothing hp loc (stime state) bfaction bproj
      cstate = state {scounter = scounter + 1}
  updateLevel (updateActorDict (IM.insert scounter m)) cstate

-- Adding projectiles

-- | Create a projectile actor containing the given missile.
addProjectile :: Kind.COps -> Item -> Point -> Kind.Id FactionKind
              -> [Point] -> Time -> State -> State
addProjectile Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}}
              item loc bfaction path btime state@State{scounter} =
  let ik = okind (jkind item)
      object = objectItem coitem state item
      name = "a flying " ++ unwords (tail (words object))
      speed = speedFromWeight (iweight ik) (itoThrow ik)
      range = rangeFromSpeed speed
      dirPath = take range $ displacePath path
      m = Actor
        { bkind   = projectileKindId coactor
        , bsymbol = Nothing
        , bname   = Just name
        , bcolor  = Nothing
        , bspeed  = Just speed
        , bhp     = 0
        , bdir    = Nothing
        , btarget = TPath dirPath
        , bloc    = loc
        , bletter = 'a'
        , btime
        , bfaction
        , bproj   = True
        }
      cstate = state { scounter = scounter + 1 }
      upd = updateActorDict (IM.insert scounter m)
            . updateInv (IM.insert scounter [item])
  in updateLevel upd cstate
