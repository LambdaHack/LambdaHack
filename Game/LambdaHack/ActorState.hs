{-# LANGUAGE OverloadedStrings #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.ActorState
  ( isProjectile, isAHero, getPlayerBody, findActorAnyLevel, calculateTotal
  , initialHeroes, deletePlayer, allHeroesAnyLevel
  , locToActor, deleteActor, addHero, addMonster, updateAnyActorItem
  , insertActor, heroList, memActor, getActor, updateAnyActorBody
  , hostileList, getActorItem, getPlayerItem, tryFindHeroK, dangerousList
  , factionList, addProjectile, foesAdjacent, targetToLoc, hostileAssocs
  ) where

import Control.Monad
import qualified Data.Char as Char
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | Checks whether an actor identifier represents a hero.
isProjectile :: State -> ActorId -> Bool
isProjectile s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bproj actor

-- | Checks whether an actor identifier represents a hero.
isAHero :: State -> ActorId -> Bool
isAHero s a =
  let (_, actor, _) = findActorAnyLevel a s
  in bfaction actor == sside s && not (bproj actor)

-- The operations with "Any", and those that use them,
-- consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Fails if not found.
findActorAnyLevel :: ActorId -> State -> (LevelId, Actor, [Item])
findActorAnyLevel actor State{sdungeon} =
  let chk (ln, lvl) =
        let (m, mi) = (IM.lookup actor (lactor lvl),
                       IM.lookup actor (linv lvl))
        in fmap (\ a -> (ln, a, fromMaybe [] mi)) m
  in case mapMaybe chk (M.toList sdungeon) of
    []      -> assert `failure` actor
    res : _ -> res  -- checking if res is unique would break laziness

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor State{sdungeon} p =
  let chk (_ln, lvl) = L.find (p . snd) $ IM.assocs $ lactor lvl
  in case mapMaybe chk (M.toList sdungeon) of
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
allHeroesAnyLevel State{sdungeon, sside} =
  let one (_, lvl) =
        [ a | (a, m) <- IM.toList $ lactor lvl
            , bfaction m == sside && not (bproj m) ]
  in L.concatMap one (M.toList sdungeon)

updateAnyActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateAnyActorBody actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
  in updateAnyLevel (updateActor $ IM.adjust f actor) ln state

updateAnyActorItem :: ActorId -> ([Item] -> [Item]) -> State -> State
updateAnyActorItem actor f state =
  let (ln, _, _) = findActorAnyLevel actor state
      g Nothing   = Just $ f []
      g (Just is) = Just $ f is
  in updateAnyLevel (updateInv $ IM.alter g actor) ln state

updateAnyLevel :: (Level -> Level) -> LevelId -> State -> State
updateAnyLevel f ln s@State{sarena, sdungeon}
  | ln == sarena = updateArena f s
  | otherwise = updateDungeon (const $ M.adjust f ln sdungeon) s

-- | Calculate the location of player's target.
targetToLoc :: StateClient -> State -> Maybe Point
targetToLoc StateClient{scursor, starget} s@State{sarena, splayer} =
  case IM.lookup splayer starget of
    Just (TLoc loc) -> Just loc
    Nothing ->
      if sarena == clocLn scursor
      then Just $ clocation scursor
      else Nothing  -- cursor invalid: set at a different level
    Just (TEnemy a _ll) -> do
      guard $ memActor a s           -- alive and visible?
      let loc = bloc (getActor a s)
      return loc

-- The operations below disregard levels other than the current.

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsGlobal (memActor a)
memActor :: ActorId -> State -> Bool
memActor a state = IM.member a (lactor (getArena state))

-- | Gets actor body from the current level. Error if not found.
getActor :: ActorId -> State -> Actor
getActor a state = lactor (getArena state) IM.! a

-- | Gets actor's items from the current level. Empty list, if not found.
getActorItem :: ActorId -> State -> [Item]
getActorItem a state = fromMaybe [] $ IM.lookup a (linv (getArena state))

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a =
  updateArena (updateActor (IM.delete a) . updateInv (IM.delete a))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m = updateArena (updateActor (IM.insert a m))

-- | Removes a player from the current level.
deletePlayer :: State -> State
deletePlayer s@State{splayer} = deleteActor splayer s

-- TODO: unify, rename
hostileAssocs :: FactionId -> Level -> [(ActorId, Actor)]
hostileAssocs faction lvl =
  filter (\ (_, m) -> bfaction m /= faction && not (bproj m)) $
    IM.toList $ lactor lvl
heroList, hostileList, dangerousList :: State -> [Actor]
heroList state@State{sside} =
  filter (\ m -> bfaction m == sside && not (bproj m)) $
    IM.elems $ lactor $ getArena state
hostileList state@State{sside} =
  filter (\ m -> bfaction m /= sside && not (bproj m)) $
    IM.elems $ lactor $ getArena state
dangerousList state@State{sside} =
  filter (\ m -> bfaction m /= sside) $
    IM.elems $ lactor $ getArena state

factionAssocs :: [FactionId] -> Level -> [(ActorId, Actor)]
factionAssocs l lvl =
  filter (\ (_, m) -> bfaction m `elem` l) $ IM.toList $ lactor lvl

factionList :: [FactionId] -> State -> [Actor]
factionList l s =
  filter (\ m -> bfaction m `elem` l) $ IM.elems $ lactor $ getArena s

-- | Finds an actor at a location on the current level. Perception irrelevant.
locToActor :: Point -> State -> Maybe ActorId
locToActor loc state =
  let l = locToActors loc state
  in assert (L.length l <= 1 `blame` l) $
     listToMaybe l

locToActors :: Point -> State -> [ActorId]
locToActors loc state =
    let l  = IM.assocs $ lactor $ getArena state
        im = L.filter (\ (_i, m) -> bloc m == loc) l
    in fmap fst im

nearbyFreeLoc :: Kind.Ops TileKind -> Point -> State -> Point
nearbyFreeLoc cotile start state =
  let lvl@Level{lxsize, lysize, lactor} = getArena state
      locs = start : L.nub (concatMap (vicinity lxsize lysize) locs)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && unoccupied (IM.elems lactor) loc
  in fromMaybe (assert `failure` ("too crowded map" :: Text))
     $ L.find good locs

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: State -> ([Item], Int)
calculateTotal s =
  let ha = factionAssocs [sside s] $ getArena s
      heroInv = L.concat $ catMaybes $
                  L.map ( \ (k, _) -> IM.lookup k $ linv $ getArena s) ha
  in (heroInv, L.sum $ L.map itemPrice heroInv)

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
addHero :: Kind.COps -> Point -> ConfigUI -> State -> StateServer
        -> (State, StateServer)
addHero Kind.COps{coactor, cotile} ploc configUI
        state@State{sside} ser@StateServer{scounter} =
  let Config{configBaseHP} = sconfig ser
      loc = nearbyFreeLoc cotile ploc state
      freeHeroK = L.elemIndex Nothing $ map (tryFindHeroK state) [0..9]
      n = fromMaybe 100 freeHeroK
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName configUI n
      startHP = configBaseHP - (configBaseHP `div` 5) * min 3 n
      m = template (heroKindId coactor) (Just symbol) (Just name)
                   startHP loc (getTime state) sside False
  in ( updateArena (updateActor (IM.insert scounter m)) state
     , ser { scounter = scounter + 1 } )

-- | Create a set of initial heroes on the current level, at location ploc.
initialHeroes :: Kind.COps -> Point -> ConfigUI -> State-> StateServer
              -> (State, StateServer)
initialHeroes cops ploc configUI state ser =
  let Config{configExtraHeroes} = sconfig ser
      k = 1 + configExtraHeroes
  in iterate (uncurry $ addHero cops ploc configUI) (state, ser) !! k

-- Adding monsters

-- | Create a new monster in the level, at a given position
-- and with a given actor kind and HP.
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Point
           -> FactionId -> Bool -> State -> StateServer
           -> (State, StateServer)
addMonster cotile mk hp ploc bfaction bproj state ser@StateServer{scounter} =
  let loc = nearbyFreeLoc cotile ploc state
      m = template mk Nothing Nothing hp loc (getTime state) bfaction bproj
  in ( updateArena (updateActor (IM.insert scounter m)) state
     , ser {scounter = scounter + 1} )

-- Adding projectiles

-- | Create a projectile actor containing the given missile.
addProjectile :: Kind.COps -> Item -> Point -> FactionId
              -> [Point] -> Time -> State -> StateServer
              -> (State, StateServer)
addProjectile Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}}
              item loc bfaction path btime
              state@State{sdisco} ser@StateServer{scounter} =
  let ik = okind (fromJust $ jkind sdisco item)
      speed = speedFromWeight (iweight ik) (itoThrow ik)
      range = rangeFromSpeed speed
      adj | range < 5 = "falling"
          | otherwise = "flying"
      -- Not much details about a fast flying object.
      (object1, object2) = partItem coitem M.empty item
      name = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      dirPath = take range $ displacePath path
      m = Actor
        { bkind   = projectileKindId coactor
        , bsymbol = Nothing
        , bname   = Just name
        , bcolor  = Nothing
        , bspeed  = Just speed
        , bhp     = 0
        , bdirAI  = Nothing
        , bpath   = Just dirPath
        , bloc    = loc
        , bletter = 'a'
        , btime
        , bwait   = timeZero
        , bfaction
        , bproj   = True
        }
      upd = updateActor (IM.insert scounter m)
            . updateInv (IM.insert scounter [item])
  in ( updateArena upd state
     , ser {scounter = scounter + 1} )
