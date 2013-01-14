{-# LANGUAGE OverloadedStrings #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.ActorState
  ( isProjectile, isAHero, calculateTotal
  , initialHeroes, allActorsAnyLevel
  , posToActor, deleteActor, addHero, addMonster, updateActorItem
  , insertActor, heroList, memActor, getActorBody, updateActorBody
  , hostileList, getActorItem, tryFindHeroK, dangerousList
  , factionList, addProjectile, foesAdjacent, targetToPos, hostileAssocs
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
isProjectile s aid = bproj $ getActorBody aid s

-- | Checks whether an actor identifier represents a hero.
isAHero :: State -> ActorId -> Bool
isAHero s a =
  let actor = getActorBody a s
  in bfaction actor == sside s && not (bproj actor)

-- TODO: unify, rename
hostileAssocs :: FactionId -> Level -> [(ActorId, Actor)]
hostileAssocs faction lvl =
  filter (\ (_, m) -> bfaction m /= faction && not (bproj m)) $
    IM.toList $ lactor lvl
heroList, hostileList, dangerousList :: State -> [Actor]
heroList s =
  filter (\ m -> bfaction m == sside s && not (bproj m)) $
    IM.elems $ lactor $ getArena s
hostileList s =
  filter (\ m -> bfaction m /= sside s && not (bproj m)) $
    IM.elems $ lactor $ getArena s
dangerousList s =
  filter (\ m -> bfaction m /= sside s) $
    IM.elems $ lactor $ getArena s

factionAssocs :: [FactionId] -> Level -> [(ActorId, Actor)]
factionAssocs l lvl =
  filter (\ (_, m) -> bfaction m `elem` l) $ IM.toList $ lactor lvl

factionList :: [FactionId] -> State -> [Actor]
factionList l s =
  filter (\ m -> bfaction m `elem` l) $ IM.elems $ lactor $ getArena s

-- | Calculate the position of leader's target.
targetToPos :: StateClient -> State -> Maybe Point
targetToPos cli@StateClient{scursor} s = do
  leader <- getLeader cli
  case getTarget leader cli of
    Just (TPos pos) -> return pos
    Just (TEnemy a _ll) -> do
      guard $ memActor a s           -- alive and visible?
      return $! bpos (getActorBody a s)
    Nothing -> return scursor

-- | Finds an actor at a position on the current level. Perception irrelevant.
posToActor :: Point -> State -> Maybe ActorId
posToActor loc s =
  let l = posToActors loc s
  in assert (L.length l <= 1 `blame` l) $
     listToMaybe l

posToActors :: Point -> State -> [ActorId]
posToActors loc s =
    let l  = IM.assocs $ lactor $ getArena s
        im = L.filter (\ (_i, m) -> bpos m == loc) l
    in fmap fst im

nearbyFreePos :: Kind.Ops TileKind -> Point -> State -> Point
nearbyFreePos cotile start s =
  let lvl@Level{lxsize, lysize, lactor} = getArena s
      poss = start : L.nub (concatMap (vicinity lxsize lysize) poss)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && unoccupied (IM.elems lactor) loc
  in fromMaybe (assert `failure` ("too crowded map" :: Text))
     $ L.find good poss

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
      lfs = IS.fromList $ L.map bpos foes
  in not $ IS.null $ IS.intersection vic lfs

-- Adding heroes

-- | Create a new hero on the current level, close to the given position.
addHero :: Kind.COps -> Point -> FactionId -> State -> StateServer
        -> (State, StateServer)
addHero Kind.COps{coactor, cotile} ppos side
        s ser@StateServer{scounter} =
  let config@Config{configBaseHP} = sconfig ser
      loc = nearbyFreePos cotile ppos s
      freeHeroK = L.elemIndex Nothing $ map (tryFindHeroK s) [0..9]
      n = fromMaybe 100 freeHeroK
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName config n
      startHP = configBaseHP - (configBaseHP `div` 5) * min 3 n
      m = template (heroKindId coactor) (Just symbol) (Just name)
                   startHP loc (getTime s) side False
  in ( updateArena (updateActor (IM.insert scounter m)) s
     , ser { scounter = scounter + 1 } )

-- | Create a set of initial heroes on the current level, at position ploc.
initialHeroes :: Kind.COps -> Point -> FactionId -> State -> StateServer
              -> (State, StateServer)
initialHeroes cops ppos side s ser =
  let Config{configExtraHeroes} = sconfig ser
      k = 1 + configExtraHeroes
  in iterate (uncurry $ addHero cops ppos side) (s, ser) !! k

-- Adding monsters

-- | Create a new monster in the level, at a given position
-- and with a given actor kind and HP.
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Point
           -> FactionId -> Bool -> State -> StateServer
           -> (State, StateServer)
addMonster cotile mk hp ppos bfaction bproj s ser@StateServer{scounter} =
  let loc = nearbyFreePos cotile ppos s
      m = template mk Nothing Nothing hp loc (getTime s) bfaction bproj
  in ( updateArena (updateActor (IM.insert scounter m)) s
     , ser {scounter = scounter + 1} )

-- Adding projectiles

-- | Create a projectile actor containing the given missile.
addProjectile :: Kind.COps -> Item -> Point -> FactionId
              -> [Point] -> Time -> State -> StateServer
              -> (State, StateServer)
addProjectile Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}}
              item loc bfaction path btime
              s ser@StateServer{scounter} =
  let ik = okind (fromJust $ jkind (sdisco s) item)
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
        , bpos    = loc
        , bletter = 'a'
        , btime
        , bwait   = timeZero
        , bfaction
        , bproj   = True
        }
      upd = updateActor (IM.insert scounter m)
            . updateInv (IM.insert scounter [item])
  in ( updateArena upd s
     , ser {scounter = scounter + 1} )

-- * These few operations look at all levels of the dungeon.

-- | The list of all non-projectile actors and their levels in the dungeon,
-- starting with the selected level.
allActorsAnyLevel :: State -> [(LevelId, (ActorId, Actor))]
allActorsAnyLevel s =
  let one (ln, lvl) =
        [ (ln, (a, m)) | (a, m) <- IM.toList $ lactor lvl
                       , not (bproj m) ]
      butFrist = M.delete (sarena s) (sdungeon s)
      selectedFirst = (sarena s, sdungeon s M.! sarena s) : M.toList butFrist
  in L.concatMap one selectedFirst

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (LevelId, ActorId)
tryFindActor s p =
  let chk (ln, lvl) =
        fmap (\a -> (ln, a)) $ L.find (p . snd) $ IM.assocs $ lactor lvl
  in case mapMaybe chk (M.toList (sdungeon s)) of
    [] -> Nothing
    (ln, (aid, _)) : _ -> Just (ln, aid)

tryFindHeroK :: State -> Int -> Maybe (LevelId, ActorId)
tryFindHeroK s k =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = assert `failure` k
  in tryFindActor s ((== Just c) . bsymbol)

-- * The operations below disregard levels other than the current.

-- | Gets actor body from the current level. Error if not found.
getActorBody :: ActorId -> State -> Actor
getActorBody a s = lactor (getArena s) IM.! a

updateActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateActorBody actor f s = updateArena (updateActor $ IM.adjust f actor) s

-- | Gets actor's items from the current level. Empty list, if not found.
getActorItem :: ActorId -> State -> [Item]
getActorItem a s = fromMaybe [] $ IM.lookup a (linv (getArena s))

updateActorItem :: ActorId -> ([Item] -> [Item]) -> State -> State
updateActorItem actor f s =
  let g Nothing   = pack $ f []
      g (Just is) = pack $ f is
      pack [] = Nothing
      pack x  = Just x
  in updateArena (updateInv $ IM.alter g actor) s

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> State -> Bool
memActor a s = IM.member a (lactor (getArena s))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m = updateArena (updateActor (IM.insert a m))

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a =
  updateArena (updateActor (IM.delete a) . updateInv (IM.delete a))
