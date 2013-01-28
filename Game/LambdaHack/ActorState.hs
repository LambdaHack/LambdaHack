{-# LANGUAGE OverloadedStrings #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.ActorState
  ( actorAssocs, actorList, actorNotProjAssocs, actorNotProjList
  , isProjectile, calculateTotal, allActorsAnyLevel, nearbyFreePos, whereTo
  , posToActor, deleteActor, updateActorItem, getItemBody
  , insertActor, memActor, getActorBody, updateActorBody
  , getActorBag, getActorItem, tryFindHeroK, foesAdjacent
  ) where

import qualified Data.Char as Char
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Actor
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert

actorAssocs :: (FactionId -> Bool) -> Level -> [(ActorId, Actor)]
actorAssocs p lvl = filter (p . bfaction . snd) $ EM.toList $ lactor lvl

actorList :: (FactionId -> Bool) -> Level -> [Actor]
actorList p lvl = filter (p . bfaction) $ EM.elems $ lactor lvl

actorNotProjAssocs :: (FactionId -> Bool) -> Level -> [(ActorId, Actor)]
actorNotProjAssocs p lvl =
  filter (\(_, m) -> not (bproj m) && p (bfaction m)) $ EM.toList $ lactor lvl

actorNotProjList :: (FactionId -> Bool) -> Level -> [Actor]
actorNotProjList p lvl =
  filter (\m -> not (bproj m) && p (bfaction m)) $ EM.elems $ lactor lvl

-- | Checks whether an actor identifier represents a hero.
isProjectile :: State -> ActorId -> Bool
isProjectile s aid = bproj $ getActorBody aid s

-- | Finds an actor at a position on the current level. Perception irrelevant.
posToActor :: Point -> State -> Maybe ActorId
posToActor loc s =
  let l = posToActors loc s
  in assert (length l <= 1 `blame` l) $
     listToMaybe l

posToActors :: Point -> State -> [ActorId]
posToActors loc s =
    let l  = EM.assocs $ lactor $ getArena s
        im = filter (\ (_i, m) -> bpos m == loc) l
    in fmap fst im

nearbyFreePos :: Kind.Ops TileKind -> Point -> State -> Point
nearbyFreePos cotile start s =
  let lvl@Level{lxsize, lysize, lactor} = getArena s
      poss = start : nub (concatMap (vicinity lxsize lysize) poss)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && unoccupied (EM.elems lactor) loc
  in fromMaybe (assert `failure` ("too crowded map" :: Text))
     $ find good poss

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: State -> (ItemBag, Int)
calculateTotal s =
  let lvl = getArena s
      bag = EM.unionsWith joinItem
            $ map bitem $ actorList (== sside s) lvl
      heroItem = map (\(iid, (k, _)) -> (getItemBody iid lvl, k))
                 $ EM.assocs bag
  in (bag, sum $ map itemPrice heroItem)

-- | Price an item, taking count into consideration.
itemPrice :: (Item, Int) -> Int
itemPrice (item, jcount) =
  case jsymbol item of
    '$' -> jcount
    '*' -> jcount * 100
    _   -> 0

foesAdjacent :: X -> Y -> Point -> [Actor] -> Bool
foesAdjacent lxsize lysize loc foes =
  let vic = ES.fromList $ vicinity lxsize lysize loc
      lfs = ES.fromList $ map bpos foes
  in not $ ES.null $ ES.intersection vic lfs

-- * These few operations look at, potentially, all levels of the dungeon.

-- | The list of all non-projectile actors and their levels in the dungeon,
-- starting with the selected level.
allActorsAnyLevel :: State -> [(LevelId, (ActorId, Actor))]
allActorsAnyLevel s =
  let one (ln, lvl) =
        [ (ln, (a, m)) | (a, m) <- EM.toList $ lactor lvl
                       , not (bproj m) ]
      butArena = EM.delete (sarena s) (sdungeon s)
      selectedFirst = (sarena s, sdungeon s EM.! sarena s) : EM.toList butArena
  in concatMap one selectedFirst

-- TODO: start with current level; also elsewhere
-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (LevelId, (ActorId, Actor))
tryFindActor s p =
  let chk (ln, lvl) =
        fmap (\a -> (ln, a)) $ find (p . snd) $ EM.assocs $ lactor lvl
  in case mapMaybe chk $ EM.toList $ sdungeon s of
    [] -> Nothing
    (ln, (aid, body)) : _ -> Just (ln, (aid, body))

tryFindHeroK :: State -> FactionId -> Int -> Maybe (LevelId, (ActorId, Actor))
tryFindHeroK s fact k =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = assert `failure` k
  in tryFindActor s (\body -> bsymbol body == Just c && bfaction body == fact)

-- | Compute the level identifier and starting position on the level,
-- after a level change.
whereTo :: State    -- ^ game state
        -> LevelId  -- ^ start from this level
        -> Int      -- ^ jump down this many levels
        -> Maybe (LevelId, Point)
                    -- ^ target level and the position of its receiving stairs
whereTo s lid k = assert (k /= 0) $
  case ascendInBranch (sdungeon s) lid k of
    [] -> Nothing
    ln : _ -> let lvlTrg = sdungeon s EM.! ln
              in Just (ln, (if k < 0 then fst else snd) (lstair lvlTrg))

-- * The operations below disregard levels other than the current.

-- | Gets actor body from the current level. Error if not found.
getActorBody :: ActorId -> State -> Actor
getActorBody a s = lactor (getArena s) EM.! a

updateActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateActorBody actor f s = updateArena (updateActor $ EM.adjust f actor) s

getActorBag :: ActorId -> State -> ItemBag
getActorBag aid s = bitem $ getActorBody aid s

-- | Gets actor's items from the current level. Warning: this does not work
-- for viewing items of actors from remote level.
getActorItem :: ActorId -> State -> [Item]
getActorItem aid s =
  map (flip getItemBody (getArena s)) $ EM.keys $ getActorBag aid s

updateActorItem :: ActorId -> (ItemBag -> ItemBag) -> State -> State
updateActorItem actor f s =
  let g body = body {bitem = f $ bitem body}
  in updateArena (updateActor $ EM.adjust g actor) s

getItemBody :: ItemId -> Level -> Item
getItemBody iid lvl = litem lvl EM.! iid

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> State -> Bool
memActor a s = EM.member a (lactor (getArena s))

-- | Add actor to the current level.
insertActor :: ActorId -> Actor -> State -> State
insertActor a m = updateArena (updateActor (EM.insert a m))

-- | Removes the actor, if present, from the current level.
deleteActor :: ActorId -> State -> State
deleteActor a = updateArena (updateActor (EM.delete a))
