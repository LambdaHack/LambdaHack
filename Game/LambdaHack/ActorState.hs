{-# LANGUAGE OverloadedStrings #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.ActorState
  ( actorAssocs, actorList, actorNotProjAssocs, actorNotProjList
  , isProjectile, calculateTotal, allActorsAnyLevel, nearbyFreePos, whereTo
  , posToActor, deleteActor, updateActorItem
  , insertActor, memActor, getActorBody, updateActorBody
  , getActorItem, tryFindHeroK, foesAdjacent
  ) where

import qualified Data.Char as Char
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)

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
actorAssocs p lvl = filter (p . bfaction . snd) $ IM.toList $ lactor lvl

actorList :: (FactionId -> Bool) -> Level -> [Actor]
actorList p lvl = filter (p . bfaction) $ IM.elems $ lactor lvl

actorNotProjAssocs :: (FactionId -> Bool) -> Level -> [(ActorId, Actor)]
actorNotProjAssocs p lvl =
  filter (\(_, m) -> not (bproj m) && p (bfaction m)) $ IM.toList $ lactor lvl

actorNotProjList :: (FactionId -> Bool) -> Level -> [Actor]
actorNotProjList p lvl =
  filter (\m -> not (bproj m) && p (bfaction m)) $ IM.elems $ lactor lvl

-- | Checks whether an actor identifier represents a hero.
isProjectile :: State -> ActorId -> Bool
isProjectile s aid = bproj $ getActorBody aid s

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
  let ha = actorAssocs (== sside s) $ getArena s
      heroInv = L.concat $ catMaybes $
                  L.map ( \ (k, _) -> IM.lookup k $ linv $ getArena s) ha
  in (heroInv, L.sum $ L.map itemPrice heroInv)

foesAdjacent :: X -> Y -> Point -> [Actor] -> Bool
foesAdjacent lxsize lysize loc foes =
  let vic = IS.fromList $ vicinity lxsize lysize loc
      lfs = IS.fromList $ L.map bpos foes
  in not $ IS.null $ IS.intersection vic lfs

-- * These few operations look at, potentially, all levels of the dungeon.

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

-- TODO: start with current level; also elsewhere
-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (LevelId, ActorId)
tryFindActor s p =
  let chk (ln, lvl) =
        fmap (\a -> (ln, a)) $ L.find (p . snd) $ IM.assocs $ lactor lvl
  in case mapMaybe chk $ M.toList $ sdungeon s of
    [] -> Nothing
    (ln, (aid, _)) : _ -> Just (ln, aid)

tryFindHeroK :: State -> FactionId -> Int -> Maybe (LevelId, ActorId)
tryFindHeroK s fact k =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = assert `failure` k
  in tryFindActor s (\body -> bsymbol body == Just c && bfaction body == fact)

-- | Compute the level identifier and starting position on the level,
-- after a level change.
whereTo :: State    -- ^ game state
        -> LevelId  -- ^ start from this level
        -> Int      -- ^ jump this many levels
        -> Maybe (LevelId, Point)
                    -- ^ target level and the position of its receiving stairs
whereTo s lid k = assert (k /= 0) $
  let n = levelNumber lid
      nln = n - k
      ln = levelDefault nln
  in case M.lookup ln (sdungeon s) of
    Nothing     -> Nothing
    Just lvlTrg -> Just (ln, (if k < 0 then fst else snd) (lstair lvlTrg))

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
