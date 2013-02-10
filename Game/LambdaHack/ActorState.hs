{-# LANGUAGE OverloadedStrings #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.ActorState
  ( actorAssocs, actorList, actorNotProjAssocs, actorNotProjList
  , isProjectile, calculateTotal, nearbyFreePos, whereTo
  , posToActor, getItemBody, memActor, getActorBody, updateActorBody
  , getActorItem, getActorBag, getActorInv, tryFindHeroK, foesAdjacent
  ) where

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
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

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  mapMaybe (\aid -> let actor = sactorD s EM.! aid
                    in if p (bfaction actor)
                       then Just (aid, actor)
                       else Nothing)
  $ concat $ EM.elems $ lprio $ sdungeon s EM.! lid

actorList :: (FactionId -> Bool) -> LevelId -> State
          -> [Actor]
actorList p lid s = map snd $ actorAssocs p lid s

actorNotProjAssocs :: (FactionId -> Bool) -> LevelId -> State
                   -> [(ActorId, Actor)]
actorNotProjAssocs p lid s =
  mapMaybe (\aid -> let actor = sactorD s EM.! aid
                    in if not (bproj actor) && p (bfaction actor)
                       then Just (aid, actor)
                       else Nothing)
  $ concat $ EM.elems $ lprio $ sdungeon s EM.! lid

actorNotProjList :: (FactionId -> Bool) -> LevelId -> State
                 -> [Actor]
actorNotProjList p lid s = map snd $ actorNotProjAssocs p lid s

-- | Checks whether an actor identifier represents a hero.
isProjectile :: State -> ActorId -> Bool
isProjectile s aid = bproj $ getActorBody aid s

-- | Finds an actor at a position on the current level. Perception irrelevant.
posToActor :: Point -> LevelId -> State -> Maybe ActorId
posToActor pos lid s =
  let l = posToActors pos lid s
  in assert (length l <= 1 `blame` l) $
     listToMaybe l

posToActors :: Point -> LevelId -> State -> [ActorId]
posToActors pos lid s =
    let as = actorAssocs (const True) lid s
        apos = filter (\(_, b) -> bpos b == pos) as
    in fmap fst apos

nearbyFreePos :: Kind.Ops TileKind -> Point -> LevelId -> State -> Point
nearbyFreePos cotile start lid s =
  let lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      poss = start : nub (concatMap (vicinity lxsize lysize) poss)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && unoccupied (actorList (const True) lid s) loc
  in fromMaybe (assert `failure` ("too crowded map" :: Text))
     $ find good poss

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: FactionId -> LevelId -> State -> (ItemBag, Int)
calculateTotal fid lid s =
  let bag = EM.unionsWith (+)
            $ map bbag $ actorList (== fid) lid s
      heroItem = map (\(iid, k) -> (getItemBody iid s, k))
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

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor s p =
  find (p . snd) $ EM.assocs $ sactorD s

tryFindHeroK :: State -> FactionId -> Int -> Maybe (ActorId, Actor)
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
getActorBody aid s = sactorD s EM.! aid

updateActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateActorBody aid f s = updateActorD (EM.adjust f aid) s

getActorBag :: ActorId -> State -> ItemBag
getActorBag aid s = bbag $ getActorBody aid s

getActorInv :: ActorId -> State -> ItemInv
getActorInv aid s = binv $ getActorBody aid s

-- | Gets actor's items from the current level. Warning: this does not work
-- for viewing items of actors from remote level.
getActorItem :: ActorId -> State -> [Item]
getActorItem aid s =
  map (flip getItemBody s) $ EM.keys $ getActorBag aid s

getItemBody :: ItemId -> State -> Item
getItemBody iid s = sitemD s EM.! iid

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> LevelId -> State -> Bool
memActor a lid s = lid == blvl (getActorBody a s)
