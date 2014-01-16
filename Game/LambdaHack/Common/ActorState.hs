-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.Common.ActorState
  ( actorAssocs, actorList, actorNotProjAssocs, actorNotProjList
  , calculateTotal, nearbyFreePoints, whereTo
  , posToActors, posToActor, getItemBody, memActor
  , getActorBody, updateActorBody
  , getActorItem, getActorBag, actorContainer, actorContainerB, getActorInv
  , tryFindHeroK, foesAdjacent
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  mapMaybe (\aid -> let actor = sactorD s EM.! aid
                    in if p (bfid actor)
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
                    in if not (bproj actor) && p (bfid actor)
                       then Just (aid, actor)
                       else Nothing)
  $ concat $ EM.elems $ lprio $ sdungeon s EM.! lid

actorNotProjList :: (FactionId -> Bool) -> LevelId -> State
                 -> [Actor]
actorNotProjList p lid s = map snd $ actorNotProjAssocs p lid s

-- | Finds an actor at a position on the current level.
posToActor :: Point -> LevelId -> State
           -> Maybe ((ActorId, Actor), [(ItemId, Item)])
posToActor pos lid s = listToMaybe $ posToActors pos lid s

posToActors :: Point -> LevelId -> State
            -> [((ActorId, Actor), [(ItemId, Item)])]
posToActors pos lid s =
  let as = actorAssocs (const True) lid s
      aps = filter (\(_, b) -> bpos b == pos) as
      f iid = (iid, getItemBody iid s)
      g (aid, b) = ((aid, b), map f $ EM.keys $ bbag b)
      l = map g aps
  in assert (length l <= 1 || all (bproj . snd . fst) l
             `blame` "many actors at the same position" `twith` l)
     l

nearbyFreePoints :: Kind.Ops TileKind
                 -> (Kind.Id TileKind -> Bool) -> Point -> LevelId -> State
                 -> [Point]
nearbyFreePoints cotile f start lid s =
  let lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      as = actorList (const True) lid s
      good p = f (lvl `at` p)
               && Tile.hasFeature cotile F.Walkable (lvl `at` p)
               && unoccupied as p
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good ps

-- | Calculate loot's worth for heroes on the current level.
--
-- Warning: scores are shown during the game, so when the server calculates
-- then, we should be careful not to leak secret information
-- (e.g., the nature of the items through the total worth of inventory).
calculateTotal :: Actor -> State -> (ItemBag, Int)
calculateTotal body s =
  let bs = actorList (== bfid body) (blid body) s
      bag = EM.unionsWith (+) $ map bbag $ if null bs then [body] else bs
      items = map (\(iid, k) -> (getItemBody iid s, k))
              $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

-- | Price an item, taking count into consideration.
itemPrice :: (Item, Int) -> Int
itemPrice (item, jcount) =
  case jsymbol item of
    '$' -> jcount
    '*' -> jcount * 100
    _   -> 0

foesAdjacent :: X -> Y -> Point -> [Actor] -> Bool
foesAdjacent lxsize lysize pos foes =
  let vic = ES.fromList $ vicinity lxsize lysize pos
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
        | otherwise       = assert `failure` "no digit" `twith` k
  in tryFindActor s (\body -> bsymbol body == c
                              && not (bproj body)
                              && bfid body == fact)

-- | Compute the level identifier and starting position on the level,
-- after a level change.
whereTo :: LevelId  -- ^ level of the stairs
        -> Point    -- ^ position of the stairs
        -> Int      -- ^ jump up this many levels
        -> State    -- ^ game state
        -> (LevelId, Point)
                    -- ^ target level and the position of its receiving stairs
whereTo lid pos k s = assert (k /= 0) $
  let dungeon = sdungeon s
      lvl = dungeon EM.! lid
      stairs = (if k < 0 then snd else fst) (lstair lvl)
      defaultStairs = 0  -- for ascending via, e.g., spells
      mindex = elemIndex pos stairs
      i = fromMaybe defaultStairs mindex
  in case ascendInBranch dungeon lid k of
    [] | isNothing mindex -> (lid, pos)  -- spell fizzles
    [] -> assert `failure` "no dungeon level to go to" `twith` (lid, pos, k)
    ln : _ -> let lvlTgt = dungeon EM.! ln
                  stairsTgt = (if k < 0 then fst else snd) (lstair lvlTgt)
              in if length stairsTgt < i + 1
                 then assert `failure` "no stairs at index"
                             `twith` (lid, pos, k, ln, stairsTgt, i)
                 else (ln, stairsTgt !! i)

-- * The operations below disregard levels other than the current.

-- | Gets actor body from the current level. Error if not found.
getActorBody :: ActorId -> State -> Actor
getActorBody aid s =
  fromMaybe (assert `failure` "body not found" `twith` (aid, s))
  $ EM.lookup aid $ sactorD s

updateActorBody :: ActorId -> (Actor -> Actor) -> State -> State
updateActorBody aid f s =
  let alt Nothing = assert `failure` "no body to update" `twith` (aid, s)
      alt (Just b) = Just $ f b
  in updateActorD (EM.alter alt aid) s

getActorBag :: ActorId -> State -> ItemBag
getActorBag aid s = bbag $ getActorBody aid s

actorContainer :: ActorId -> ItemInv -> ItemId -> Container
actorContainer aid binv iid =
  case find ((== iid) . snd) $ EM.assocs binv of
    Just (l, _) -> CActor aid l
    Nothing -> assert `failure` "item not in inventory" `twith` (aid, binv, iid)

actorContainerB :: ActorId -> Actor -> ItemId -> Item -> Maybe Container
actorContainerB aid body iid item =
  case find ((== iid) . snd) $ EM.assocs (binv body) of
    Just (l, _) -> Just $ CActor aid l
    Nothing ->
      let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      in case assignLetter iid l body of
        Just l2 -> Just $ CActor aid l2
        Nothing -> Nothing

getActorInv :: ActorId -> State -> ItemInv
getActorInv aid s = binv $ getActorBody aid s

-- | Gets actor's items from the current level. Warning: this does not work
-- for viewing items of actors from remote level.
getActorItem :: ActorId -> State -> [(ItemId, Item)]
getActorItem aid s =
  let f iid = (iid, getItemBody iid s)
  in map f $ EM.keys $ getActorBag aid s

getItemBody :: ItemId -> State -> Item
getItemBody iid s =
  fromMaybe (assert `failure` "item body not found"
                    `twith` (iid, s)) $ EM.lookup iid $ sitemD s

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> LevelId -> State -> Bool
memActor aid lid s =
  maybe False ((== lid) . blid) $ EM.lookup aid $ sactorD s
