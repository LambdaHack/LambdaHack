-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, actorAssocsLvl, actorAssocs, actorList
  , actorNotProjAssocsLvl, actorNotProjAssocs, actorNotProjList
  , calculateTotal, sharedInv, sharedAllOwned
  , getInvBag, nearbyFreePoints, whereTo
  , posToActors, posToActor, getItemBody, memActor
  , getActorBody, updateActorBody, updateFactionBody
  , getCarriedAssocs, getEqpAssocs, getInvAssocs, getFloorAssocs
  , actorContainer, actorContainerB
  , tryFindHeroK, foesAdjacent
  , allSlots, assignSlot, slotLabel
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

fidActorNotProjAssocs :: FactionId -> State -> [(ActorId, Actor)]
fidActorNotProjAssocs fid s =
  let f lvl bs = actorNotProjAssocsLvl (== fid) lvl (sactorD s) ++ bs
  in foldr f [] $ EM.elems $ sdungeon s

fidActorNotProjList :: FactionId -> State -> [Actor]
fidActorNotProjList fid s = map snd $ fidActorNotProjAssocs fid s

actorAssocsLvl :: (FactionId -> Bool) -> Level -> ActorDict
               -> [(ActorId, Actor)]
actorAssocsLvl p lvl actorD =
  mapMaybe (\aid -> let actor = actorD EM.! aid
                    in if p (bfid actor)
                       then Just (aid, actor)
                       else Nothing)
  $ concat $ EM.elems $ lprio lvl

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  actorAssocsLvl p (sdungeon s EM.! lid) (sactorD s)

actorList :: (FactionId -> Bool) -> LevelId -> State
          -> [Actor]
actorList p lid s = map snd $ actorAssocs p lid s

actorNotProjAssocsLvl :: (FactionId -> Bool) -> Level -> ActorDict
                      -> [(ActorId, Actor)]
actorNotProjAssocsLvl p lvl actorD =
  mapMaybe (\aid -> let actor = actorD EM.! aid
                    in if not (bproj actor) && p (bfid actor)
                       then Just (aid, actor)
                       else Nothing)
  $ concat $ EM.elems $ lprio lvl

actorNotProjAssocs :: (FactionId -> Bool) -> LevelId -> State
                   -> [(ActorId, Actor)]
actorNotProjAssocs p lid s =
  actorNotProjAssocsLvl p (sdungeon s EM.! lid) (sactorD s)

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
      g (aid, b) = ((aid, b), map f $ EM.keys (binv b) ++ EM.keys (beqp b))
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
               && Tile.isWalkable cotile (lvl `at` p)
               && unoccupied as p
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good ps

-- | Calculate loot's worth for a faction of a given actor.
calculateTotal :: Actor -> State -> (ItemBag, Int)
calculateTotal body s =
  let bag = sharedAllOwned body s
      items = map (\(iid, k) -> (getItemBody iid s, k))
              $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

sharedInv :: Actor -> State -> ItemBag
sharedInv body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith (+) $ map binv $ if null bs then [body] else bs

sharedEqp :: Actor -> State -> ItemBag
sharedEqp body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith (+) $ map beqp $ if null bs then [body] else bs

sharedAllOwned :: Actor -> State -> ItemBag
sharedAllOwned body s = EM.unionWith (+) (sharedInv body s) (sharedEqp body s)

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
        -> Dungeon  -- ^ current game dungeon
        -> (LevelId, Point)
                    -- ^ target level and the position of its receiving stairs
whereTo lid pos k dungeon = assert (k /= 0) $
  let lvl = dungeon EM.! lid
      stairs = (if k < 0 then snd else fst) (lstair lvl)
      defaultStairs = 0  -- for ascending via, e.g., spells
      mindex = elemIndex pos stairs
      i = fromMaybe defaultStairs mindex
  in case ascendInBranch dungeon k lid of
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

updateFactionBody :: FactionId -> (Faction -> Faction) -> State -> State
updateFactionBody fid f s =
  let alt Nothing = assert `failure` "no faction to update" `twith` (fid, s)
      alt (Just fact) = Just $ f fact
  in updateFactionD (EM.alter alt fid) s

allSlots :: [SlotChar]
allSlots = map SlotChar $ ['a'..'z'] ++ ['A'..'Z']

-- | Assigns a slot to an item, for inclusion in the inventory
-- of a hero. Tries to to use the requested slot, if any.
assignSlot :: ItemId -> Maybe SlotChar -> Actor -> ItemSlots -> SlotChar
           -> State
           -> Maybe SlotChar
assignSlot iid r body slots freeSlot s =
  case lookup iid $ map swap $ EM.assocs slots of
    Just l -> Just l
    Nothing -> case r of
      Just l | l `elem` allowed -> Just l
      _ -> listToMaybe free
 where
  candidates = take (length allSlots)
               $ drop (fromJust (elemIndex freeSlot allSlots))
               $ cycle allSlots
  inBag = EM.keysSet $ sharedAllOwned body s
  f l = maybe True (`ES.notMember` inBag) $ EM.lookup l slots
  free = filter f candidates
  allowed = SlotChar '$' : free

actorContainer :: ActorId -> ItemSlots -> ItemId -> SlotChar
actorContainer aid slotChars iid =
  case find ((== iid) . snd) $ EM.assocs slotChars of
    Just (l, _) -> l
    Nothing -> assert `failure` "item not in inventory"
                      `twith` (aid, slotChars, iid)

actorContainerB :: Actor -> ItemSlots -> SlotChar
                -> ItemId -> Item -> State
                -> Maybe SlotChar
actorContainerB body slots freeSlot iid item s =
  case find ((== iid) . snd) $ EM.assocs slots of
    Just (l, _) -> Just l
    Nothing ->
      let l = if jsymbol item == '$' then Just $ SlotChar '$' else Nothing
      in case assignSlot iid l body slots freeSlot s of
        Just l2 -> Just l2
        Nothing -> Nothing

slotLabel :: SlotChar -> MU.Part
slotLabel c = MU.Text $ T.pack $ slotChar c : " -"

getCarriedAssocs :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocs b s =
  let f iid = (iid, getItemBody iid s)
  in map f $ EM.keys $ EM.unionWith (+) (binv b) (beqp b)

getEqpAssocs :: Actor -> State -> [(ItemId, Item)]
getEqpAssocs b s =
  let f iid = (iid, getItemBody iid s)
  in map f $ EM.keys $ beqp b

getInvAssocs :: Actor -> State -> [(ItemId, Item)]
getInvAssocs b s =
  let f iid = (iid, getItemBody iid s)
  in map f $ EM.keys $ getInvBag b s

getInvBag :: Actor -> State -> ItemBag
getInvBag b s =
  let RuleKind{rsharedInventory} = Kind.stdRuleset $ Kind.corule $ scops s
  in if rsharedInventory
     then sharedInv b s
     else binv b

getFloorAssocs :: LevelId -> Point -> State -> [(ItemId, Item)]
getFloorAssocs lid pos s =
  let f iid = (iid, getItemBody iid s)
  in map f $ EM.keys $ sdungeon s EM.! lid `atI` pos

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
