-- | The monad for writing to the game state and related operations.
module Game.LambdaHack.Atomic.MonadStateWrite
  ( MonadStateWrite(..)
  , putState, updateLevel, updateActor, updateFaction
  , insertItemContainer, insertItemActor, deleteItemContainer, deleteItemActor
  , updateFloor, updateActorMap, moveActorMap
  , updateTile, updateSmell
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State

class MonadStateRead m => MonadStateWrite m where
  modifyState :: (State -> State) -> m ()

putState :: MonadStateWrite m => State -> m ()
putState s = modifyState (const s)

-- | Update the items on the ground map.
updateFloor :: (ItemFloor -> ItemFloor) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

-- | Update the items embedded in a tile on the level.
updateEmbed :: (ItemFloor -> ItemFloor) -> Level -> Level
updateEmbed f lvl = lvl {lembed = f (lembed lvl)}

-- | Update the actors on the ground map.
updateActorMap :: (ActorMap -> ActorMap) -> Level -> Level
updateActorMap f lvl = lvl {lactor = f (lactor lvl)}

moveActorMap :: MonadStateWrite m => ActorId -> Actor -> Actor -> m ()
moveActorMap aid body newBody = do
  let rmActor Nothing = assert `failure` "actor already removed"
                               `twith` (aid, body)
      rmActor (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `elem` l `blame` "actor already removed"
                             `twith` (aid, body, l))
#endif
        (let l2 = delete aid l
         in if null l2 then Nothing else Just l2)
      addActor Nothing = Just [aid]
      addActor (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `notElem` l `blame` "actor already added"
                                `twith` (aid, body, l))
#endif
        (Just $ aid : l)
      updActor = EM.alter addActor (bpos newBody)
                 . EM.alter rmActor (bpos body)
  updateLevel (blid body) $ updateActorMap updActor

-- | Update the tile map.
updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- INLIning offers no speedup, increases alloc and binary size.
-- EM.alter not necessary, because levels not removed, so little risk
-- of adjusting at absent index.
-- | Update a given level data within state.
updateLevel :: MonadStateWrite m => LevelId -> (Level -> Level) -> m ()
updateLevel lid f = modifyState $ updateDungeon $ EM.adjust f lid

-- INLIning doesn't help despite probably canceling the alt indirection.
-- perhaps it's applied automatically due to INLINABLE.
updateActor :: MonadStateWrite m => ActorId -> (Actor -> Actor) -> m ()
updateActor aid f = do
  let alt Nothing = assert `failure` "no body to update" `twith` aid
      alt (Just b) = Just $ f b
  modifyState $ updateActorD $ EM.alter alt aid

updateFaction :: MonadStateWrite m => FactionId -> (Faction -> Faction) -> m ()
updateFaction fid f = do
  let alt Nothing = assert `failure` "no faction to update" `twith` fid
      alt (Just fact) = Just $ f fact
  modifyState $ updateFactionD $ EM.alter alt fid

insertItemContainer :: MonadStateWrite m
                    => ItemId -> ItemQuant -> Container -> m ()
insertItemContainer iid kit c = case c of
  CFloor lid pos -> insertItemFloor iid kit lid pos
  CEmbed lid pos -> insertItemEmbed iid kit lid pos
  CActor aid store -> insertItemActor iid kit aid store
  CTrunk{} -> return ()

-- New @kit@ lands at the front of the list.
insertItemFloor :: MonadStateWrite m
                => ItemId -> ItemQuant -> LevelId -> Point -> m ()
insertItemFloor iid kit lid pos =
  let bag = EM.singleton iid kit
      mergeBag = EM.insertWith (EM.unionWith mergeItemQuant) pos bag
  in updateLevel lid $ updateFloor mergeBag

insertItemEmbed :: MonadStateWrite m
                => ItemId -> ItemQuant -> LevelId -> Point -> m ()
insertItemEmbed iid kit lid pos =
  let bag = EM.singleton iid kit
      mergeBag = EM.insertWith (EM.unionWith mergeItemQuant) pos bag
  in updateLevel lid $ updateEmbed mergeBag

insertItemActor :: MonadStateWrite m
                => ItemId -> ItemQuant -> ActorId -> CStore -> m ()
insertItemActor iid kit aid cstore = case cstore of
  CGround -> do
    b <- getsState $ getActorBody aid
    insertItemFloor iid kit (blid b) (bpos b)
  COrgan -> insertItemOrgan iid kit aid
  CEqp -> insertItemEqp iid kit aid
  CInv -> insertItemInv iid kit aid
  CSha -> do
    b <- getsState $ getActorBody aid
    insertItemSha iid kit (bfid b)

insertItemOrgan :: MonadStateWrite m
                => ItemId -> ItemQuant -> ActorId -> m ()
insertItemOrgan iid kit aid = do
  let bag = EM.singleton iid kit
      upd = EM.unionWith mergeItemQuant bag
  item <- getsState $ getItemBody iid
  updateActor aid $ \b ->
    b { borgan = upd (borgan b)
      , bweapon = if isMelee item then bweapon b + 1 else bweapon b }

insertItemEqp :: MonadStateWrite m
              => ItemId -> ItemQuant -> ActorId -> m ()
insertItemEqp iid kit aid = do
  let bag = EM.singleton iid kit
      upd = EM.unionWith mergeItemQuant bag
  item <- getsState $ getItemBody iid
  updateActor aid $ \b ->
    b { beqp = upd (beqp b)
      , bweapon = if isMelee item then bweapon b + 1 else bweapon b }

insertItemInv :: MonadStateWrite m
              => ItemId -> ItemQuant -> ActorId -> m ()
insertItemInv iid kit aid = do
  let bag = EM.singleton iid kit
      upd = EM.unionWith mergeItemQuant bag
  updateActor aid $ \b -> b {binv = upd (binv b)}

insertItemSha :: MonadStateWrite m
              => ItemId -> ItemQuant -> FactionId -> m ()
insertItemSha iid kit fid = do
  let bag = EM.singleton iid kit
      upd = EM.unionWith mergeItemQuant bag
  updateFaction fid $ \fact -> fact {gsha = upd (gsha fact)}

deleteItemContainer :: MonadStateWrite m
                    => ItemId -> ItemQuant -> Container -> m ()
deleteItemContainer iid kit c = case c of
  CFloor lid pos -> deleteItemFloor iid kit lid pos
  CEmbed lid pos -> deleteItemEmbed iid kit lid pos
  CActor aid store -> deleteItemActor iid kit aid store
  CTrunk{} -> assert `failure` c

deleteItemFloor :: MonadStateWrite m
                => ItemId -> ItemQuant -> LevelId -> Point -> m ()
deleteItemFloor iid kit lid pos =
  let rmFromFloor (Just bag) =
        let nbag = rmFromBag kit iid bag
        in if EM.null nbag then Nothing else Just nbag
      rmFromFloor Nothing = assert `failure` "item already removed"
                                   `twith` (iid, kit, lid, pos)
  in updateLevel lid $ updateFloor $ EM.alter rmFromFloor pos

deleteItemEmbed :: MonadStateWrite m
                => ItemId -> ItemQuant -> LevelId -> Point -> m ()
deleteItemEmbed iid kit lid pos =
  let rmFromFloor (Just bag) =
        let nbag = rmFromBag kit iid bag
        in if EM.null nbag then Nothing else Just nbag
      rmFromFloor Nothing = assert `failure` "item already removed"
                                   `twith` (iid, kit, lid, pos)
  in updateLevel lid $ updateEmbed $ EM.alter rmFromFloor pos

deleteItemActor :: MonadStateWrite m
                => ItemId -> ItemQuant -> ActorId -> CStore -> m ()
deleteItemActor iid kit aid cstore = case cstore of
  CGround -> do
    b <- getsState $ getActorBody aid
    deleteItemFloor iid kit (blid b) (bpos b)
  COrgan -> deleteItemOrgan iid kit aid
  CEqp -> deleteItemEqp iid kit aid
  CInv -> deleteItemInv iid kit aid
  CSha -> do
    b <- getsState $ getActorBody aid
    deleteItemSha iid kit (bfid b)

deleteItemOrgan :: MonadStateWrite m => ItemId -> ItemQuant -> ActorId -> m ()
deleteItemOrgan iid kit aid = do
  item <- getsState $ getItemBody iid
  updateActor aid $ \b ->
    b { borgan = rmFromBag kit iid (borgan b)
      , bweapon = if isMelee item then bweapon b - 1 else bweapon b }

deleteItemEqp :: MonadStateWrite m => ItemId -> ItemQuant -> ActorId -> m ()
deleteItemEqp iid kit aid = do
  item <- getsState $ getItemBody iid
  updateActor aid $ \b ->
    b { beqp = rmFromBag kit iid (beqp b)
      , bweapon = if isMelee item then bweapon b - 1 else bweapon b }

deleteItemInv :: MonadStateWrite m => ItemId -> ItemQuant -> ActorId -> m ()
deleteItemInv iid kit aid =
  updateActor aid $ \b -> b {binv = rmFromBag kit iid (binv b)}

deleteItemSha :: MonadStateWrite m => ItemId -> ItemQuant -> FactionId -> m ()
deleteItemSha iid kit fid =
  updateFaction fid $ \fact -> fact {gsha = rmFromBag kit iid (gsha fact)}

-- Removing the part of the kit from the back of the list,
-- so that @DestroyItem kit (CreateItem kit x) == x@.
rmFromBag :: ItemQuant -> ItemId -> ItemBag -> ItemBag
rmFromBag kit@(k, rmIt) iid bag =
  let rfb Nothing = assert `failure` "rm from empty slot" `twith` (k, iid, bag)
      rfb (Just (n, it)) =
        case compare n k of
          LT -> assert `failure` "rm more than there is"
                       `twith` (n, kit, iid, bag)
          EQ -> assert (rmIt == it `blame` (rmIt, it, n, kit, iid, bag))
                $ Nothing
          GT -> assert (rmIt == take k it
                        `blame` (rmIt, take k it, n, kit, iid, bag))
                $ Just (n - k, take (n - k) it)
  in EM.alter rfb iid bag
