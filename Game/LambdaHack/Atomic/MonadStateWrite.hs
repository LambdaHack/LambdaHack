-- | The monad for writing to the game state and related operations.
module Game.LambdaHack.Atomic.MonadStateWrite
  ( MonadStateWrite(..)
  , updateLevel, updateActor, updateFaction
  , insertItemContainer, insertItemActor, deleteItemContainer, deleteItemActor
  , updatePrio, updateFloor, updateTile, updateSmell
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State

class MonadStateRead m => MonadStateWrite m where
  modifyState :: (State -> State) -> m ()
  putState    :: State -> m ()

-- | Update the actor time priority queue.
updatePrio :: (ActorPrio -> ActorPrio) -> Level -> Level
updatePrio f lvl = lvl {lprio = f (lprio lvl)}

-- | Update the items on the ground map.
updateFloor :: (ItemFloor -> ItemFloor) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

-- | Update the tile map.
updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- | Update a given level data within state.
updateLevel :: MonadStateWrite m => LevelId -> (Level -> Level) -> m ()
updateLevel lid f = modifyState $ updateDungeon $ EM.adjust f lid

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
                    => ItemId -> Int -> Container -> Bool -> m ()
insertItemContainer iid k c isOn = case c of
  CFloor lid pos -> insertItemFloor iid k lid pos isOn
  CActor aid store -> insertItemActor iid k aid store isOn

insertItemFloor :: MonadStateWrite m
                => ItemId -> Int -> LevelId -> Point -> Bool -> m ()
insertItemFloor iid k lid pos isOn =
  let bag = EM.singleton iid (k, isOn)
      mergeBag = EM.insertWith (EM.unionWith (addKCheck)) pos bag
  in updateLevel lid $ updateFloor mergeBag

insertItemActor :: MonadStateWrite m
                => ItemId -> Int -> ActorId -> CStore -> Bool -> m ()
insertItemActor iid k aid cstore isOn = case cstore of
  CGround -> do
    b <- getsState $ getActorBody aid
    insertItemFloor iid k (blid b) (bpos b) isOn
  CEqp -> insertItemEqp iid k aid isOn
  CInv -> insertItemInv iid k aid isOn
  CBody -> insertItemBody iid k aid isOn

insertItemEqp :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
insertItemEqp iid k aid isOn = do
  let bag = EM.singleton iid (k, isOn)
      upd = EM.unionWith (addKCheck) bag
  updateActor aid $ \b -> b {beqp = upd (beqp b)}

insertItemInv :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
insertItemInv iid k aid isOn = do
  let bag = EM.singleton iid (k, isOn)
      upd = EM.unionWith (addKCheck) bag
  updateActor aid $ \b -> b {binv = upd (binv b)}

insertItemBody :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
insertItemBody iid k aid isOn = do
  let bag = EM.singleton iid (k, isOn)
      upd = EM.unionWith (addKCheck) bag
  updateActor aid $ \b -> b {bbody = upd (bbody b)}

addKCheck :: KisOn -> KisOn -> KisOn
addKCheck (k1, kIsOn1) (k2, kIsOn2) =
  assert (kIsOn1 == kIsOn2) (k1 + k2, kIsOn1)

deleteItemContainer :: MonadStateWrite m
                    => ItemId -> Int -> Container -> Bool -> m ()
deleteItemContainer iid k c isOn = case c of
    CFloor lid pos -> deleteItemFloor iid k lid pos isOn
    CActor aid store -> deleteItemActor iid k aid store isOn

deleteItemFloor :: MonadStateWrite m
                => ItemId -> Int -> LevelId -> Point -> Bool -> m ()
deleteItemFloor iid k lid pos isOn =
  let rmFromFloor (Just bag) =
        let nbag = rmFromBag k iid bag isOn
        in if EM.null nbag then Nothing else Just nbag
      rmFromFloor Nothing = assert `failure` "item already removed"
                                   `twith` (iid, k, lid, pos)
  in updateLevel lid $ updateFloor $ EM.alter rmFromFloor pos

deleteItemActor :: MonadStateWrite m
                => ItemId -> Int -> ActorId -> CStore -> Bool -> m ()
deleteItemActor iid k aid cstore isOn = case cstore of
  CGround -> do
    b <- getsState $ getActorBody aid
    deleteItemFloor iid k (blid b) (bpos b) isOn
  CEqp -> deleteItemEqp iid k aid isOn
  CInv -> deleteItemInv iid k aid isOn
  CBody -> deleteItemBody iid k aid isOn

deleteItemEqp :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
deleteItemEqp iid k aid isOn = do
  updateActor aid $ \b -> b {beqp = rmFromBag k iid (beqp b) isOn}

deleteItemInv :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
deleteItemInv iid k aid isOn = do
  updateActor aid $ \b -> b {binv = rmFromBag k iid (binv b) isOn}

deleteItemBody :: MonadStateWrite m => ItemId -> Int -> ActorId -> Bool -> m ()
deleteItemBody iid k aid isOn = do
  updateActor aid $ \b -> b {bbody = rmFromBag k iid (bbody b) isOn}

rmFromBag :: Int -> ItemId -> ItemBag -> Bool -> ItemBag
rmFromBag k iid bag isOn =
  let rfb Nothing = assert `failure` "rm from empty bag" `twith` (k, iid, bag)
      rfb (Just (n, isOnOld)) = assert (isOnOld == isOn) $
        case compare n k of
          LT -> assert `failure` "rm more than there is"
                       `twith` (n, k, iid, bag)
          EQ -> Nothing
          GT -> Just (n - k, isOn)
  in EM.alter rfb iid bag
