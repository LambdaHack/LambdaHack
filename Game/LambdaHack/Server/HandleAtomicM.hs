{-# LANGUAGE TupleSections #-}
-- | Handle atomic commands before they are executed to change State
-- and sent to clients.
module Game.LambdaHack.Server.HandleAtomicM
  ( cmdAtomicSemSer
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Fov
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.TileKind (TileKind)
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Effect of atomic actions on server state is calculated
-- with the global state from before the command is executed.
cmdAtomicSemSer :: MonadServer m => UpdAtomic -> m ()
cmdAtomicSemSer cmd = case cmd of
  UpdCreateActor aid b _ -> do
    discoAspect <- getsServer sdiscoAspect
    let aspectRecord = fovAspectFromActor discoAspect b
        f = EM.insert aid aspectRecord
    modifyServer $ \ser -> ser {sactorAspect = f $ sactorAspect ser}
    actorAspect <- getsServer sactorAspect
    -- We don't have the body in the State yet, hence no @invalidateLucidAid@.
    when (actorHasShine actorAspect aid) $ invalidateLucidLid $ blid b
  UpdDestroyActor aid b _ -> do
    actorAspectOld <- getsServer sactorAspect
    when (actorHasShine actorAspectOld aid) $ invalidateLucidLid $ blid b
    let f = EM.delete aid
    modifyServer $ \ser -> ser {sactorAspect = f $ sactorAspect ser}
  UpdCreateItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdCreateItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    addItemToActorIfStore iid k aid store
  UpdDestroyItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdDestroyItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    addItemToActorIfStore iid (-k) aid store
  UpdSpotActor aid b _ -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    actorAspect <- getsServer sactorAspect
    -- We don't have the body in the State yet, hence no @invalidateLucidAid@.
    when (actorHasShine actorAspect aid) $ invalidateLucidLid $ blid b
  UpdLoseActor aid b _ -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    actorAspectOld <- getsServer sactorAspect
    when (actorHasShine actorAspectOld aid) $ invalidateLucidLid $ blid b
  UpdSpotItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdSpotItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    addItemToActorIfStore iid k aid store
  UpdLoseItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdLoseItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    addItemToActorIfStore iid (-k) aid store
  UpdMoveActor aid _ _ -> do
    actorAspect <- getsServer sactorAspect
    when (actorHasShine actorAspect aid) $ invalidateLucidAid aid
  UpdDisplaceActor aid1 aid2 -> do
    actorAspect <- getsServer sactorAspect
    when (actorHasShine actorAspect aid1 || actorHasShine actorAspect aid2) $
      invalidateLucidAid aid1  -- the same lid as aid2
  UpdMoveItem iid k aid s1 s2 -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [s1, s2]) $
      invalidateLucidAid aid
    case s1 of
      CEqp -> case s2 of
        COrgan -> return ()
        _ -> addItemToActor iid (-k) aid
      COrgan -> case s2 of
        CEqp -> return ()
        _ -> addItemToActor iid (-k) aid
      _ -> addItemToActorIfStore iid k aid s2
  UpdAlterTile lid pos fromTile toTile -> do
    clearChanged <- updateSclear lid pos fromTile toTile
    litChanged <- updateSlit lid pos fromTile toTile
    when (clearChanged || litChanged) $ invalidateLucidLid lid
  _ -> return ()

addItemToActorIfStore :: MonadServer m
                      => ItemId -> Int -> ActorId -> CStore -> m ()
addItemToActorIfStore iid k aid store =
  when (store `elem` [CEqp, COrgan]) $ addItemToActor iid k aid

addItemToActor :: MonadServer m
               => ItemId -> Int -> ActorId -> m ()
addItemToActor iid k aid = do
  discoAspect <- getsServer sdiscoAspect
  let arItem = discoAspect EM.! iid
      g arActor = sumAspectRecord [(arActor, 1), (arItem, k)]
      f = EM.adjust g aid
  modifyServer $ \ser -> ser {sactorAspect = f $ sactorAspect ser}

updateSclear :: MonadServer m
             => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind -> m Bool
updateSclear lid pos fromTile toTile = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let fromClear = Tile.isClear coTileSpeedup fromTile
      toClear = Tile.isClear coTileSpeedup toTile
  if fromClear == toClear then return False else do
    let f FovClear{fovClear} =
          FovClear $ fovClear PointArray.// [(pos, toClear)]
    modifyServer $ \ser ->
      ser {sfovClearLid = EM.adjust f lid $ sfovClearLid ser}
    return True

updateSlit :: MonadServer m
           => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind -> m Bool
updateSlit lid pos fromTile toTile = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let fromLit = Tile.isLit coTileSpeedup fromTile
      toLit = Tile.isLit coTileSpeedup toTile
  if fromLit == toLit then return False else do
    let f (FovLit set) =
          FovLit $ if toLit then ES.insert pos set else ES.delete pos set
    modifyServer $ \ser -> ser {sfovLitLid = EM.adjust f lid $ sfovLitLid ser}
    return True

invalidateLucidLid :: MonadServer m => LevelId -> m ()
invalidateLucidLid lid =
  modifyServer $ \ser ->
    ser {sfovLucidLid = EM.insert lid FovInvalid $ sfovLucidLid ser}

invalidateLucidAid :: MonadServer m => ActorId  -> m ()
invalidateLucidAid aid = do
  lid <- getsState $ blid . getActorBody aid
  invalidateLucidLid lid

actorHasShine :: ActorAspect -> ActorId -> Bool
actorHasShine actorAspect aid = case EM.lookup aid actorAspect of
  Just AspectRecord{aShine} -> aShine > 0
  Nothing -> assert `failure` aid

itemAffectsShineRadius :: DiscoveryAspect -> ItemId -> [CStore] -> Bool
itemAffectsShineRadius discoAspect iid stores =
  (null stores || (not $ null $ intersect stores [CEqp, COrgan, CGround]))
  && case EM.lookup iid discoAspect of
    Just AspectRecord{aShine} -> aShine /= 0
    Nothing -> False
