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
  UpdDestroyActor aid _ _ -> do
    let f = EM.delete aid
    modifyServer $ \ser -> ser {sactorAspect = f $ sactorAspect ser}
  UpdCreateItem iid _ (k, _) (CActor aid store) ->
    addItemToActorIfStore iid k aid store
  UpdDestroyItem iid _ (k, _) (CActor aid store) ->
    addItemToActorIfStore iid (-k) aid store
  -- UpdSpotActor{} -> return ()  -- on server, does't affect aspects
  -- UpdLoseActor{} -> return ()  -- on server, does't affect aspects
  UpdSpotItem iid _ (k, _) (CActor aid store) ->
    addItemToActorIfStore iid k aid store
  UpdLoseItem iid _ (k, _) (CActor aid store) ->
    addItemToActorIfStore iid (-k) aid store
  UpdMoveItem iid k aid s1 s2 ->
    case s1 of
      CEqp -> case s2 of
        COrgan -> return ()
        _ -> addItemToActor iid (-k) aid
      COrgan -> case s2 of
        CEqp -> return ()
        _ -> addItemToActor iid (-k) aid
      _ -> addItemToActorIfStore iid k aid s2
  UpdAlterTile lid pos _ toTile -> do
    updateSclear lid pos toTile
    updateSlit lid pos toTile
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

updateSclear :: MonadServer m => LevelId -> Point -> Kind.Id TileKind -> m ()
updateSclear lid pos toTile = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let f = (PointArray.// [(pos, Tile.isClear coTileSpeedup toTile)])
  modifyServer $ \ser -> ser {sfovClearLid = EM.adjust f lid $ sfovClearLid ser}

updateSlit :: MonadServer m => LevelId -> Point -> Kind.Id TileKind -> m ()
updateSlit lid pos toTile = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let f (FovLit set) = FovLit $ if Tile.isLit coTileSpeedup toTile
                                then ES.insert pos set
                                else ES.delete pos set
  modifyServer $ \ser -> ser {sfovLitLid = EM.adjust f lid $ sfovLitLid ser}
