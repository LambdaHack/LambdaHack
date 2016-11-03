{-# LANGUAGE TupleSections #-}
-- | Handle atomic commands before they are executed to change State
-- and sent to clients.
module Game.LambdaHack.Server.HandleAtomicM
  ( cmdAtomicSemSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , addItemToActor, updateSclear, updateSlit
  , invalidateLucidLid, invalidateLucidAid
  , actorHasShine, itemAffectsShineRadius, itemAffectsPerRadius
  , addPerActor, addPerActorAny, deletePerActor, deletePerActorAny
  , invalidatePerActor, reconsiderPerActor, invalidatePerLid
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.TileKind (TileKind)
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Effect of atomic actions on server state is calculated
-- with the global state from before the command is executed.
cmdAtomicSemSer :: MonadServer m => UpdAtomic -> m ()
cmdAtomicSemSer cmd = case cmd of
  UpdCreateActor aid b _ -> do
    discoAspect <- getsServer sdiscoAspect
    let aspectRecord = aspectRecordFromActorServer discoAspect b
        f = EM.insert aid aspectRecord
    modifyServer $ \ser -> ser {sactorAspect = f $ sactorAspect ser}
    actorAspect <- getsServer sactorAspect
    -- We don't have the body in the State yet, hence no @invalidateLucidAid@.
    when (actorHasShine actorAspect aid) $ invalidateLucidLid $ blid b
    addPerActor aid b
  UpdDestroyActor aid b _ -> do
    deletePerActor aid b
    actorAspectOld <- getsServer sactorAspect
    when (actorHasShine actorAspectOld aid) $ invalidateLucidLid $ blid b
    modifyServer $ \ser -> ser {sactorAspect = EM.delete aid $ sactorAspect ser}
    modifyServer $ \ser ->
      ser {sactorTime = EM.adjust (EM.delete aid) (blid b) $ sactorTime ser}
  UpdCreateItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdCreateItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    when (store `elem` [CEqp, COrgan]) $ do
      addItemToActor iid k aid
      when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdDestroyItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdDestroyItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    when (store `elem` [CEqp, COrgan]) $ do
      addItemToActor iid (-k) aid
      when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdSpotActor aid b _ -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    actorAspect <- getsServer sactorAspect
    -- We don't have the body in the State yet, hence no @invalidateLucidAid@.
    when (actorHasShine actorAspect aid) $ invalidateLucidLid $ blid b
    addPerActor aid b
  UpdLoseActor aid b _ -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    deletePerActor aid b
    actorAspectOld <- getsServer sactorAspect
    when (actorHasShine actorAspectOld aid) $ invalidateLucidLid $ blid b
    modifyServer $ \ser ->
      ser {sactorTime = EM.adjust (EM.delete aid) (blid b) $ sactorTime ser}
  UpdSpotItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdSpotItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    when (store `elem` [CEqp, COrgan]) $ do
      addItemToActor iid k aid
      when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdLoseItem iid _ _ (CFloor lid _) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid []) $ invalidateLucidLid lid
  UpdLoseItem iid _ (k, _) (CActor aid store) -> do
    discoAspect <- getsServer sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid [store]) $
      invalidateLucidAid aid
    when (store `elem` [CEqp, COrgan]) $ do
      addItemToActor iid (-k) aid
      when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdMoveActor aid _ _ -> do
    actorAspect <- getsServer sactorAspect
    when (actorHasShine actorAspect aid) $ invalidateLucidAid aid
    invalidatePerActor aid
  UpdDisplaceActor aid1 aid2 -> do
    actorAspect <- getsServer sactorAspect
    when (actorHasShine actorAspect aid1 || actorHasShine actorAspect aid2) $
      invalidateLucidAid aid1  -- the same lid as aid2
    invalidatePerActor aid1
    invalidatePerActor aid2
  UpdMoveItem iid k aid s1 s2 -> do
    discoAspect <- getsServer sdiscoAspect
    let itemAffectsPer = itemAffectsPerRadius discoAspect iid
        invalidatePer = when itemAffectsPer $ reconsiderPerActor aid
        itemAffectsShine = itemAffectsShineRadius discoAspect iid [s1, s2]
        invalidateLucid = when itemAffectsShine $ invalidateLucidAid aid
    case s1 of
      CEqp -> case s2 of
        COrgan -> return ()
        _ -> do
          addItemToActor iid (-k) aid
          invalidatePer
          invalidateLucid
      COrgan -> case s2 of
        CEqp -> return ()
        _ -> do
          addItemToActor iid (-k) aid
          invalidatePer
          invalidateLucid
      _ -> do
        when (s2 `elem` [CEqp, COrgan]) $ do
          addItemToActor iid k aid
          invalidatePer
        invalidateLucid  -- from itemAffects, s2 provides light or s1 is CGround
  UpdRefillCalm aid n -> do
    actorAspect <- getsServer sactorAspect
    body <- getsState $ getActorBody aid
    let AspectRecord{aSight} = actorAspect EM.! aid
        radiusOld = boundSightByCalm aSight (bcalm body)
        radiusNew = boundSightByCalm aSight (bcalm body + n)
    when (radiusOld /= radiusNew) $ invalidatePerActor aid
  UpdAlterTile lid pos fromTile toTile -> do
    clearChanged <- updateSclear lid pos fromTile toTile
    litChanged <- updateSlit lid pos fromTile toTile
    when (clearChanged || litChanged) $ invalidateLucidLid lid
    when clearChanged $ invalidatePerLid lid
  _ -> return ()

addItemToActor :: MonadServer m => ItemId -> Int -> ActorId -> m ()
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
    ser { sfovLucidLid = EM.insert lid FovInvalid $ sfovLucidLid ser
        , sperValidFid = EM.map (EM.insert lid False) $ sperValidFid ser }

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
    Nothing -> assert `failure` iid

itemAffectsPerRadius :: DiscoveryAspect -> ItemId -> Bool
itemAffectsPerRadius discoAspect iid =
  case EM.lookup iid discoAspect of
    Just AspectRecord{aSight, aSmell, aNocto} ->
      aSight /= 0 || aSmell /= 0 || aNocto /= 0
    Nothing -> assert `failure` iid

addPerActor :: MonadServer m => ActorId -> Actor -> m ()
addPerActor aid b = do
  actorAspect <- getsServer sactorAspect
  let AspectRecord{..} = actorAspect EM.! aid
  unless (aSight <= 0 && aNocto <= 0 && aSmell <= 0) $ addPerActorAny aid b

addPerActorAny :: MonadServer m => ActorId -> Actor -> m ()
addPerActorAny aid b = do
  let fid = bfid b
      lid = blid b
      f PerceptionCache{perActor} = PerceptionCache
        { ptotal = FovInvalid
        , perActor = EM.insert aid FovInvalid perActor }
  modifyServer $ \ser ->
    ser { sperCacheFid = EM.adjust (EM.adjust f lid) fid $ sperCacheFid ser
        , sperValidFid = EM.adjust (EM.insert lid False) fid
                         $ sperValidFid ser }

deletePerActor :: MonadServer m => ActorId -> Actor -> m ()
deletePerActor aid b = do
  actorAspect <- getsServer sactorAspect
  let AspectRecord{..} = actorAspect EM.! aid
  unless (aSight <= 0 && aNocto <= 0 && aSmell <= 0) $ deletePerActorAny aid b

deletePerActorAny :: MonadServer m => ActorId -> Actor -> m ()
deletePerActorAny aid b = do
  let fid = bfid b
      lid = blid b
      f PerceptionCache{perActor} = PerceptionCache
        { ptotal = FovInvalid
        , perActor = EM.delete aid perActor }
  modifyServer $ \ser ->
    ser { sperCacheFid = EM.adjust (EM.adjust f lid) fid $ sperCacheFid ser
        , sperValidFid = EM.adjust (EM.insert lid False) fid
                         $ sperValidFid ser }

invalidatePerActor :: MonadServer m => ActorId -> m ()
invalidatePerActor aid = do
  actorAspect <- getsServer sactorAspect
  let AspectRecord{..} = actorAspect EM.! aid
  unless (aSight <= 0 && aNocto <= 0 && aSmell <= 0) $ do
    b <- getsState $ getActorBody aid
    addPerActorAny aid b

reconsiderPerActor :: MonadServer m => ActorId -> m ()
reconsiderPerActor aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsServer sactorAspect
  let AspectRecord{..} = actorAspect EM.! aid
  if aSight <= 0 && aNocto <= 0 && aSmell <= 0
  then do
    perCacheFid <- getsServer sperCacheFid
    when (EM.member aid $ perActor ((perCacheFid EM.! bfid b) EM.! blid b)) $
      deletePerActorAny aid b
  else addPerActorAny aid b

invalidatePerLid :: MonadServer m => LevelId -> m ()
invalidatePerLid lid = do
  let f pc@PerceptionCache{perActor}
        | EM.null perActor = pc
        | otherwise = PerceptionCache
          { ptotal = FovInvalid
          , perActor = EM.map (const FovInvalid) perActor }
  modifyServer $ \ser ->
    let perCacheFidNew = EM.map (EM.adjust f lid) $ sperCacheFid ser
        g fid valid |
          ptotal ((perCacheFidNew EM.! fid) EM.! lid) == FovInvalid =
          EM.insert lid False valid
        g _ valid = valid
    in ser { sperCacheFid = perCacheFidNew
           , sperValidFid = EM.mapWithKey g $ sperValidFid ser }
