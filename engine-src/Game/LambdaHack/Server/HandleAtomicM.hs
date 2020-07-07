-- | Handle atomic commands on the server, after they are executed
-- to change server 'State' and before they are sent to clients.
module Game.LambdaHack.Server.HandleAtomicM
  ( cmdAtomicSemSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateFloor, validateFloorBag, levelOfStash
  , invalidateArenas, updateSclear, updateSlit
  , invalidateLucidLid, invalidateLucidAid
  , actorHasShine, itemAffectsShineRadius, itemAffectsPerRadius
  , addPerActor, addPerActorAny, deletePerActor, deletePerActorAny
  , invalidatePerActor, reconsiderPerActor, invalidatePerLid
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.Fov
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.State

-- | Effect of atomic actions on server state is calculated
-- with the global state from after the command is executed
-- (except where the supplied @oldState@ is used).
cmdAtomicSemSer :: MonadServer m => State -> UpdAtomic -> m ()
cmdAtomicSemSer oldState cmd = case cmd of
  UpdRegisterItems{} -> return ()
  UpdCreateActor aid b _ -> do
    actorMaxSkills <- getsState sactorMaxSkills
    when (actorHasShine actorMaxSkills aid) $ invalidateLucidLid $ blid b
    addPerActor aid b
  UpdDestroyActor aid b _ -> do
    let actorMaxSkillsOld = sactorMaxSkills oldState
    when (actorHasShine actorMaxSkillsOld aid) $ invalidateLucidLid $ blid b
    deletePerActor actorMaxSkillsOld aid b
    modifyServer $ \ser ->
      ser { sactorTime = EM.adjust (EM.adjust (EM.delete aid) (blid b)) (bfid b)
                                   (sactorTime ser)
          , strajTime = EM.adjust (EM.adjust (EM.delete aid) (blid b)) (bfid b)
                                  (strajTime ser)
          , strajPushedBy = EM.delete aid (strajPushedBy ser)
          , sactorAn = EM.delete aid (sactorAn ser)
          , sactorStasis = ES.delete aid (sactorStasis ser) }
  UpdCreateItem _ iid _ _ (CFloor lid _) -> validateFloor iid lid
  UpdCreateItem _ iid _ _ (CActor aid CStash) -> do
    lid <- levelOfStash aid
    validateFloor iid lid
  UpdCreateItem _ iid _ _ (CActor aid CGround) -> do
    lid <- getsState $ blid . getActorBody aid
    validateFloor iid lid
  UpdCreateItem _ iid _ _ (CActor aid _) -> do
    discoAspect <- getsState sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid) $
      invalidateLucidAid aid
    when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdCreateItem{} -> return ()
  UpdDestroyItem _ iid _ _ (CFloor lid _) -> validateFloor iid lid
  UpdDestroyItem _ iid _ _  (CActor aid CStash) -> do
    lid <- levelOfStash aid
    validateFloor iid lid
  UpdDestroyItem _ iid _ _ (CActor aid CGround) -> do
    lid <- getsState $ blid . getActorBody aid
    validateFloor iid lid
  UpdDestroyItem _ iid _ _ (CActor aid _) -> do
    discoAspect <- getsState sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid) $
      invalidateLucidAid aid
    when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdDestroyItem{} -> return ()
  UpdSpotActor aid b -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    actorMaxSkills <- getsState sactorMaxSkills
    when (actorHasShine actorMaxSkills aid) $ invalidateLucidLid $ blid b
    addPerActor aid b
  UpdLoseActor aid b -> do
    -- On server, it does't affect aspects, but does affect lucid (Ascend).
    let actorMaxSkillsOld = sactorMaxSkills oldState
    when (actorHasShine actorMaxSkillsOld aid) $ invalidateLucidLid $ blid b
    deletePerActor actorMaxSkillsOld aid b
    modifyServer $ \ser ->
      ser { sactorTime = EM.adjust (EM.adjust (EM.delete aid) (blid b)) (bfid b)
                                   (sactorTime ser)
          , strajTime = EM.adjust (EM.adjust (EM.delete aid) (blid b)) (bfid b)
                                  (strajTime ser)
          , strajPushedBy = EM.delete aid (strajPushedBy ser)
          , sactorAn = EM.delete aid (sactorAn ser)
          , sactorStasis = ES.delete aid (sactorStasis ser) }
  UpdSpotItem _ iid _ (CFloor lid _) -> validateFloor iid lid
  UpdSpotItem _ iid _  (CActor aid CStash) -> do
    lid <- levelOfStash aid
    validateFloor iid lid
  UpdSpotItem _ iid _ (CActor aid CGround) -> do
    lid <- getsState $ blid . getActorBody aid
    validateFloor iid lid
  UpdSpotItem _ iid _ (CActor aid _) -> do
    discoAspect <- getsState sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid) $
      invalidateLucidAid aid
    when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdSpotItem{} -> return ()
  UpdLoseItem _ iid _ (CFloor lid _) -> validateFloor iid lid
  UpdLoseItem _ iid _ (CActor aid CStash) -> do
    lid <- levelOfStash aid
    validateFloor iid lid
  UpdLoseItem _ iid _ (CActor aid CGround) -> do
    lid <- getsState $ blid . getActorBody aid
    validateFloor iid lid
  UpdLoseItem _ iid _ (CActor aid _) -> do
    discoAspect <- getsState sdiscoAspect
    when (itemAffectsShineRadius discoAspect iid) $
      invalidateLucidAid aid
    when (itemAffectsPerRadius discoAspect iid) $ reconsiderPerActor aid
  UpdLoseItem{} -> return ()
  UpdSpotItemBag (CFloor lid _) bag  -> validateFloorBag bag lid
  UpdSpotItemBag (CActor aid CStash) bag -> do
    lid <- levelOfStash aid
    validateFloorBag bag lid
  UpdSpotItemBag (CActor aid CGround) bag -> do
    lid <- getsState $ blid . getActorBody aid
    validateFloorBag bag lid
  UpdSpotItemBag (CActor aid _) bag -> do
    discoAspect <- getsState sdiscoAspect
    let iids = EM.keys bag
    when (any (itemAffectsShineRadius discoAspect) iids) $
      invalidateLucidAid aid
    when (any (itemAffectsPerRadius discoAspect) iids) $
      reconsiderPerActor aid
  UpdSpotItemBag{} -> return ()
  UpdLoseItemBag (CFloor lid _) bag -> validateFloorBag bag lid
  UpdLoseItemBag (CActor aid CStash) bag -> do
    lid <- levelOfStash aid
    validateFloorBag bag lid
  UpdLoseItemBag (CActor aid CGround) bag -> do
    lid <- levelOfStash aid
    validateFloorBag bag lid
  UpdLoseItemBag (CActor aid _) bag -> do
    discoAspect <- getsState sdiscoAspect
    let iids = EM.keys bag
    when (any (itemAffectsShineRadius discoAspect) iids) $
      invalidateLucidAid aid
    when (any (itemAffectsPerRadius discoAspect) iids) $
      reconsiderPerActor aid
  UpdLoseItemBag{} -> return ()
  UpdMoveActor aid _ _ -> do
    actorMaxSkills <- getsState sactorMaxSkills
    when (actorHasShine actorMaxSkills aid) $ invalidateLucidAid aid
    invalidatePerActor aid
  UpdWaitActor{} -> return ()
  UpdDisplaceActor aid1 aid2 -> do
    actorMaxSkills <- getsState sactorMaxSkills
    when (actorHasShine actorMaxSkills aid1
          || actorHasShine actorMaxSkills aid2) $
      invalidateLucidAid aid1  -- the same lid as aid2
    invalidatePerActor aid1
    invalidatePerActor aid2
  UpdMoveItem iid _k aid s1 s2 -> do
    let dummyVerbose = False
        dummyKit = quantSingle
    cmdAtomicSemSer oldState $
      UpdLoseItem dummyVerbose iid dummyKit (CActor aid s1)
    cmdAtomicSemSer oldState $
      UpdSpotItem dummyVerbose iid dummyKit (CActor aid s2)
  UpdRefillHP{} -> return ()
  UpdRefillCalm aid _ -> do
    actorMaxSk <- getsState $ getActorMaxSkills aid
    body <- getsState $ getActorBody aid
    let sight = Ability.getSk Ability.SkSight actorMaxSk
        oldBody = getActorBody aid oldState
        radiusOld = boundSightByCalm sight (bcalm oldBody)
        radiusNew = boundSightByCalm sight (bcalm body)
    when (radiusOld /= radiusNew) $ invalidatePerActor aid
  UpdTrajectory{} -> return ()
  UpdQuitFaction{} -> return ()
  UpdSpotStashFaction _ fid lid _ -> invalidatePerFidLid fid lid
  UpdLoseStashFaction _ fid lid _ -> invalidatePerFidLid fid lid
  UpdLeadFaction{} -> invalidateArenas
  UpdDiplFaction{} -> return ()
  UpdDoctrineFaction{} -> return ()
  UpdAutoFaction{} -> return ()
  UpdRecordKill{} -> invalidateArenas
  UpdAlterTile lid pos fromTile toTile -> do
    clearChanged <- updateSclear lid pos fromTile toTile
    litChanged <- updateSlit lid pos fromTile toTile
    when (clearChanged || litChanged) $ invalidateLucidLid lid
    when clearChanged $ invalidatePerLid lid
  UpdAlterExplorable{} -> return ()
  UpdAlterGold{} -> return ()
  UpdSearchTile{} -> return ()
  UpdHideTile{} -> return ()
  UpdSpotTile{} -> return ()
  UpdLoseTile{} -> return ()
  UpdSpotEntry{} -> return ()
  UpdLoseEntry{} -> return ()
  UpdAlterSmell{} -> return ()
  UpdSpotSmell{} -> return ()
  UpdLoseSmell{} -> return ()
  UpdTimeItem{} -> return ()
  UpdAgeGame{} -> return ()
  UpdUnAgeGame{} -> return ()
  UpdDiscover{} -> return ()
  UpdCover{} -> return ()
  UpdDiscoverKind{} -> return ()
  UpdCoverKind{} -> return ()
  UpdDiscoverAspect{} -> return ()
  UpdCoverAspect{} -> return ()
  UpdDiscoverServer{} -> return ()
  UpdCoverServer{} -> return ()
  UpdPerception{} -> return ()
  UpdRestart{} -> return ()
  UpdRestartServer{} -> return ()
  UpdResume{} -> return ()
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> return ()
  UpdWriteSave{} -> return ()
  UpdHearFid{} -> return ()

validateFloor :: MonadServer m => ItemId -> LevelId -> m ()
validateFloor iid lid = do
  discoAspect <- getsState sdiscoAspect
  when (itemAffectsShineRadius discoAspect iid) $ invalidateLucidLid lid

validateFloorBag :: MonadServer m => ItemBag -> LevelId -> m ()
validateFloorBag bag lid = do
  discoAspect <- getsState sdiscoAspect
  let iids = EM.keys bag
  when (any (itemAffectsShineRadius discoAspect) iids) $
    invalidateLucidLid lid

levelOfStash :: MonadStateRead m => ActorId -> m LevelId
levelOfStash aid = do
  b <- getsState $ getActorBody aid
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  case mstash of
    Just (lid, _) -> return lid
    Nothing -> error $ "" `showFailure` (aid, b)

invalidateArenas :: MonadServer m => m ()
invalidateArenas = modifyServer $ \ser -> ser {svalidArenas = False}

updateSclear :: MonadServer m
             => LevelId -> Point -> ContentId TileKind -> ContentId TileKind
             -> m Bool
updateSclear lid pos fromTile toTile = do
  COps{coTileSpeedup} <- getsState scops
  let fromClear = Tile.isClear coTileSpeedup fromTile
      toClear = Tile.isClear coTileSpeedup toTile
  if fromClear == toClear then return False else do
    let f FovClear{fovClear} =
          FovClear $ fovClear PointArray.// [(pos, toClear)]
    modifyServer $ \ser ->
      ser {sfovClearLid = EM.adjust f lid $ sfovClearLid ser}
    return True

updateSlit :: MonadServer m
           => LevelId -> Point -> ContentId TileKind -> ContentId TileKind
           -> m Bool
updateSlit lid pos fromTile toTile = do
  COps{coTileSpeedup} <- getsState scops
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

invalidateLucidAid :: MonadServer m => ActorId -> m ()
invalidateLucidAid aid = do
  lid <- getsState $ blid . getActorBody aid
  invalidateLucidLid lid

actorHasShine :: ActorMaxSkills -> ActorId -> Bool
actorHasShine actorMaxSkills aid = case EM.lookup aid actorMaxSkills of
  Just actorMaxSk -> Ability.getSk Ability.SkShine actorMaxSk > 0
  Nothing -> error $ "" `showFailure` aid

itemAffectsShineRadius :: DiscoveryAspect -> ItemId -> Bool
itemAffectsShineRadius discoAspect iid = case EM.lookup iid discoAspect of
  Just arItem -> IA.getSkill Ability.SkShine arItem /= 0
  Nothing -> error $ "" `showFailure` iid

itemAffectsPerRadius :: DiscoveryAspect -> ItemId -> Bool
itemAffectsPerRadius discoAspect iid =
  case EM.lookup iid discoAspect of
    Just arItem -> IA.getSkill Ability.SkSight arItem /= 0
               || IA.getSkill Ability.SkSmell arItem /= 0
               || IA.getSkill Ability.SkNocto arItem /= 0
    Nothing -> error $ "" `showFailure` iid

addPerActor :: MonadServer m => ActorId -> Actor -> m ()
addPerActor aid b = do
  actorMaxSk <- getsState $ getActorMaxSkills aid
  unless (Ability.getSk Ability.SkSight actorMaxSk <= 0
          && Ability.getSk Ability.SkNocto actorMaxSk <= 0
          && Ability.getSk Ability.SkSmell actorMaxSk <= 0) $
    addPerActorAny aid b

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

deletePerActor :: MonadServer m => ActorMaxSkills -> ActorId -> Actor -> m ()
deletePerActor actorMaxSkillsOld aid b = do
  let actorMaxSk = actorMaxSkillsOld EM.! aid
  unless (Ability.getSk Ability.SkSight actorMaxSk <= 0
          && Ability.getSk Ability.SkNocto actorMaxSk <= 0
          && Ability.getSk Ability.SkSmell actorMaxSk <= 0) $
    deletePerActorAny aid b

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
  actorMaxSk <- getsState $ getActorMaxSkills aid
  unless (Ability.getSk Ability.SkSight actorMaxSk <= 0
          && Ability.getSk Ability.SkNocto actorMaxSk <= 0
          && Ability.getSk Ability.SkSmell actorMaxSk <= 0) $ do
    b <- getsState $ getActorBody aid
    addPerActorAny aid b

reconsiderPerActor :: MonadServer m => ActorId -> m ()
reconsiderPerActor aid = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  if Ability.getSk Ability.SkSight actorMaxSk <= 0
     && Ability.getSk Ability.SkNocto actorMaxSk <= 0
     && Ability.getSk Ability.SkSmell actorMaxSk <= 0
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

invalidatePerFidLid :: MonadServer m => FactionId -> LevelId -> m ()
invalidatePerFidLid fid lid = do
  let adj = EM.insert lid False
  modifyServer $ \ser ->
    ser {sperValidFid = EM.adjust adj fid $ sperValidFid ser}
