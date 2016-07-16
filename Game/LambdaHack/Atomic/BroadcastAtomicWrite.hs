-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.BroadcastAtomicWrite
  ( handleAndBroadcast
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Atomic.PosAtomicRead
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Fov
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

-- TODO: split into simpler pieces

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- TODO: undoCmdAtomic atomic

handleCmdAtomicServer :: forall m. MonadStateWrite m
                      => PosAtomic -> CmdAtomic -> m ()
handleCmdAtomicServer posAtomic atomic =
  when (seenAtomicSer posAtomic) $
--    storeUndo atomic
    handleCmdAtomic atomic

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: forall m. MonadStateWrite m
                   => Bool -> PerFid -> PerCacheFid -> m FovAspectItem
                   -> FovAspectActor -> FovLucidLid -> FovClearLid -> FovLitLid
                   -> ((PerFid -> PerFid) -> m ())
                   -> ((PerCacheFid -> PerCacheFid) -> m ())
                   -> (FovAspectActor -> FovLucidLid
                       -> FovClearLid -> FovLitLid
                       -> m ())
                   -> (FactionId -> ResponseAI -> m ())
                   -> (FactionId -> ResponseUI -> m ())
                   -> CmdAtomic
                   -> m ()
handleAndBroadcast knowEvents sperFidOld sperCacheFidOld getFovAspectItem
                   fovAspectActorOld fovLucidLidOld fovClearLidOld fovLitLidOld
                   doUpdatePerFid doUpdatePerCacheFid doUpdateLight
                   doSendUpdateAI doSendUpdateUI atomic = do
  -- Gather data from the old state.
  sOld <- getState
  factionDold <- getsState sfactionD
  (ps, atomicBroken, psBroken) <-
    case atomic of
      UpdAtomic cmd -> do
        ps <- posUpdAtomic cmd
        atomicBroken <- breakUpdAtomic cmd
        psBroken <- mapM posUpdAtomic atomicBroken
        return ( ps, map UpdAtomic atomicBroken, psBroken )
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        atomicBroken <- breakSfxAtomic sfx
        psBroken <- mapM posSfxAtomic atomicBroken
        return ( ps, map SfxAtomic atomicBroken, psBroken )
  -- Perform the action on the server.
  handleCmdAtomicServer ps atomic
  fovAspectItem <- getFovAspectItem
  let (resetsFov, resetsLucidC, resetsTiles, resetsAspectActor) =
        case atomic of
          UpdAtomic cmd ->
            ( resetsFovCmdAtomic cmd fovAspectItem
            , resetsLucidCmdAtomic cmd fovAspectItem
            , resetsTilesCmdAtomic cmd
            , resetsAspectActorCmdAtomic cmd fovAspectItem )
          SfxAtomic{} -> (Just [], \_ _ ->  Right [], Nothing, Nothing)
  -- Update lights in the dungeon. This is not needed and not performed
  -- in particular if not @resets@.
  -- This is needed every (even enemy) move to show thrown torches.
  -- We need to update lights even if cmd doesn't change any perception,
  -- so that for next cmd that does, but doesn't change lights,
  -- and operates on the same level, the lights are up to date.
  -- We could make lights lazy to ensure no computation is wasted,
  -- but it's rare that cmd changed them, but not the perception
  -- (e.g., earthquake in an uninhabited corner of the active arena,
  -- but the we'd probably want some feedback, at least sound).
  fovAspectActor <- case resetsAspectActor of
    Nothing -> return fovAspectActorOld
    Just aid ->
      getsState $ updateFovAspectActor fovAspectActorOld aid fovAspectItem
  let resetsLucid = resetsLucidC fovAspectActorOld fovAspectActor
      resetsOthers = resetsLucid /= Right []
                     || isJust resetsTiles
                     || isJust resetsAspectActor
  -- TODO: assert also that the sum of psBroken is equal to ps;
  -- with deep equality these assertions can be expensive; optimize.
  let !_A = assert (case ps of
                      PosSight{} -> True
                      PosFidAndSight{} -> True
                      PosFidAndSer (Just _) _ -> True
                      _ -> not $ resetsFov /= Just [] || resetsOthers
                   `blame`
                    ( ps, resetsFov
                    , resetsLucid, resetsTiles, resetsAspectActor )) ()
  -- Invariant: if the various resets determine we do not need to update FOV,
  -- perception (@psight@ to be precise, @psmell@ is irrelevant)
  -- of any faction does not change upon full recomputation. Otherwise,
  -- save/restore would change game state (see also the assertions in gameExit).
  fovClearLid <- case resetsTiles of
    Nothing -> return fovClearLidOld
    Just lid -> getsState $ updateFovClear fovClearLidOld lid
  fovLitLid <- case resetsTiles of
    Nothing -> return fovLitLidOld
    Just lid -> getsState $ updateFovLit fovLitLidOld lid
  let updLucid lid = getsState $ updateFovLucid fovLucidLidOld lid
                                                fovAspectItem fovAspectActor
                                                fovClearLid fovLitLid
  fovLucidLid <- case resetsLucid of
    Right [] -> return fovLucidLidOld
    Right (aid : aids) -> do
      lid <- getsState $ blid . getActorBody aid
      lids <- mapM (\a -> getsState $ blid . getActorBody a) aids
      let !_A = assert (all (== lid) lids) ()
      updLucid lid
    Left lid -> updLucid lid
  doUpdateLight fovAspectActor fovLucidLid fovClearLid fovLitLid
  -- Send some actions to the clients, one faction at a time.
  let sendUI fid cmdUI = when (fhasUI $ gplayer $ factionDold EM.! fid) $
        doSendUpdateUI fid cmdUI
      sendAI = doSendUpdateAI
      sendA fid cmd = do
        sendUI fid $ RespUpdAtomicUI cmd
        sendAI fid $ RespUpdAtomicAI cmd
      sendUpdate fid (UpdAtomic cmd) = sendA fid cmd
      sendUpdate fid (SfxAtomic sfx) = sendUI fid $ RespSfxAtomicUI sfx
      breakSend lid fid perNew = do
        let send2 (atomic2, ps2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid atomic2
                else do
                  -- We take the new leader, from after cmd execution.
                  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
                  case (atomic2, mleader) of
                    (UpdAtomic cmd, Just (leader, _)) -> do
                      body <- getsState $ getActorBody leader
                      loud <- loudUpdAtomic (blid body == lid) fid cmd
                      case loud of
                        Nothing -> return ()
                        Just msg -> sendUpdate fid $ SfxAtomic $ SfxMsgAll msg
                    _ -> return ()
        mapM_ send2 $ zip atomicBroken psBroken
      anySend lid fid perOld perNew = do
        let startSeen = seenAtomicCli knowEvents fid perOld ps
            endSeen = seenAtomicCli knowEvents fid perNew ps
        if startSeen && endSeen
          then sendUpdate fid atomic
          else breakSend lid fid perNew
      posLevel fid lid = do
        resetsBodies <- case resetsFov of
          Nothing -> getsState $ actorAssocs (== fid) lid
          Just as -> do
            let findOrOld aid = EM.findWithDefault (getActorBody aid sOld) aid
                f aid s = (aid, findOrOld aid $ sactorD s)
            -- Filter due to cases like @UpdDisplaceActor@.
            filter ((== fid) . bfid . snd) <$> mapM (getsState . f) as
        let perOld = sperFidOld EM.! fid EM.! lid
            perCacheOld = sperCacheFidOld EM.! fid EM.! lid
            resetsFovFid = not $ null resetsBodies
        if resetsFovFid || resetsOthers then do
          -- Needed often, e.g., to show thrown torches in dark corridors.
          perNew <-
            if resetsFovFid then do
              (per, perCache) <- getsState $
                perceptionFromResets (perActor perCacheOld) resetsBodies
                                     fovAspectActor fovLucidLid fovClearLid lid
              let fperFid = EM.adjust (EM.adjust (const per) lid) fid
                  fperCacheFid = EM.adjust (EM.adjust (const perCache) lid) fid
              doUpdatePerFid fperFid
              doUpdatePerCacheFid fperCacheFid
              return per
            else do
              let per = perceptionFromPTotal (ptotal perCacheOld)
                                             fovLucidLid lid
                  fperFid = EM.adjust (EM.adjust (const per) lid) fid
              doUpdatePerFid fperFid
              return per
          let inPer = diffPer perNew perOld
              outPer = diffPer perOld perNew
          if nullPer outPer && nullPer inPer
            then anySend lid fid perOld perOld
            else do
              unless knowEvents $ do  -- inconsistencies would quickly manifest
                sendA fid $ UpdPerception lid outPer inPer
                let remember = atomicRemember lid inPer sOld
                    seenNew = seenAtomicCli False fid perNew
                    seenOld = seenAtomicCli False fid perOld
                psRem <- mapM posUpdAtomic remember
                -- Verify that we remember only currently seen things.
                let !_A = assert (allB seenNew psRem) ()
                -- Verify that we remember only new things.
                let !_A = assert (allB (not . seenOld) psRem) ()
                mapM_ (sendA fid) remember
              anySend lid fid perOld perNew
        else anySend lid fid perOld perOld
      -- TODO: simplify; best after state-diffs approach tried
      send fid = case ps of
        PosSight lid _ -> posLevel fid lid
        PosFidAndSight _ lid _ -> posLevel fid lid
        PosFidAndSer (Just lid) _ -> posLevel fid lid
        -- In the following cases, from the assertion above,
        -- @resets@ is false here and broken atomic has the same ps.
        PosSmell lid _ -> do
          let perOld = sperFidOld EM.! fid EM.! lid
          anySend lid fid perOld perOld
        PosFid fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosFidAndSer Nothing fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosSer -> return ()
        PosAll -> sendUpdate fid atomic
        PosNone -> return ()
  mapWithKeyM_ (\fid _ -> send fid) factionDold

atomicRemember :: LevelId -> Perception -> State -> [UpdAtomic]
atomicRemember lid inPer s =
  -- No @UpdLoseItem@ is sent for items that became out of sight.
  -- The client will create these atomic actions based on @outPer@,
  -- if required. Any client that remembers out of sight items, OTOH,
  -- will create atomic actions that forget remembered items
  -- that are revealed not to be there any more (no @UpdSpotItem@ for them).
  -- Similarly no @UpdLoseActor@, @UpdLoseTile@ nor @UpdLoseSmell@.
  let inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      -- Actors.
      carriedAssocs b = getCarriedAssocs b s
      inPrio = concatMap (\p -> posToActors p lid s) inFov
      fActor (aid, b) =
        let ais = carriedAssocs b
        in UpdSpotActor aid b ais
      inActor = map fActor inPrio
      -- Items.
      pMaybe p = maybe Nothing (\x -> Just (p, x))
      inContainer fc itemFloor =
        let inItem = mapMaybe (\p -> pMaybe p $ EM.lookup p itemFloor) inFov
            fItem p (iid, kit) =
              UpdSpotItem iid (getItemBody iid s) kit (fc lid p)
            fBag (p, bag) = map (fItem p) $ EM.assocs bag
        in concatMap fBag inItem
      inFloor = inContainer CFloor (lfloor lvl)
      inEmbed = inContainer CEmbed (lembed lvl)
      -- Tiles.
      inTileMap = map (\p -> (p, hideTile (scops s) lvl p)) inFov
      atomicTile = if null inTileMap then [] else [UpdSpotTile lid inTileMap]
      -- Smells.
      inSmellFov = ES.elems $ totalSmelled inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [UpdSpotSmell lid inSm]
  in inFloor ++ inEmbed ++ inActor ++ atomicTile ++ atomicSmell
