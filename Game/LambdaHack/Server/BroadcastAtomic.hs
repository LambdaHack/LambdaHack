-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast, updatePer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , handleCmdAtomicServer, atomicRemember
#endif
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
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.State

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- TODO: undoCmdAtomic atomic

handleCmdAtomicServer :: MonadStateWrite m => PosAtomic -> UpdAtomic -> m ()
{-# INLINE handleCmdAtomicServer #-}
handleCmdAtomicServer _posAtomic cmd =
-- Not needed ATM:
--  when (seenAtomicSer posAtomic) $
-- Not implemented ATM:
--    storeUndo atomic
    handleUpdAtomic cmd

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: (MonadStateWrite m, MonadServerReadRequest m)
                   => CmdAtomic -> m ()
{-# INLINABLE handleAndBroadcast #-}
handleAndBroadcast atomic = do
  -- This is calculated in the server State before action (simulating
  -- current client State, because action has not been applied
  -- on the client yet; the same in @atomicRemember@).
  -- E.g., actor's position in @breakUpdAtomic@ is assumed to be pre-action.
  -- To get rid of breakUpdAtomic we'd need to send only Spot and Lose
  -- commands instead of Move and Displace (plus Sfx for Displace).
  -- So this only makes sense when we switch to sending state diffs.
  (ps, atomicBroken, psBroken) <-
    case atomic of
      UpdAtomic cmd -> do
        ps <- posUpdAtomic cmd
        atomicBroken <- breakUpdAtomic cmd
        psBroken <- mapM posUpdAtomic atomicBroken
        -- Perform the action on the server. The only part that requires
        -- @MonadStateWrite@ and modifies server State.
        handleCmdAtomicServer ps cmd
        return (ps, atomicBroken, psBroken)
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        return (ps, [], [])
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  sperFidOld <- getsServer sperFid
  -- Send some actions to the clients, one faction at a time.
  let sendAtomic fid (UpdAtomic cmd) = sendUpdate fid cmd
      sendAtomic fid (SfxAtomic sfx) = sendSfx fid sfx
      breakSend lid fid perFidLid = do
        let hear atomic2 = do
              -- We take the new leader, from after cmd execution.
              mleader <- getsState $ gleader . (EM.! fid) . sfactionD
              case (atomic2, mleader) of
                (UpdAtomic cmd, Just leader) -> do
                  body <- getsState $ getActorBody leader
                  loud <- loudUpdAtomic (blid body == lid) fid cmd
                  case loud of
                    Nothing -> return ()
                    Just msg -> sendSfx fid $ SfxMsgAll msg
                _ -> return ()
            send2 (cmd2, ps2) =
              when (seenAtomicCli knowEvents fid perFidLid ps2) $
                sendUpdate fid cmd2
        case psBroken of
          _ : _ -> mapM_ send2 $ zip atomicBroken psBroken
          [] -> hear atomic  -- broken commands are never loud
      -- We assume players perceive perception change before the action,
      -- so the action is perceived in the new perception,
      -- even though the new perception depends on the action's outcome
      -- (e.g., new actor created).
      anySend lid fid perFidLid =
        if seenAtomicCli knowEvents fid perFidLid ps
        then sendAtomic fid atomic
        else breakSend lid fid perFidLid
      posLevel lid fid = anySend lid fid $ sperFidOld EM.! fid EM.! lid
      -- TODO: simplify; best after state-diffs approach tried
      send = case ps of
        PosSight lid _ -> posLevel lid
        PosFidAndSight _ lid _ -> posLevel lid
        PosFidAndSer (Just lid) _ -> posLevel lid
        PosSmell lid _ -> posLevel lid
        PosFid fid2 -> \fid ->
          when (fid == fid2) $ sendAtomic fid atomic
        PosFidAndSer Nothing fid2 -> \fid ->
          when (fid == fid2) $ sendAtomic fid atomic
        PosSer -> \_ -> return ()
        PosAll -> \fid -> sendAtomic fid atomic
        PosNone -> \fid -> assert `failure` (fid, atomic)
  -- Faction that are eliminated by the command are processed as well,
  -- because they are not deleted from @sfactionD@.
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> send fid) factionD

updatePer :: MonadServerReadRequest m => FactionId -> LevelId -> m ()
{-# INLINE updatePer #-}
updatePer fid lid = do
  modifyServer $ \ser ->
    ser {sperValidFid = EM.adjust (EM.insert lid True) fid $ sperValidFid ser}
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! fid EM.! lid
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  -- Performed in the State after action, e.g., with a new actor.
  perNew <- recomputeCachePer fid lid
  let inPer = diffPer perNew perOld
      outPer = diffPer perOld perNew
  unless (nullPer outPer && nullPer inPer) $ do
    unless knowEvents $ do  -- inconsistencies would quickly manifest
      sendUpdate fid $ UpdPerception lid outPer inPer
      remember <- getsState $ atomicRemember lid inPer
      let seenNew = seenAtomicCli False fid perNew
--          seenOld = seenAtomicCli False fid perOld
      psRem <- mapM posUpdAtomic remember
      -- Verify that we remember only currently seen things.
      let !_A = assert (allB seenNew psRem) ()
      -- Verify that we remember only new things.
      -- but now e.g., actors are added by actions before,
      -- so they will not appear new. TODO; refine somehow
--        let !_A = assert (allB (not . seenOld) psRem) ()
      mapM_ (sendUpdate fid) remember

atomicRemember :: LevelId -> Perception -> State -> [UpdAtomic]
{-# INLINE atomicRemember #-}
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
      inAssocs = concatMap (\p -> posToAssocs p lid s) inFov
      fActor (aid, b) = let ais = getCarriedAssocs b s
                        in UpdSpotActor aid b ais
      inActor = map fActor inAssocs
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
  in atomicTile ++ inFloor ++ inEmbed ++ atomicSmell ++ inActor
