-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast
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

-- TODO: split into simpler pieces

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- TODO: undoCmdAtomic atomic

handleCmdAtomicServer :: MonadStateWrite m
                      => PosAtomic -> CmdAtomic -> m ()
{-# INLINE handleCmdAtomicServer #-}
handleCmdAtomicServer posAtomic atomic =
  when (seenAtomicSer posAtomic) $
--    storeUndo atomic
    handleCmdAtomic atomic

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: (MonadStateWrite m, MonadServerReadRequest m)
                   => CmdAtomic -> m ()
{-# INLINE handleAndBroadcast #-}
handleAndBroadcast atomic = do
  -- This is calculated in the server State before action (simulating
  -- current client State, because action has not been applied
  -- on the client yet; the same in @atomicRemember@).
  -- E.g., actor's position in @breakUpdAtomic@ is assumed to be post-action.
  (ps, atomicBroken, psBroken) <-
    case atomic of
      UpdAtomic cmd -> do
        ps <- posUpdAtomic cmd
        atomicBroken <- breakUpdAtomic cmd
        psBroken <- mapM posUpdAtomic atomicBroken
        return (ps, map UpdAtomic atomicBroken, psBroken)
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        atomicBroken <- breakSfxAtomic sfx
        psBroken <- mapM posSfxAtomic atomicBroken
        -- TODO: assert that the sum of psBroken is equal to ps.
        return (ps, map SfxAtomic atomicBroken, psBroken)
  sOld <- getState
  -- Perform the action on the server. The only part that requires
  -- @MonadStateWrite@ and modifies server State.
  handleCmdAtomicServer ps atomic
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  sperFidOld <- getsServer sperFid
  -- Send some actions to the clients, one faction at a time.
  let sendAtomic fid (UpdAtomic cmd) = sendUpdate fid cmd
      sendAtomic fid (SfxAtomic sfx) = sendSfx fid sfx
      breakSend lid fid perFidLid = do
        let send2 (atomic2, ps2) =
              if seenAtomicCli knowEvents fid perFidLid ps2
                then sendAtomic fid atomic2
                else do
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
        mapM_ send2 $ zip atomicBroken psBroken
      -- We assume players perceive perception change before the action,
      -- so the action is perceived in the new perception,
      -- even though the new perception depends on the action's outcome
      -- (e.g., new actor created).
      anySend lid fid perFidLid =
        if seenAtomicCli knowEvents fid perFidLid ps
        then sendAtomic fid atomic
        else breakSend lid fid perFidLid
      posLevel lid fid = do
        perFidLid <- updatePer sOld fid lid
        anySend lid fid perFidLid
      -- TODO: simplify; best after state-diffs approach tried
      send = case ps of
        PosSight lid _ -> posLevel lid
        PosFidAndSight _ lid _ -> posLevel lid
        PosFidAndSer (Just lid) _ -> posLevel lid
        -- In the following cases perception is unchanged and broken atomic
        -- has the same ps.
        PosSmell lid _ -> \fid ->
          let perOld = sperFidOld EM.! fid EM.! lid
          in anySend lid fid perOld
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

updatePer :: MonadServerReadRequest m
          => State -> FactionId -> LevelId -> m Perception
{-# INLINE updatePer #-}
updatePer sOld fid lid = do
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! fid EM.! lid
  perValid <- getsServer $ (EM.! lid) . (EM.! fid) . sperValidFid
  if perValid then return perOld
  else do
    modifyServer $ \ser ->
      ser {sperValidFid = EM.adjust (EM.insert lid True) fid
                          $ sperValidFid ser}
    knowEvents <- getsServer $ sknowEvents . sdebugSer
    -- Performed in the State after action, e.g., with a new actor.
    perNew <- recomputeCachePer fid lid
    let inPer = diffPer perNew perOld
        outPer = diffPer perOld perNew
    unless (nullPer outPer && nullPer inPer) $ do
      unless knowEvents $ do  -- inconsistencies would quickly manifest
        sendUpdate fid $ UpdPerception lid outPer inPer
        let remember = atomicRemember lid inPer sOld
            seenNew = seenAtomicCli False fid perNew
            seenOld = seenAtomicCli False fid perOld
        psRem <- mapM posUpdAtomic remember
        -- Verify that we remember only currently seen things.
        let !_A = assert (allB seenNew psRem) ()
        -- Verify that we remember only new things.
        let !_A = assert (allB (not . seenOld) psRem) ()
        mapM_ (sendUpdate fid) remember
    return perNew

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
  in inFloor ++ inEmbed ++ inActor ++ atomicTile ++ atomicSmell
