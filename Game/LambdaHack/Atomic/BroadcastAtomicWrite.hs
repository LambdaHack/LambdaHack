-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.BroadcastAtomicWrite
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

-- TODO: split into simpler pieces

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- TODO: undoCmdAtomic atomic

handleCmdAtomicServer :: forall m. MonadStateWrite m
                      => PosAtomic -> CmdAtomic -> m ()
{-# INLINE handleCmdAtomicServer #-}
handleCmdAtomicServer posAtomic atomic =
  when (seenAtomicSer posAtomic) $
--    storeUndo atomic
    handleCmdAtomic atomic

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: forall m. MonadStateWrite m
                   => Bool
                   -> PerFid
                   -> (FactionId -> LevelId -> m Bool)
                   -> (FactionId -> LevelId -> m Perception)
                   -> (FactionId -> UpdAtomic -> m ())
                   -> (FactionId -> SfxAtomic -> m ())
                   -> CmdAtomic
                   -> m ()
{-# INLINE handleAndBroadcast #-}
handleAndBroadcast knowEvents sperFidOld checkSetPerValid doRecomputeCachePer
                   doSendUpdate doSendSfx atomic = do
  -- Gather data from the old state.
  sOld <- getState
  factionDold <- getsState sfactionD
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
        return (ps, map SfxAtomic atomicBroken, psBroken)
  -- Perform the action on the server.
  handleCmdAtomicServer ps atomic
  -- Invariant: if the various resets determine we do not need to update FOV,
  -- perception (@psight@ to be precise, @psmell@ is irrelevant)
  -- of any faction does not change upon full recomputation. Otherwise,
  -- save/restore would change game state (see also the assertions in gameExit).
  -- TODO: assert also that the sum of psBroken is equal to ps;
  -- with deep equality these assertions can be expensive; optimize.
  --
  -- Send some actions to the clients, one faction at a time.
  let sendUpdate fid (UpdAtomic cmd) = doSendUpdate fid cmd
      sendUpdate fid (SfxAtomic sfx) = doSendSfx fid sfx
      breakSend lid fid perNew = do
        let send2 (atomic2, ps2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid atomic2
                else do
                  -- We take the new leader, from after cmd execution.
                  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
                  case (atomic2, mleader) of
                    (UpdAtomic cmd, Just leader) -> do
                      body <- getsState $ getActorBody leader
                      loud <- loudUpdAtomic (blid body == lid) fid cmd
                      case loud of
                        Nothing -> return ()
                        Just msg -> doSendSfx fid $ SfxMsgAll msg
                    _ -> return ()
        mapM_ send2 $ zip atomicBroken psBroken
      anySend lid fid perOld perNew = do
        let startSeen = seenAtomicCli knowEvents fid perOld ps
            endSeen = seenAtomicCli knowEvents fid perNew ps
        if startSeen && endSeen
          then sendUpdate fid atomic
          else breakSend lid fid perNew
      posLevel lid fid = do
        let perOld = sperFidOld EM.! fid EM.! lid
        perValid <- checkSetPerValid fid lid
        if perValid then anySend lid fid perOld perOld
        else do
          perNew <- doRecomputeCachePer fid lid
          let inPer = diffPer perNew perOld
              outPer = diffPer perOld perNew
          if nullPer outPer && nullPer inPer
            then anySend lid fid perOld perOld
            else do
              unless knowEvents $ do  -- inconsistencies would quickly manifest
                doSendUpdate fid $ UpdPerception lid outPer inPer
                let remember = atomicRemember lid inPer sOld
                    seenNew = seenAtomicCli False fid perNew
                    seenOld = seenAtomicCli False fid perOld
                psRem <- mapM posUpdAtomic remember
                -- Verify that we remember only currently seen things.
                let !_A = assert (allB seenNew psRem) ()
                -- Verify that we remember only new things.
                let !_A = assert (allB (not . seenOld) psRem) ()
                mapM_ (doSendUpdate fid) remember
              anySend lid fid perOld perNew
      -- TODO: simplify; best after state-diffs approach tried
      send = case ps of
        PosSight lid _ -> posLevel lid
        PosFidAndSight _ lid _ -> posLevel lid
        PosFidAndSer (Just lid) _ -> posLevel lid
        -- In the following cases, from the assertion above,
        -- @resets@ is false here and broken atomic has the same ps.
        PosSmell lid _ -> \fid ->
          let perOld = sperFidOld EM.! fid EM.! lid
          in anySend lid fid perOld perOld
        PosFid fid2 -> \fid ->
          when (fid == fid2) $ sendUpdate fid atomic
        PosFidAndSer Nothing fid2 -> \fid ->
          when (fid == fid2) $ sendUpdate fid atomic
        PosSer -> \_ -> return ()
        PosAll -> \fid -> sendUpdate fid atomic
        PosNone -> \_ -> return ()
  mapWithKeyM_ (\fid _ -> send fid) factionDold

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
      carriedAssocs b = getCarriedAssocs b s
      inPrio = concatMap (\p -> posToAssocs p lid s) inFov
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
