-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast, sendPer
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

import Game.LambdaHack.Atomic
import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Atomic.PosAtomicRead
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.State

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- undoCmdAtomic atomic

handleCmdAtomicServer :: MonadStateWrite m => PosAtomic -> UpdAtomic -> m ()
{-# INLINE handleCmdAtomicServer #-}
handleCmdAtomicServer posAtomic cmd =
  when (seenAtomicSer posAtomic) $
-- Not implemented ATM:
--    storeUndo atomic
    handleUpdAtomic cmd

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: (MonadStateWrite m, MonadServerReadRequest m)
                   => CmdAtomic -> m ()
handleAndBroadcast atomic = do
  -- This is calculated in the server State before action (simulating
  -- current client State, because action has not been applied
  -- on the client yet).
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
      breakSend lid fid fact perFidLid = do
        -- We take the new leader, from after cmd execution.
        let hear atomic2 = do
              local <- case _gleader fact of
                Nothing -> return True  -- give leaderless factions some love
                Just leader -> do
                  body <- getsState $ getActorBody leader
                  return $! (blid body == lid)
              loud <- case atomic2 of
                UpdAtomic cmd -> loudUpdAtomic local cmd
                SfxAtomic cmd -> loudSfxAtomic local cmd
              case loud of
                Nothing -> return ()
                Just msg -> sendSfx fid $ SfxMsgFid fid msg
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
      anySend lid fid fact perFidLid =
        if seenAtomicCli knowEvents fid perFidLid ps
        then sendAtomic fid atomic
        else breakSend lid fid fact perFidLid
      posLevel lid fid fact =
        anySend lid fid fact $ sperFidOld EM.! fid EM.! lid
      send fid fact = case ps of
        PosSight lid _ -> posLevel lid fid fact
        PosFidAndSight _ lid _ -> posLevel lid fid fact
        PosFidAndSer (Just lid) _ -> posLevel lid fid fact
        PosSmell lid _ -> posLevel lid fid fact
        PosFid fid2 -> when (fid == fid2) $ sendAtomic fid atomic
        PosFidAndSer Nothing fid2 ->
          when (fid == fid2) $ sendAtomic fid atomic
        PosSer -> return ()
        PosAll -> sendAtomic fid atomic
        PosNone -> assert `failure` (fid, fact, atomic)
  -- Factions that are eliminated by the command are processed as well,
  -- because they are not deleted from @sfactionD@.
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid fact -> send fid fact) factionD

-- | Messages for some unseen atomic commands.
loudUpdAtomic :: MonadStateRead m => Bool -> UpdAtomic -> m (Maybe SfxMsg)
loudUpdAtomic local cmd = do
  Kind.COps{coTileSpeedup} <- getsState scops
  mcmd <- case cmd of
    UpdDestroyActor _ body _ | not $ bproj body -> return $ Just cmd
    UpdCreateItem _ _ _ (CActor _ CGround) -> return $ Just cmd
    UpdTrajectory _ (Just (l, _)) Nothing | not (null l) && local ->
      -- Projectile hits an non-walkable tile on leader's level.
      return $ Just cmd
    UpdAlterTile _ _ fromTile _ -> return $!
      if Tile.isDoor coTileSpeedup fromTile
      then if local then Just cmd else Nothing
      else Just cmd
    UpdAlterClear{} -> return $ Just cmd
    _ -> return Nothing
  return $! SfxLoudUpd local <$> mcmd

-- | Messages for some unseen sfx.
loudSfxAtomic :: MonadServer m => Bool -> SfxAtomic -> m (Maybe SfxMsg)
loudSfxAtomic local cmd = do
  case cmd of
    SfxStrike source _ iid cstore | local -> do
      itemToF <- itemToFullServer
      sb <- getsState $ getActorBody source
      bag <- getsState $ getBodyStoreBag sb cstore
      let kit = EM.findWithDefault (1, []) iid bag
          itemFull = itemToF iid kit
          ik = itemKindId $ fromJust $ itemDisco itemFull
          distance = 20  -- TODO: distance to leader; also, add a skill
      return $ Just $ SfxLoudStrike local ik distance
    _ -> return Nothing

sendPer :: MonadServerReadRequest m
        => FactionId -> LevelId
        -> Perception -> Perception -> Perception -> m ()
{-# INLINE sendPer #-}
sendPer fid lid outPer inPer perNew = do
  sendUpdate fid $ UpdPerception lid outPer inPer
  remember <- getsState $ atomicRemember lid inPer
  let seenNew = seenAtomicCli False fid perNew
  psRem <- mapM posUpdAtomic remember
  -- Verify that we remember only currently seen things.
  let !_A = assert (allB seenNew psRem) ()
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
              UpdSpotItem True iid (getItemBody iid s) kit (fc lid p)
            fBag (p, bag) = map (fItem p) $ EM.assocs bag
        in concatMap fBag inItem
      inFloor = inContainer CFloor (lfloor lvl)
      inEmbed = inContainer CEmbed (lembed lvl)
      -- Tiles.
      Kind.COps{cotile} = scops s
      hideTile p = Tile.hideAs cotile $ lvl `at` p
      inTileMap = map (\p -> (p, hideTile p)) inFov
      atomicTile = if null inTileMap then [] else [UpdSpotTile lid inTileMap]
      -- Smells.
      inSmellFov = ES.elems $ totalSmelled inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [UpdSpotSmell lid inSm]
  in atomicTile ++ inFloor ++ inEmbed ++ atomicSmell ++ inActor
