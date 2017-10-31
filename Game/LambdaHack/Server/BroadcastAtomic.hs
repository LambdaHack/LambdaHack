{-# LANGUAGE TupleSections #-}
-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast, sendPer, handleCmdAtomicServer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , atomicRemember
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic
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
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.State

--storeUndo :: MonadServer m => CmdAtomic -> m ()
--storeUndo _atomic =
--  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
--    $ Nothing   -- undoCmdAtomic atomic

handleCmdAtomicServer :: MonadServerAtomic m
                       => UpdAtomic -> m (PosAtomic, [UpdAtomic], Bool)
handleCmdAtomicServer cmd = do
  ps <- posUpdAtomic cmd
  atomicBroken <- breakUpdAtomic cmd
  executedOnServer <- if seenAtomicSer ps
                      then execUpdAtomicSer cmd
                      else return False
  return (ps, atomicBroken, executedOnServer)

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: (MonadServerAtomic m, MonadServerReadRequest m)
                   => PosAtomic -> [UpdAtomic] -> CmdAtomic -> m ()
handleAndBroadcast ps atomicBroken atomic = do
  -- This is calculated in the server State before action (simulating
  -- current client State, because action has not been applied
  -- on the client yet).
  -- E.g., actor's position in @breakUpdAtomic@ is assumed to be pre-action.
  -- To get rid of breakUpdAtomic we'd need to send only Spot and Lose
  -- commands instead of Move and Displace (plus Sfx for Displace).
  -- So this only makes sense when we switch to sending state diffs.
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
        psBroken <- mapM posUpdAtomic atomicBroken
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
        PosNone -> error $ "" `showFailure` (fid, fact, atomic)
  -- Factions that are eliminated by the command are processed as well,
  -- because they are not deleted from @sfactionD@.
  factionD <- getsState sfactionD
  mapWithKeyM_ send factionD

-- | Messages for some unseen atomic commands.
loudUpdAtomic :: MonadStateRead m => Bool -> UpdAtomic -> m (Maybe SfxMsg)
loudUpdAtomic local cmd = do
  Kind.COps{coTileSpeedup} <- getsState scops
  mcmd <- case cmd of
    UpdDestroyActor _ body _ | not $ bproj body -> return $ Just cmd
    UpdCreateItem _ _ _ (CActor _ CGround) -> return $ Just cmd
    UpdTrajectory aid (Just (l, _)) Nothing | local && not (null l) -> do
      -- Non-blast projectile hits an non-walkable tile on leader's level.
      b <- getsState $ getActorBody aid
      trunk <- getsState $ getItemBody $ btrunk b
      return $! if actorTrunkIsBlast trunk then Nothing else Just cmd
    UpdAlterTile _ _ fromTile _ -> return $!
      if Tile.isDoor coTileSpeedup fromTile
      then if local then Just cmd else Nothing
      else Just cmd
    UpdAlterExplorable{} -> return $ Just cmd
    _ -> return Nothing
  return $! SfxLoudUpd local <$> mcmd

-- | Messages for some unseen sfx.
loudSfxAtomic :: MonadStateRead m => Bool -> SfxAtomic -> m (Maybe SfxMsg)
loudSfxAtomic local cmd =
  case cmd of
    SfxStrike source _ iid cstore | local -> do
      itemToF <- getsState $ itemToFull
      sb <- getsState $ getActorBody source
      bag <- getsState $ getBodyStoreBag sb cstore
      let kit = EM.findWithDefault (1, []) iid bag
          itemFull = itemToF iid kit
          ik = itemKindId $ fromJust $ itemDisco itemFull
          distance = 20  -- TODO: distance to leader; also, add a skill
      return $ Just $ SfxLoudStrike local ik distance
    SfxEffect _ aid (IK.Summon grp p) _ | local -> do
      b <- getsState $ getActorBody aid
      return $ Just $ SfxLoudSummon (bproj b) grp p
    _ -> return Nothing

sendPer :: (MonadServerAtomic m, MonadServerReadRequest m)
        => FactionId -> LevelId -> Perception -> Perception -> Perception
        -> m ()
{-# INLINE sendPer #-}
sendPer fid lid outPer inPer perNew = do
  sendUpdNoState fid $ UpdPerception lid outPer inPer
  sLocal <- getsServer $ (EM.! fid) . sclientStates
  let forget = cmdsFromPer fid lid outPer inPer sLocal
  remember <- getsState $ atomicRemember lid inPer sLocal
  let seenNew = seenAtomicCli False fid perNew
  psRem <- mapM posUpdAtomic remember
  -- Verify that we remember only currently seen things.
  let !_A = assert (allB seenNew psRem) ()
  mapM_ (sendUpdateCheck fid) $ forget
  mapM_ (sendUpdate fid) $ remember

cmdsFromPer :: FactionId -> LevelId -> Perception -> Perception -> State
            -> [UpdAtomic]
cmdsFromPer side lid outPer inPer sLocal =
  -- Wipe out actors that just became invisible due to changed FOV.
  let outFov = totalVisible outPer
      outPrio = concatMap (\p -> posToAssocs p lid sLocal) $ ES.elems outFov
      fActor (aid, b) =
        -- We forget only currently invisible actors. Actors can be outside
        -- perception, but still visible, if they belong to our faction,
        -- e.g., if they teleport to outside of current perception
        -- or if they have disabled senses.
        if not (bproj b) && bfid b == side
        then Nothing
        else Just $ UpdLoseActor aid b $ getCarriedAssocs b sLocal
          -- this command always succeeds, the actor can be always removed,
          -- because the actor is taken from the state
      outActor = mapMaybe fActor outPrio
  -- Wipe out remembered items on tiles that now came into view.
      lvl = (EM.! lid) . sdungeon $ sLocal
      inFov = ES.elems $ totalVisible inPer
      inContainer fc itemFloor =
        let inItem = mapMaybe (\p -> (p,) <$> EM.lookup p itemFloor) inFov
            fItem p (iid, kit) =
              UpdLoseItem True iid (getItemBody iid sLocal) kit (fc lid p)
            fBag (p, bag) = map (fItem p) $ EM.assocs bag
        in concatMap fBag inItem
      inFloor = inContainer CFloor (lfloor lvl)
      inEmbed = inContainer CEmbed (lembed lvl)
  -- Remembered map tiles not wiped out, due to optimization in @updSpotTile@.
  -- Wipe out remembered smell on tiles that now came into smell Fov.
      inSmellFov = totalSmelled inPer
      inSm = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvl))
                      (ES.elems inSmellFov)
      inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
  -- Note that the items and smells that we forget were previously
  -- invisible, only remembered (because taken from @inPer@),
  -- and the tiles they are on are currently visible (ditto).
  in outActor ++ inFloor ++ inEmbed ++ inSmell

atomicRemember :: LevelId -> Perception -> State -> State -> [UpdAtomic]
{-# INLINE atomicRemember #-}
atomicRemember lid inPer sLocal s =
  let inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      -- Actors.
      inAssocs = concatMap (\p -> posToAssocs p lid s) inFov
      -- Here, the actor may be already visible, e.g., when teleporting,
      -- so the exception is caught in @sendUpdate@ above.
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
      lvlLocal = sdungeon sLocal EM.! lid
      -- We ignore the server resending us hidden versions of the tiles
      -- (and resending us the same data we already got).
      -- If the tiles are changed to other variants of the hidden tile,
      -- we can still verify by searching.
      notKnown (p, t) = let tClient = lvlLocal `at` p
                        in Tile.hideAs cotile tClient /= t
      newTs = filter notKnown inTileMap
      atomicTile = if null newTs then [] else [UpdSpotTile lid newTs]
      -- Smells.
      inSmellFov = ES.elems $ totalSmelled inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [UpdSpotSmell lid inSm]
  in atomicTile ++ inFloor ++ inEmbed ++ atomicSmell ++ inActor
