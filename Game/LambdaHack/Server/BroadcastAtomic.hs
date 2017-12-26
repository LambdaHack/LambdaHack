{-# LANGUAGE TupleSections #-}
-- | Sending atomic commands to clients and executing them on the server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast, sendPer, handleCmdAtomicServer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , loudUpdAtomic, loudSfxAtomic, atomicForget, atomicRemember
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Key (mapWithKeyM_)

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind (isUknownSpace)
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ProtocolM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

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
  knowEvents <- getsServer $ sknowEvents . soptions
  sperFidOld <- getsServer sperFid
  -- Send some actions to the clients, one faction at a time.
  let sendAtomic fid (UpdAtomic cmd) = sendUpdate fid cmd
      sendAtomic fid (SfxAtomic sfx) = sendSfx fid sfx
      breakSend lid fid fact perFidLid = do
        -- We take the new leader, from after cmd execution.
        let hear atomic2 = do
              local <- case gleader fact of
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
      return $! if bproj b && actorTrunkIsBlast trunk then Nothing else Just cmd
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
      itemToF <- getsState itemToFull
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
  sClient <- getsServer $ (EM.! fid) . sclientStates
  let forget = atomicForget fid lid outPer sClient
  remember <- getsState $ atomicRemember lid inPer sClient
  let seenNew = seenAtomicCli False fid perNew
  psRem <- mapM posUpdAtomic remember
  -- Verify that we remember only currently seen things.
  let !_A = assert (allB seenNew psRem) ()
  mapM_ (sendUpdateCheck fid) forget
  mapM_ (sendUpdate fid) remember

-- Remembered items, map tiles and smells are not wiped out when they get
-- out of FOV. Clients remember them. Only actors are forgotten.
atomicForget :: FactionId -> LevelId -> Perception -> State
             -> [UpdAtomic]
atomicForget side lid outPer sClient =
  -- Wipe out actors that just became invisible due to changed FOV.
  let outFov = totalVisible outPer
      outPrio = concatMap (\p -> posToAssocs p lid sClient) $ ES.elems outFov
      fActor (aid, b) =
        -- We forget only currently invisible actors. Actors can be outside
        -- perception, but still visible, if they belong to our faction,
        -- e.g., if they teleport to outside of current perception
        -- or if they have disabled senses.
        if not (bproj b) && bfid b == side
        then Nothing
        else Just $ UpdLoseActor aid b $ getCarriedAssocsAndTrunk b sClient
          -- this command always succeeds, the actor can be always removed,
          -- because the actor is taken from the state
      outActor = mapMaybe fActor outPrio
  in outActor

atomicRemember :: LevelId -> Perception -> State -> State -> [UpdAtomic]
{-# INLINE atomicRemember #-}
atomicRemember lid inPer sClient s =
  let Kind.COps{cotile, coTileSpeedup} = scops s
      inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      -- Wipe out remembered items on tiles that now came into view
      -- and spot items on these tiles. Optimized away, when items match.
      lvlClient = sdungeon sClient EM.! lid
      inContainer allow fc bagEM bagEMClient =
        let f p = case (EM.lookup p bagEM, EM.lookup p bagEMClient) of
              (Nothing, Nothing) -> []  -- most common, no items ever
              (Just bag, Nothing) ->  -- common, client unaware
                let ais = map (\iid -> (iid, getItemBody iid s))
                              (EM.keys bag)
                in  [UpdSpotItemBag (fc lid p) bag ais | allow p]
              (Nothing, Just bagClient) ->  -- uncommon, all items vanished
                -- We don't check @allow@, because client sees items there,
                -- so we assume he's aware of the tile enough to notice.
               let aisClient = map (\iid -> (iid, getItemBody iid sClient))
                                    (EM.keys bagClient)
                in [UpdLoseItemBag (fc lid p) bagClient aisClient]
              (Just bag, Just bagClient) ->
                -- We don't check @allow@, because client sees items there,
                -- so we assume he's aware of the tile enough to see new items.
                if bag == bagClient
                then []  -- common, nothing has changed, so optimized
                else  -- uncommon, surprise; because it's rare, we send
                      -- whole bags and don't optimize by sending only delta
                  let aisClient = map (\iid -> (iid, getItemBody iid sClient))
                                      (EM.keys bagClient)
                      ais = map (\iid -> (iid, getItemBody iid s))
                                (EM.keys bag)
                  in [ UpdLoseItemBag (fc lid p) bagClient aisClient
                     , UpdSpotItemBag (fc lid p) bag ais ]
        in concatMap f inFov
      inFloor = inContainer (const True) CFloor (lfloor lvl) (lfloor lvlClient)
      -- Check that client may be shown embedded items, assuming he's not seeing
      -- any at this position so far. If he's not shown now, the items will be
      -- revealed via searching the tile later on.
      -- This check is essential to prevent embedded items from leaking
      -- tile identity.
      allowEmbed p = not (Tile.isHideAs coTileSpeedup $ lvl `at` p)
                     || lvl `at` p == lvlClient `at` p
      inEmbed = inContainer allowEmbed CEmbed (lembed lvl) (lembed lvlClient)
      -- Spot tiles.
      atomicTile =
        -- We ignore the server resending us hidden versions of the tiles
        -- (or resending us the same data we already got).
        -- If the tiles are changed to other variants of the hidden tile,
        -- we can still verify by searching.
        let f p (loses1, spots1) =
              let t = lvl `at` p
                  tHidden = fromMaybe t $ Tile.hideAs cotile t
                  tClient = lvlClient `at` p
              in if tClient `elem` [t, tHidden]
                 then (loses1, spots1)
                 else ( if isUknownSpace tClient
                        then loses1
                        else (p, tClient) : loses1
                      , (p, tHidden) : spots1 )  -- send the hidden version
            (loses, spots) = foldr f ([], []) inFov
        in [UpdLoseTile lid loses | not $ null loses]
           ++ [UpdSpotTile lid spots | not $ null spots]
      -- Wipe out remembered smell on tiles that now came into smell Fov.
      -- Smell radius is small, so we can just wipe and send all.
      inSmellFov = ES.elems $ totalSmelled inPer
      inSm = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvlClient)) inSmellFov
      inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
      -- Spot smells.
      inSm2 = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm2 then [] else [UpdSpotSmell lid inSm2]
      -- Actors come last to report the environment they land on.
      inAssocs = concatMap (\p -> posToAssocs p lid s) inFov
      -- Here, the actor may be already visible, e.g., when teleporting,
      -- so the exception is caught in @sendUpdate@ above.
      fActor (aid, b) = let ais = getCarriedAssocsAndTrunk b s
                        in UpdSpotActor aid b ais
      inActor = map fActor inAssocs
  in atomicTile ++ inFloor ++ inEmbed ++ inSmell ++ atomicSmell ++ inActor
