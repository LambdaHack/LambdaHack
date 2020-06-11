{-# LANGUAGE TupleSections #-}
-- | Sending atomic commands to clients and executing them on the server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.BroadcastAtomic
  ( handleAndBroadcast, sendPer, handleCmdAtomicServer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , cmdItemsFromIids, hearUpdAtomic, hearSfxAtomic, filterHear, atomicForget
  , atomicRemember
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
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind (isUknownSpace)
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
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
    -- needs to be done before the states are changed and may make no sense
  executedOnServer <- if seenAtomicSer ps
                      then execUpdAtomicSer cmd
                      else return False
  return (ps, atomicBroken, executedOnServer)

-- | Send an atomic action to all clients that can see it.
handleAndBroadcast :: (MonadServerAtomic m, MonadServerComm m)
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
  let sendAtomic fid (UpdAtomic cmd) = do
        let iids = iidUpdAtomic cmd
        s <- getState
        sClient <- getsServer $ (EM.! fid) . sclientStates
        mapM_ (sendUpdateCheck fid) $ cmdItemsFromIids iids sClient s
        sendUpdate fid cmd
      sendAtomic fid (SfxAtomic sfx) = do
        let iids = iidSfxAtomic sfx
        s <- getState
        sClient <- getsServer $ (EM.! fid) . sclientStates
        mapM_ (sendUpdateCheck fid) $ cmdItemsFromIids iids sClient s
        sendSfx fid sfx
      breakSend fid perFid = case lidOfPos ps of
        Nothing -> return ()
        Just lidOriginal -> do
          psBroken <- mapM posUpdAtomic atomicBroken
          case psBroken of
            _ : _ -> do
              let send2 (cmd2, ps2) =
                    when (seenAtomicCli knowEvents fid perFid ps2) $
                      sendAtomic fid (UpdAtomic cmd2)
              mapM_ send2 $ zip atomicBroken psBroken
            [] -> do  -- hear only here; broken commands are never loud
              -- At most @minusM@ applied total over a single actor move,
              -- to avoid distress as if wounded (which is measured via deltas).
              -- So, if faction hits an enemy and it yells, hearnig yell will
              -- not decrease calm over the decrease from hearing strike.
              -- This may accumulate over time, though, to eventually wake up
              -- sleeping actors.
              let drainCalmOnce aid = do
                    b <- getsState $ getActorBody aid
                    when (deltaBenign $ bcalmDelta b) $
                      execUpdAtomic $ UpdRefillCalm aid minusM
                  leaderDistance pos = do
                    mleader <- getsState $ gleader . (EM.! fid) . sfactionD
                    case mleader of
                      Nothing -> return Nothing
                      Just leader -> do
                        b <- getsState $ getActorBody leader
                        -- Leader's hearing irrelevant, which prevents
                        -- changing leader just to get hearing intel.
                        -- However, leader's position affects accuracy.
                        return $ Just $ min 5 $ chessDist pos (bpos b) `div` 10
              -- Projectiles never hear, for speed and simplicity,
              -- even though they sometimes see. There are flying cameras,
              -- but no microphones --- drones make too much noise themselves.
              as <- getsState $ fidActorRegularAssocs fid lidOriginal
              case atomic of
                UpdAtomic cmd -> do
                  (profound, mpos) <- hearUpdAtomic cmd
                  case mpos of
                    Nothing | profound ->
                      sendUpdate fid $ UpdHearFid fid Nothing
                                     $ HearUpd cmd
                    Nothing -> return ()
                    Just pos -> do
                      distance <- leaderDistance pos
                      aids <- filterHear pos as
                      if null aids && not profound
                      then return ()
                      else do
                        sendUpdate fid $ UpdHearFid fid distance
                                       $ HearUpd cmd
                        mapM_ drainCalmOnce aids
                SfxAtomic cmd -> do
                  mhear <- hearSfxAtomic cmd
                  case mhear of
                    Nothing -> return ()
                    Just (hearMsg, profound, pos) -> do
                      distance <- leaderDistance pos
                      aids <- filterHear pos as
                      if null aids && not profound
                      then return ()
                      else do
                        sendUpdate fid $ UpdHearFid fid distance hearMsg
                        mapM_ drainCalmOnce aids
      -- We assume players perceive perception change before the action,
      -- so the action is perceived in the new perception,
      -- even though the new perception depends on the action's outcome
      -- (e.g., new actor created).
      send fid = do
        let perFid = sperFidOld EM.! fid
        if seenAtomicCli knowEvents fid perFid ps
        then sendAtomic fid atomic
        else breakSend fid perFid
  -- Factions that are eliminated by the command are processed as well,
  -- because they are not deleted from @sfactionD@.
  factionD <- getsState sfactionD
  mapM_ send $ EM.keys factionD

cmdItemsFromIids :: [ItemId] -> State -> State -> [UpdAtomic]
cmdItemsFromIids iids sClient s =
  let iidsUnknown = filter (\iid -> EM.notMember iid $ sitemD sClient) iids
      items = map (\iid -> (iid, sitemD s EM.! iid)) iidsUnknown
  in if null items then [] else [UpdRegisterItems items]

-- | Messages for some unseen atomic commands.
hearUpdAtomic :: MonadStateRead m
              => UpdAtomic -> m (Bool, Maybe Point)
hearUpdAtomic cmd = do
  COps{coTileSpeedup} <- getsState scops
  case cmd of
    UpdDestroyActor _ body _ | not $ bproj body ->
      return (True, Just $ bpos body)
    UpdCreateItem True iid item _ (CActor aid cstore) -> do
      -- Kinetic damage implies the explosion is loud enough to cause noise.
      itemKind <- getsState $ getItemKindServer item
      discoAspect <- getsState sdiscoAspect
      let arItem = discoAspect EM.! iid
      if cstore /= COrgan
         || IA.checkFlag Ability.Blast arItem
            && Dice.supDice (IK.idamage itemKind) > 0 then do
        body <- getsState $ getActorBody aid
        return (True, Just $ bpos body)
      else return (False, Nothing)
    UpdTrajectory aid (Just (l, _)) Nothing | not (null l) -> do
      -- Non-blast actor hits a non-walkable tile.
      b <- getsState $ getActorBody aid
      discoAspect <- getsState sdiscoAspect
      let arTrunk = discoAspect EM.! btrunk b
      return $! ( False, if bproj b && IA.checkFlag Ability.Blast arTrunk
                         then Nothing
                         else Just $ bpos b )
    UpdAlterTile _ p _ toTile ->
      return (not $ Tile.isDoor coTileSpeedup toTile, Just p)
    UpdAlterExplorable{} -> return (True, Nothing)
    _ -> return (False, Nothing)

-- | Messages for some unseen sfx.
hearSfxAtomic :: MonadServer m
              => SfxAtomic -> m (Maybe (HearMsg, Bool, Point))
hearSfxAtomic cmd =
  case cmd of
    SfxStrike aid _ iid -> do
      -- Only the attacker position considered, for simplicity.
      b <- getsState $ getActorBody aid
      discoAspect <- getsState sdiscoAspect
      let arItem = discoAspect EM.! iid
      itemKindId <- getsState $ getIidKindIdServer iid
      -- Loud explosions cause enough noise, so ignoring particle hit spam.
      return $! if IA.checkFlag Ability.Blast arItem
                then Nothing
                else Just (HearStrike itemKindId, False, bpos b)
    SfxEffect _ aid (IK.Summon grp p) _ -> do
      b <- getsState $ getActorBody aid
      return $ Just (HearSummon (bproj b) grp p, False, bpos b)
    SfxTaunt voluntary aid -> do
      b <- getsState $ getActorBody aid
      (subject, verb) <- displayTaunt voluntary rndToAction aid
      return $ Just (HearTaunt $ subject <+> verb, True, bpos b)  -- intentional
    _ -> return Nothing

filterHear :: MonadStateRead m => Point -> [(ActorId, Actor)] -> m [ActorId]
filterHear pos as = do
  let actorHear (aid, body) = do
        -- Actors hear as if they were leaders, for speed and to prevent
        -- micromanagement by switching leader to hear more.
        -- This is analogous to actors seeing as if they were leaders.
        actorMaxSk <- getsState $ getActorMaxSkills aid
        return $! Ability.getSk Ability.SkHearing actorMaxSk
                  >= chessDist pos (bpos body)
  map fst <$> filterM actorHear as

sendPer :: (MonadServerAtomic m, MonadServerComm m)
        => FactionId -> LevelId -> Perception -> Perception -> Perception
        -> m ()
sendPer fid lid outPer inPer perNew = do
  knowEvents <- getsServer $ sknowEvents . soptions
  unless knowEvents $ do  -- inconsistencies would quickly manifest
    sendUpdNoState fid $ UpdPerception lid outPer inPer
    sClient <- getsServer $ (EM.! fid) . sclientStates
    let forget = atomicForget fid lid outPer sClient
    remember <- getsState $ atomicRemember lid inPer sClient
    let seenNew = seenAtomicCli False fid (EM.singleton lid perNew)
        onLevel UpdRegisterItems{} = True
        onLevel UpdLoseStashFaction{} = True
        onLevel _ = False
    psRem <- mapM posUpdAtomic $ filter (not . onLevel) remember
    -- Verify that we remember the currently seen things.
    let !_A = assert (allB seenNew psRem) ()
    mapM_ (sendUpdateCheck fid) forget
    mapM_ (sendUpdate fid) remember

-- Remembered items, map tiles, smells and stashes are not wiped out
-- when they get out of FOV. Clients remember them. Only actors are forgotten.
atomicForget :: FactionId -> LevelId -> Perception -> State
             -> [UpdAtomic]
atomicForget side lid outPer sClient =
  -- Wipe out actors that just became invisible due to changed FOV.
  let outFov = totalVisible outPer
      fActor (aid, b) =
        -- We forget only currently invisible actors. Actors can be outside
        -- perception, but still visible, if they belong to our faction,
        -- e.g., if they teleport to outside of current perception
        -- or if they have disabled senses.
        UpdLoseActor aid b
          -- this command always succeeds, the actor can be always removed,
          -- because the actor is taken from the state
      outPrioBig = mapMaybe (\p -> posToBigAssoc p lid sClient)
                   $ ES.elems outFov
      outPrioProj = concatMap (\p -> posToProjAssocs p lid sClient)
                    $ ES.elems outFov
  in map fActor $ filter ((/= side) . bfid . snd) outPrioBig ++ outPrioProj

-- The second argument are the points newly in FOV.
atomicRemember :: LevelId -> Perception -> State -> State -> [UpdAtomic]
{-# INLINE atomicRemember #-}
atomicRemember lid inPer sClient s =
  let COps{cotile, coTileSpeedup} = scops s
      locateStash ((fidClient, factClient), (fid, fact)) =
        assert (fidClient == fid)
        $ case (gstash factClient, gstash fact) of
            (Just (lidStash, pos), Nothing)
              | lidStash == lid && pos `ES.member` totalVisible inPer ->
                [UpdLoseStashFaction False fid lid pos]
            (Nothing, Just (lidStash, pos))
              | lidStash == lid && pos `ES.member` totalVisible inPer ->
                [UpdSpotStashFaction True fid lid pos]
            (Just (lidStash1, pos1), Just (lidStash2, pos2))
              | gstash factClient /= gstash fact ->
                if | lidStash2 == lid && pos2 `ES.member` totalVisible inPer ->
                     [ UpdLoseStashFaction False fid lidStash1 pos1
                     , UpdSpotStashFaction True fid lid pos2 ]
                   | lidStash1 == lid && pos1 `ES.member` totalVisible inPer ->
                     [UpdLoseStashFaction False fid lid pos1]
                   | otherwise -> []
            _ -> []
      atomicStash = concatMap locateStash $ zip (EM.assocs $ sfactionD sClient)
                                                (EM.assocs $ sfactionD s)
      inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      -- Wipe out remembered items on tiles that now came into view
      -- and spot items on these tiles. Optimized away, when items match.
      lvlClient = sdungeon sClient EM.! lid
      inContainer allow fc bagEM bagEMClient =
        let f p = case (EM.lookup p bagEM, EM.lookup p bagEMClient) of
              (Nothing, Nothing) -> []  -- most common, no items ever
              (Just bag, Nothing) ->  -- common, client unaware
                cmdItemsFromIids (EM.keys bag) sClient s
                ++ [UpdSpotItemBag (fc lid p) bag | allow p]
              (Nothing, Just bagClient) ->  -- uncommon, all items vanished
                -- We don't check @allow@, because client sees items there,
                -- so we assume he's aware of the tile enough to notice.
                [UpdLoseItemBag (fc lid p) bagClient]
              (Just bag, Just bagClient) ->
                -- We don't check @allow@, because client sees items there,
                -- so we assume he's aware of the tile enough to see new items.
                if bag == bagClient
                then []  -- common, nothing has changed, so optimized
                else -- uncommon, surprise; because it's rare, we send
                     -- whole bags and don't optimize by sending only delta
                     cmdItemsFromIids (EM.keys bag) sClient s
                     ++ [ UpdLoseItemBag (fc lid p) bagClient
                        , UpdSpotItemBag (fc lid p) bag ]
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
        let f p (loses1, spots1, entries1) =
              let t = lvl `at` p
                  tHidden = fromMaybe t $ Tile.hideAs cotile t
                  tClient = lvlClient `at` p
                  entries2 = case EM.lookup p $ lentry lvl of
                    Nothing -> entries1
                    Just entry2 -> case EM.lookup p $ lentry lvlClient of
                      Nothing -> (p, entry2) : entries1
                      Just entry3 -> assert (entry3 == entry2) entries1
                        -- avoid resending entries if client previously saw
                        -- another not hidden tile at that position
              in if tClient `elem` [t, tHidden]
                 then (loses1, spots1, entries1)
                 else ( if isUknownSpace tClient
                        then loses1
                        else (p, tClient) : loses1
                      , (p, tHidden) : spots1  -- send the hidden version
                      , if tHidden == t then entries2 else entries1)
            (loses, spots, entries) = foldr f ([], [], []) inFov
        in [UpdLoseTile lid loses | not $ null loses]
           ++ [UpdSpotTile lid spots | not $ null spots]
           ++ [UpdSpotEntry lid entries | not $ null entries]
      -- Wipe out remembered smell on tiles that now came into smell Fov.
      -- Smell radius is small, so we can just wipe and send all.
      -- TODO: only send smell younger than ltime (states get out of sync)
      -- or remove older smell elsewhere in the code each turn (expensive).
      -- For now clients act as if this was the case, not peeking into old.
      inSmellFov = ES.elems $ totalSmelled inPer
      inSm = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvlClient)) inSmellFov
      inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
      -- Spot smells.
      inSm2 = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm2 then [] else [UpdSpotSmell lid inSm2]
      -- Actors come last to report the environment they land on.
      inAssocs = concatMap (\p -> posToAidAssocs p lid s) inFov
      -- Here, the actor may be already visible, e.g., when teleporting,
      -- so the exception is caught in @sendUpdate@ above.
      fActor (aid, b) = cmdItemsFromIids (getCarriedIidsAndTrunk b) sClient s
                        ++ [UpdSpotActor aid b]
      inActor = concatMap fActor inAssocs
  in atomicStash ++ atomicTile ++ inFloor ++ inEmbed ++ inSmell
     ++ atomicSmell ++ inActor
