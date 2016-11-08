{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fprof-auto #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopM
  ( loopSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , endClip, applyPeriodicLevel, handleActors, gameExit, restartGame
  , writeSaveAll, setTrajectory
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Arrow ((&&&))
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import qualified Data.Ord as Ord

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.BroadcastAtomic
import Game.LambdaHack.Server.EndM
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.HandleEffectM
import Game.LambdaHack.Server.HandleRequestM
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicM
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.StartM
import Game.LambdaHack.Server.State

-- | Start a game session, including the clients, and then loop,
-- communicating with the clients.
loopSer :: forall m. (MonadAtomic m, MonadServerReadRequest m)
        => DebugModeSer  -- ^ server debug parameters
        -> KeyKind -> Config -> DebugModeCli
        -> (SessionUI -> Kind.COps -> FactionId -> ChanServer ResponseUI RequestUI -> IO ())
             -- ^ the code to run for UI clients
        -> (Kind.COps -> FactionId -> ChanServer ResponseAI RequestAI -> IO ())
             -- ^ the code to run for AI clients
        -> m ()
{-# INLINE loopSer #-}
loopSer sdebug copsClient sconfig sdebugCli executorUI executorAI = do
  -- Recover states and launch clients.
  cops <- getsState scops
  let updConn = updateConn cops copsClient sconfig sdebugCli
                           executorUI executorAI
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser, dict) | not $ snewGameSer sdebug -> do  -- a restored game
      execUpdAtomic $ UpdResumeServer $ updateCOps (const cops) sRaw
      putDict dict
      updateCopsDict copsClient sconfig sdebugCli
      putServer ser
      modifyServer $ \ser2 -> ser2 {sdebugNxt = sdebug}
      applyDebug
      updConn
      initPer
      pers <- getsServer sperFid
      factionD <- getsState sfactionD
      mapWithKeyM_ (\fid _ ->
       sendUpdate fid $ UpdResume fid (pers EM.! fid)) factionD
      -- We dump RNG seeds here, in case the game wasn't run
      -- with --dumpInitRngs previously and we need to seeds.
      when (sdumpInitRngs sdebug) dumpRngs
    _ -> do  -- Starting the first new game for this savefile.
      -- Set up commandline debug mode
      let mrandom = case restored of
            Just (_, ser, _) -> Just $ srandom ser
            Nothing -> Nothing
      s <- gameReset cops sdebug Nothing mrandom
      let debugBarRngs = sdebug {sdungeonRng = Nothing, smainRng = Nothing}
      modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                                 , sdebugSer = debugBarRngs }
      execUpdAtomic $ UpdRestartServer s
      updConn
      initPer
      reinitGame
      writeSaveAll False
  -- Note that if a faction enters dungeon on a level with no spawners,
  -- the faction won't cause spawning on its active arena
  -- as long as it has no leader. This may cause regeneration items
  -- of its opponents become overpowered and lead to micromanagement
  -- (make sure to kill all actors of the faction, go to a no-spawn
  -- level and heal fully with no risk nor cost).
  let arenasForLoop = do
        let factionArena fact =
              case gleader fact of
               -- Even spawners need an active arena for their leader,
               -- or they start clogging stairs.
               Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
               Nothing -> if fleaderMode (gplayer fact) == LeaderNull
                             || EM.null (gvictims fact)
                          then return Nothing
                          else Just <$> getEntryArena fact
        factionD <- getsState sfactionD
        marenas <- mapM factionArena $ EM.elems factionD
        let arenas = ES.fromList $ catMaybes marenas
        let !_A = assert (not (ES.null arenas)
                          `blame` "game over not caught earlier"
                          `twith` factionD) ()
        return $! arenas
  -- Start a clip (a part of a turn for which one or more frames
  -- will be generated). Do whatever has to be done
  -- every fixed number of time units, e.g., monster generation.
  -- Run the leader and other actors moves. Eventually advance the time
  -- and repeat.
  let loop :: ES.EnumSet LevelId -> [LevelId] -> m ()
      loop arenasStart [] = do
        arenas <- arenasForLoop
        endClip arenasStart
        loop arenas $ ES.toList arenas
      loop arenasStart (arena : rest) = do
        factionD <- getsState sfactionD
        mapM_ (\fid -> handleActors arenasStart arena True fid
                       >> updatePer fid arena
                       >> handleActors arenasStart arena False fid)
              (EM.keys factionD)
        quit <- getsServer squit
        if quit then do
          -- In case of game save+exit or restart, don't age levels (endClip)
          -- since possibly not all actors have moved yet.
          modifyServer $ \ser -> ser {squit = False}
          let loopAgain = loop arenasStart (arena : rest)
          endOrLoop loopAgain
                    (restartGame updConn loopNew) gameExit (writeSaveAll True)
        else
          loop arenasStart rest
      loopNew = do
        arenas <- arenasForLoop
        loop arenas $ ES.toList arenas
  loopNew

endClip :: (MonadAtomic m, MonadServerReadRequest m)
        => ES.EnumSet LevelId -> m ()
endClip arenas = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      writeSaveClips = rwriteSaveClips stdRuleset
      leadLevelClips = rleadLevelClips stdRuleset
  -- I need to send time updates, because I can't add time to each command,
  -- because I'd need to send also all arenas, which should be updated,
  -- and this is too expensive data for each, e.g., projectile move.
  -- I send even if nothing changes so that UI time display can progress.
  execUpdAtomic $ UpdAgeGame (Delta timeClip) $ ES.toList arenas
  -- Perform periodic dungeon maintenance.
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r >= 5) r
  when (clipN `mod` leadLevelClips == 0) leadLevelSwitch
  when (clipN `mod` clipInTurn == 2) $
    -- Periodic activation only once per turn, for speed,
    -- but on all active arenas.
    applyPeriodicLevel arenas
  when (clipN `mod` clipInTurn == 4) $ do
    -- Add monsters each turn, not each clip.
    -- Do this on only one of the arenas to prevent micromanagement,
    -- e.g., spreading leaders across levels to bump monster generation.
    arena <- rndToAction $ oneOf $ ES.toList arenas
    spawnMonster arena
  when (clipN `mod` writeSaveClips == 0) $ do
    modifyServer $ \ser -> ser {swriteSave = False}
    writeSaveAll False

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: (MonadAtomic m, MonadServer m)
                   => ES.EnumSet LevelId -> m ()
applyPeriodicLevel arenas = do
  let applyPeriodicItem _ _ (_, (_, [])) = return ()
        -- periodic items always have at least one timer
      applyPeriodicItem aid cstore (iid, _) = do
        -- Check if the item is still in the bag (previous items act!).
        b <- getsState $ getActorBody aid
        bag <- getsState $ getBodyStoreBag b cstore
        case iid `EM.lookup` bag of
          Nothing -> return ()  -- item dropped
          Just kit -> do
            itemToF <- itemToFullServer
            let itemFull = itemToF iid kit
            case itemDisco itemFull of
              Just ItemDisco {itemKind=IK.ItemKind{IK.ieffects}} ->
                when (IK.Periodic `elem` ieffects) $ do
                  -- In periodic activation, consider *only* recharging effects.
                  effectAndDestroy aid aid iid (CActor aid cstore) True
                                   (filterRecharging ieffects) itemFull
              _ -> assert `failure` (aid, cstore, iid)
      applyPeriodicActor (aid, b) =
        when (blid b `ES.member` arenas) $ do
          mapM_ (applyPeriodicItem aid COrgan) $ EM.assocs $ borgan b
          mapM_ (applyPeriodicItem aid CEqp) $ EM.assocs $ beqp b
  allActors <- getsState sactorD
  mapM_ applyPeriodicActor $ EM.assocs allActors

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less or equal to the end of current cut-off.
handleActors :: (MonadAtomic m, MonadServerReadRequest m)
             => ES.EnumSet LevelId -> LevelId -> Bool -> FactionId -> m ()
handleActors arenas lid proj fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . sactorTime
  quit <- getsServer squit
  factionD <- getsState sfactionD
  s <- getState
  let -- Actors of the same faction move together.
      notDying (_, b) = not $ actorDying b
      notLeader ((aid, _), b) = Just aid /= gleader (factionD EM.! bfid b)
      order = Ord.comparing $
        snd . fst &&& notDying &&& notLeader &&& bsymbol . snd
      -- TODO: separate projectiles in sactorTime; also separate factions
      as = filter (\(_, b) -> bfid b == fid
                              && if proj then bproj b else not (bproj b))
           $ map (\(a, atime) -> ((a, atime), getActorBody a s))
           $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
      maa | null as = Nothing
          | otherwise = Just $ minimumBy order as
      mnext = case maa of
        Just ((a, _atime), b) -> Just (a, b)
        Nothing -> Nothing
  case mnext of
    _ | quit -> return ()
    Nothing -> return ()
    Just (aid, b) | bproj b && maybe True (null . fst) (btrajectory b) -> do
      -- A projectile drops to the ground due to obstacles or range.
      -- The carried item is not destroyed, but drops to the ground.
      dieSer aid b False
      handleActors arenas lid proj fid
    Just (aid, b) | bhp b <= 0 -> do
      -- If @b@ is a projectile and it hits an actor,
      -- the carried item is destroyed and that's all.
      -- Otherwise, an actor dies, items drop to the ground
      -- and possibly a new leader is elected.
      dieSer aid b (bproj b)
      handleActors arenas lid proj fid
    Just (aid, body) -> do
      let side = bfid body
          fact = factionD EM.! side
          mleader = gleader fact
          aidIsLeader = mleader == Just aid
          mainUIactor = fhasUI (gplayer fact)
                        && (aidIsLeader
                            || fleaderMode (gplayer fact) == LeaderNull)
          mainUIunderAI = mainUIactor && isAIFact fact
          doQueryUI = mainUIactor && not (isAIFact fact)
      when mainUIunderAI $ do
        cmdS <- sendQueryUI side aid
        case fst cmdS of
          ReqUIAutomate -> execUpdAtomic $ UpdAutoFaction side False
          ReqUINop -> return ()
          ReqUIGameExit -> do
            reqGameExit aid
            -- This is not proper UI-forced save, but a timeout, so don't save.
            modifyServer $ \ser -> ser {swriteSave = False}
          _ -> assert `failure` cmdS  -- TODO: handle more
        -- Clear messages in the UI client, regardless if leaderless or not.
        execUpdAtomic $ UpdRecordHistory side
      if | isJust $ btrajectory body -> do
           setTrajectory aid
           b2 <- getsState $ getActorBody aid
           unless (bproj b2 && actorDying b2) $ do
             advanceTime aid
             managePerTurn aid
         | doQueryUI -> do
           cmdS <- sendQueryUI side aid
           -- TODO: check that the command is legal first, report and reject,
           -- but do not crash (currently server asserts things and crashes)
           handleRequestUI arenas side aid cmdS
         | aidIsLeader -> do
           cmdS <- sendQueryAI side aid
           handleRequestAI arenas side aid cmdS
         | otherwise -> do
           cmdN <- sendNonLeaderQueryAI side aid
           handleReqAI arenas side aid cmdN
      handleActors arenas lid proj fid

gameExit :: (MonadAtomic m, MonadServerReadRequest m) => m ()
gameExit = do
  -- Verify that the not saved caches are equal to future reconstructed.
  -- Otherwise, save/restore would change game state.
  -- TODO: to make this hold, we have to update the caches, so we guarantee
  -- that save/restore at most updates perception.
--  debugPossiblyPrint "Updating all perceptions."
  factionD <- getsState sfactionD
  dungeon <- getsState sdungeon
  mapWithKeyM_ (\fid _ ->
    mapWithKeyM_ (\lid _ -> updatePer fid lid) dungeon) factionD
--  debugPossiblyPrint "Verifying all perceptions."
  sperFid <- getsServer sperFid
  sperCacheFid <- getsServer sperCacheFid
  sperValidFid <- getsServer sperValidFid
  sactorAspect <- getsServer sactorAspect
  sfovLucidLid <- getsServer sfovLucidLid
  sfovClearLid <- getsServer sfovClearLid
  sfovLitLid <- getsServer sfovLitLid
  discoAspect <- getsServer sdiscoAspect
  ( actorAspect, fovLitLid, fovClearLid, fovLucidLid
   ,perValidFid, perCacheFid, perFid )
    <- getsState $ perFidInDungeon discoAspect
  let !_A7 = assert (sfovLitLid == fovLitLid
                     `blame` "wrong accumulated sfovLitLid"
                     `twith` (sfovLitLid, fovLitLid)) ()
      !_A6 = assert (sfovClearLid == fovClearLid
                     `blame` "wrong accumulated sfovClearLid"
                     `twith` (sfovClearLid, fovClearLid)) ()
      !_A5 = assert (sactorAspect == actorAspect
                     `blame` "wrong accumulated sactorAspect"
                     `twith` (sactorAspect, actorAspect)) ()
      !_A4 = assert (sfovLucidLid == fovLucidLid
                     `blame` "wrong accumulated sfovLucidLid"
                     `twith` (sfovLucidLid, fovLucidLid)) ()
      !_A3 = assert (sperValidFid == perValidFid
                     `blame` "wrong accumulated sperValidFid"
                     `twith` (sperValidFid, perValidFid)) ()
      !_A2 = assert (sperCacheFid == perCacheFid
                     `blame` "wrong accumulated sperCacheFid"
                     `twith` (sperCacheFid, perCacheFid)) ()
      !_A1 = assert (sperFid == perFid
                     `blame` "wrong accumulated perception"
                     `twith` (sperFid, perFid)) ()
  -- Kill all clients, including those that did not take part
  -- in the current game.
  -- Clients exit not now, but after they print all ending screens.
  -- debugPrint "Server kills clients"
--  debugPossiblyPrint "Killing all clients."
  killAllClients
--  debugPossiblyPrint "All clients killed."
  return ()

restartGame :: (MonadAtomic m, MonadServerReadRequest m)
            => m () -> m () -> Maybe (GroupName ModeKind) ->  m ()
restartGame updConn loop mgameMode = do
  cops <- getsState scops
  sdebugNxt <- getsServer sdebugNxt
  srandom <- getsServer srandom
  s <- gameReset cops sdebugNxt mgameMode (Just srandom)
  let debugBarRngs = sdebugNxt {sdungeonRng = Nothing, smainRng = Nothing}
  modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                             , sdebugSer = debugBarRngs }
  execUpdAtomic $ UpdRestartServer s
  updConn
  initPer
  reinitGame
  writeSaveAll False
  loop

-- TODO: This can be improved by adding a timeout
-- and by asking clients to prepare
-- a save (in this way checking they have permissions, enough space, etc.)
-- and when all report back, asking them to commit the save.
-- | Save game on server and all clients.
writeSaveAll :: (MonadAtomic m, MonadServerReadRequest m) => Bool -> m ()
writeSaveAll uiRequested = do
  bench <- getsServer $ sbenchmark . sdebugCli . sdebugSer
  when (uiRequested || not bench) $ do
    execUpdAtomic UpdWriteSave
    saveServer

-- TODO: move somewhere?
-- | Manage trajectory of a projectile.
--
-- Colliding with a wall or actor doesn't take time, because
-- the projectile does not move (the move is blocked).
-- Not advancing time forces dead projectiles to be destroyed ASAP.
-- Otherwise, with some timings, it can stay on the game map dead,
-- blocking path of human-controlled actors and alarming the hapless human.
setTrajectory :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
setTrajectory aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  case btrajectory b of
    Just (d : lv, speed) ->
      if not $ accessibleDir cops lvl (bpos b) d
      then do
        -- Lose HP due to bumping into an obstacle.
        execUpdAtomic $ UpdRefillHP aid minusM
        execUpdAtomic $ UpdTrajectory aid
                                      (btrajectory b)
                                      (Just ([], speed))
      else do
        when (bproj b && null lv) $ do
          let toColor = Color.BrBlack
          when (bcolor b /= toColor) $
            execUpdAtomic $ UpdColorActor aid (bcolor b) toColor
        -- Hit clears trajectory of non-projectiles in reqMelee so no need here.
        -- Non-projectiles displace, to make pushing in crowds less lethal
        -- and chaotic and to avoid hitting harpoons when pulled by them.
        let tpos = bpos b `shift` d  -- target position
        case posToAidsLvl tpos lvl of
          [target] | not (bproj b) -> reqDisplace aid target
          _ -> reqMove aid d
        b2 <- getsState $ getActorBody aid
        unless (btrajectory b2 == Just (lv, speed)) $  -- cleared in reqMelee
          execUpdAtomic $ UpdTrajectory aid (btrajectory b2) (Just (lv, speed))
    Just ([], _) -> do  -- non-projectile actor stops flying
      let !_A = assert (not $ bproj b) ()
      execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
    _ -> assert `failure` "Nothing trajectory" `twith` (aid, b)
