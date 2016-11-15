{-# LANGUAGE GADTs #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopM
  ( loopSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , factionArena, arenasForLoop, handleFidUpd, loopUpd, endClip
  , applyPeriodicLevel
  , handleTrajectories, hTrajectories, handleActors, hActors
  , gameExit, restartGame, writeSaveAll, setTrajectory
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
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
loopSer :: (MonadAtomic m, MonadServerReadRequest m)
        => DebugModeSer  -- ^ server debug parameters
        -> KeyKind -> Config -> DebugModeCli
        -> (SessionUI -> Kind.COps -> FactionId -> ChanServer ResponseUI RequestUI -> IO ())
             -- ^ the code to run for UI clients
        -> (Kind.COps -> FactionId -> ChanServer ResponseAI RequestAI -> IO ())
             -- ^ the code to run for AI clients
        -> m ()
{-# INLINE loopSer #-}
loopSer sdebug copsClient sconfig sdebugCli executorUI executorAI = {-# SCC loopSer #-} do
  -- Recover states and launch clients.
  cops <- getsState scops
  let updConn = updateConn cops copsClient sconfig sdebugCli
                           executorUI executorAI
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser, dict) | not $ snewGameSer sdebug -> do  -- a restored game
      execUpdAtomic $ UpdResumeServer $ updateCOps (const cops) sRaw
      putDict dict
#ifndef CLIENTS_AS_THREADS
      -- Avoid duplicated frontend init, if we do threaded clients.
      updateCopsDict copsClient sconfig sdebugCli
#endif
      putServer ser
      modifyServer $ \ser2 -> ser2 {sdebugNxt = sdebug}
      applyDebug
      updConn
      initPer
      pers <- getsServer sperFid
      factionD <- getsState sfactionD
      mapM_ (\fid -> sendUpdate fid $ UpdResume fid (pers EM.! fid))
            (EM.keys factionD)
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
  loopUpd updConn

-- TODO: the code is duplicated due to INLINE
factionArena :: MonadStateRead m => Faction -> m (Maybe LevelId)
{-# INLINE factionArena #-}
factionArena fact = {-# SCC factionArena #-} case gleader fact of
  -- Even spawners need an active arena for their leader,
  -- or they start clogging stairs.
  Just leader -> do
    b <- getsState $ getActorBody leader
    return $ Just $ blid b
  Nothing -> if fleaderMode (gplayer fact) == LeaderNull
                || EM.null (gvictims fact)  -- not in-between spawns
             then return Nothing
             else Just <$> getEntryArena fact

arenasForLoop :: MonadStateRead m => m [LevelId]
{-# INLINE arenasForLoop #-}
arenasForLoop = {-# SCC arenasForLoop #-} do
  factionD <- getsState sfactionD
  marenas <- mapM factionArena $ EM.elems factionD
  let arenas = ES.toList $ ES.fromList $ catMaybes marenas
      !_A = assert (not (null arenas)
                    `blame` "game over not caught earlier"
                    `twith` factionD) ()
  return $! arenas

handleFidUpd :: (MonadAtomic m, MonadServerReadRequest m)
             => (FactionId -> m ()) -> FactionId -> Faction -> m ()
{-# INLINE handleFidUpd #-}
handleFidUpd updatePerFid fid fact = do
  fa <- factionArena fact
  arenas <- getsServer sarenas
  -- Projectiles are processed first, to get out of the way
  -- and give info for deciding how to move actors.
  mapM_ (\lid -> handleTrajectories lid fid) arenas
  -- Update perception on all levels at once,
  -- in case a leader is changed to actor on another
  -- (possibly not even currently active) level.
  updatePerFid fid
  -- Move a single actor, starting on arena with leader, if available.
  let handle [] = return ()
      handle (lid : rest) = do
        nonWaitMove <- handleActors lid fid
        unless nonWaitMove $ handle rest
      myArenas = case fa of
        Just myArena -> myArena : delete myArena arenas
        Nothing -> arenas
  handle myArenas

-- Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
loopUpd :: forall m. (MonadAtomic m, MonadServerReadRequest m) => m () -> m ()
{-# INLINE loopUpd #-}
loopUpd updConn = {-# SCC loopUpd #-} do
  let updatePerFid :: FactionId -> m ()
      {-# NOINLINE updatePerFid #-}
      updatePerFid fid = {-# SCC updatePerFid #-} do
        perValid <- getsServer $ (EM.! fid) . sperValidFid
        mapM_ (\(lid, valid) -> unless valid $ updatePer fid lid)
              (EM.assocs perValid)
      handleFid :: (FactionId, Faction) -> m ()
      {-# NOINLINE handleFid #-}
      handleFid (fid, fact) = {-# SCC handleFid #-} handleFidUpd updatePerFid fid fact
      loopUpdConn = do
        endClip
        factionD <- getsState sfactionD
        mapM_ handleFid (EM.assocs factionD)
        -- Update all perception for visual feedback and to make sure
        -- saving a game doesn't affect gameplay (by updating perception).
        mapM_ updatePerFid (EM.keys factionD)
        quit <- getsServer squit
        if quit then do
          modifyServer $ \ser -> ser {squit = False}
          endOrLoop loopUpdConn (restartGame updConn loopUpdConn)
                    gameExit (writeSaveAll True)
        else
          loopUpdConn
  loopUpdConn

endClip :: (MonadAtomic m, MonadServerReadRequest m) => m ()
{-# INLINE endClip #-}
endClip = {-# SCC endClip #-} do
  Kind.COps{corule} <- getsState scops
  let RuleKind{rwriteSaveClips, rleadLevelClips} = Kind.stdRuleset corule
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r >= 5) r
  when (clipN `mod` rwriteSaveClips == 0) $ do
    modifyServer $ \ser -> ser {swriteSave = False}
    writeSaveAll False
  validArenas <- getsServer svalidArenas
  unless validArenas $ do
    sarenas <- arenasForLoop
    modifyServer $ \ser -> ser {sarenas, svalidArenas = True}
  arenas <- getsServer sarenas
  -- I need to send time updates, because I can't add time to each command,
  -- because I'd need to send also all arenas, which should be updated,
  -- and this is too expensive data for each, e.g., projectile move.
  -- I send even if nothing changes so that UI time display can progress.
  execUpdAtomic $ UpdAgeGame arenas
  -- Perform periodic dungeon maintenance.
  when (clipN `mod` rleadLevelClips == 0) leadLevelSwitch
  case clipN `mod` clipInTurn of
    2 ->
      -- Periodic activation only once per turn, for speed,
      -- but on all active arenas.
      applyPeriodicLevel
    4 ->
      -- Add monsters each turn, not each clip.
      spawnMonster
    _ -> return ()

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: (MonadAtomic m, MonadServer m) => m ()
{-# INLINE applyPeriodicLevel #-}
applyPeriodicLevel = {-# SCC applyPeriodicLevel #-} do
  arenas <- getsServer sarenas
  let arenasSet = ES.fromDistinctAscList arenas
      applyPeriodicItem _ _ _ (_, (_, [])) = return ()
        -- periodic items always have at least one timer
      applyPeriodicItem aid cstore getStore (iid, _) = do
        -- Check if the item is still in the bag (previous items act!).
        bag <- getsState $ getStore . getActorBody aid
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
        when (not (bproj b) && blid b `ES.member` arenasSet) $ do
          mapM_ (applyPeriodicItem aid COrgan borgan) $ EM.assocs $ borgan b
          mapM_ (applyPeriodicItem aid CEqp beqp) $ EM.assocs $ beqp b
  allActors <- getsState sactorD
  mapM_ applyPeriodicActor $ EM.assocs allActors

handleTrajectories :: (MonadAtomic m, MonadServer m)
                   => LevelId -> FactionId -> m ()
{-# INLINE handleTrajectories #-}
handleTrajectories lid fid = {-# SCC handleTrajectories #-} do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  s <- getState
  let l = sortBy (Ord.comparing fst)
          $ filter (\(_, (_, b)) -> isJust (btrajectory b) || bhp b <= 0)
          $ map (\(a, atime) -> (atime, (a, getActorBody a s)))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  mapM_ (hTrajectories . snd) l

-- The body @b@ may be outdated by this time
-- (due to other actors following their trajectories)
-- but we decide death inspecting it --- last moment rescue
-- from projectiles or pushed actors doesn't work; too late.
-- Even if the actor got teleported to another level by this point,
-- we don't care, we set the trajectory, check death, etc.
hTrajectories :: (MonadAtomic m, MonadServer m)
              => (ActorId, Actor) -> m ()
{-# INLINE hTrajectories #-}
hTrajectories (aid, b) = {-# SCC hTrajectories #-} do
  if actorDying b then do
    -- if bhp b <= 0:
    -- If @b@ is a projectile and it hits an actor,
    -- the carried item is destroyed and that's all.
    -- Otherwise, an actor dies, items drop to the ground
    -- and possibly a new leader is elected.
    --
    -- if btrajectory null:
    -- A projectile drops to the ground due to obstacles or range.
    -- The carried item is not destroyed, but drops to the ground.
    --
    -- If a projectile hits actor or is hit, it's destroyed,
    -- as opposed to just bouncing off the wall or landing on the ground,
    -- in which case it breaks only if the item is fragile.
    dieSer aid b (bhp b <= 0 && bproj b)
  else do
    setTrajectory aid
    b2 <- getsState $ getActorBody aid
    if actorDying b2 then
      -- Dispose of the actor with trajectory ASAP to make sure it doesn't
      -- block movement of other actors.
      hTrajectories (aid, b2)
    else do
      advanceTime aid
      managePerTurn aid

-- TODO: move somewhere?
-- | Manage trajectory of a projectile.
--
-- Colliding with a wall or actor doesn't take time, because
-- the projectile does not move (the move is blocked).
-- Not advancing time forces dead projectiles to be destroyed ASAP.
-- Otherwise, with some timings, it can stay on the game map dead,
-- blocking path of human-controlled actors and alarming the hapless human.
setTrajectory :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
{-# INLINE setTrajectory #-}
setTrajectory aid = {-# SCC setTrajectory #-} do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  case btrajectory b of
    Just (d : lv, speed) ->
      if not $ accessibleDir cops lvl (bpos b) d
      then do
        -- Lose HP due to bumping into an obstacle.
        execUpdAtomic $ UpdRefillHP aid minusM
        execUpdAtomic $ UpdTrajectory aid (btrajectory b)
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

handleActors :: (MonadAtomic m, MonadServerReadRequest m)
             => LevelId -> FactionId -> m Bool
{-# INLINE handleActors #-}
handleActors lid fid = {-# SCC handleActors #-} do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  factionD <- getsState sfactionD
  s <- getState
  let notLeader (aid, b) = Just aid /= gleader (factionD EM.! bfid b)
      l = sortBy (Ord.comparing $ \(aid, b) ->
                   (notLeader (aid, b), bsymbol b /= '@', bsymbol b, bcolor b))
          $ filter (\(_, b) -> isNothing (btrajectory b) && bhp b > 0)
          $ map (\(a, _) -> (a, getActorBody a s))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  hActors fid l

hActors :: forall m. (MonadAtomic m, MonadServerReadRequest m)
        => FactionId -> [(ActorId, Actor)] -> m Bool
{-# INLINE hActors #-}
hActors _ [] = return False
hActors fid ((aid, body) : rest) = {-# SCC hActors #-} do
  let side = bfid body
  fact <- getsState $ (EM.! side) . sfactionD
  quit <- getsServer squit
  let mleader = gleader fact
      aidIsLeader = mleader == Just aid
      mainUIactor = fhasUI (gplayer fact)
                    && (aidIsLeader
                        || fleaderMode (gplayer fact) == LeaderNull)
      mainUIunderAI = mainUIactor && isAIFact fact && not quit
      doQueryUI = mainUIactor && not (isAIFact fact)
  when mainUIunderAI $ do
    cmdS <- sendQueryUI side aid
    case fst cmdS of
      ReqUINop -> return ()
      ReqUIAutomate -> execUpdAtomic $ UpdAutoFaction side False
      ReqUIGameExit -> do
        reqGameExit aid
        -- This is not proper UI-forced save, but a timeout, so don't save.
        modifyServer $ \ser -> ser {swriteSave = False}
      _ -> assert `failure` cmdS  -- TODO: handle more
    -- Clear messages in the UI client, regardless if leaderless or not.
    execUpdAtomic $ UpdRecordHistory side
  let mswitchLeader :: Maybe ActorId -> m ActorId
      {-# NOINLINE mswitchLeader #-}
      mswitchLeader (Just aidNew) = switchLeader side aidNew >> return aidNew
      mswitchLeader Nothing = return aid
  (aidNew, mtimed) <-
    if doQueryUI then do
      (cmd, maid) <- sendQueryUI side aid
      aidNew <- mswitchLeader maid
      mtimed <- handleRequestUI side aidNew cmd
      return (aidNew, mtimed)
    else do
      (cmd, maid) <- sendQueryAI side aid
      aidNew <- mswitchLeader maid
      mtimed <- handleRequestAI side aidNew cmd
      return (aidNew, mtimed)
  nonWaitMove <- case mtimed of
    -- TODO: check that the command is legal first, report and reject,
    -- but do not crash (currently server asserts things and crashes)
    Just (RequestAnyAbility timed) -> handleRequestTimed side aidNew timed
    Nothing -> return False
  if nonWaitMove then return True else hActors fid rest

gameExit :: (MonadAtomic m, MonadServerReadRequest m) => m ()
gameExit = do
  -- Verify that the not saved caches are equal to future reconstructed.
  -- Otherwise, save/restore would change game state.
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
{-# INLINE restartGame #-}
restartGame updConn loop mgameMode = {-# SCC restartGame #-} do
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
