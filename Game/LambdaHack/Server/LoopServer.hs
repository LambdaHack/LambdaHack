{-# LANGUAGE GADTs #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopServer (loopSer) where

import Prelude ()
import Prelude.Compat

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import Control.Monad (unless, when)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import Data.List (minimumBy)
import Data.Maybe
import qualified Data.Ord as Ord

import Game.LambdaHack.Atomic
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
import Game.LambdaHack.Server.EndServer
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.HandleEffectServer
import Game.LambdaHack.Server.HandleRequestServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.ProtocolServer
import Game.LambdaHack.Server.StartServer
import Game.LambdaHack.Server.State

-- | Start a game session, including the clients, and then loop,
-- communicating with the clients.
loopSer :: (MonadAtomic m, MonadServerReadRequest m)
        => DebugModeSer  -- ^ server debug parameters
        -> (FactionId -> ChanServer ResponseUI RequestUI -> IO ())
             -- ^ the code to run for UI clients
        -> (FactionId -> ChanServer ResponseAI RequestAI -> IO ())
             -- ^ the code to run for AI clients
        -> m ()
loopSer sdebug executorUI executorAI = do
  -- Recover states and launch clients.
  let updConn = updateConn executorUI executorAI
  cops <- getsState scops
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser) | not $ snewGameSer sdebug -> do  -- run a restored game
      -- First, set the previous cops, to send consistent info to clients.
      let setPreviousCops = const cops
      execUpdAtomic $ UpdResumeServer $ updateCOps setPreviousCops sRaw
      putServer ser
      let sdebugNxt = initDebug cops sdebug
      modifyServer $ \ser2 -> ser2 {sdebugNxt}
      applyDebug
      updConn
      initPer
      pers <- getsServer sper
      broadcastUpdAtomic $ \fid -> UpdResume fid (pers EM.! fid)
      -- Second, set the current cops and reinit perception.
      let setCurrentCops = const (speedupCOps sdebugNxt cops)
      -- @sRaw@ is correct here, because none of the above changes State.
      execUpdAtomic $ UpdResumeServer $ updateCOps setCurrentCops sRaw
      -- We dump RNG seeds here, in case the game wasn't run
      -- with --dumpInitRngs previously and we need to seeds.
      when (sdumpInitRngs sdebug) dumpRngs
    _ -> do  -- Starting the first new game for this savefile.
      -- Set up commandline debug mode
      let mrandom = case restored of
            Just (_, ser) -> Just $ srandom ser
            Nothing -> Nothing
      s <- gameReset cops sdebug Nothing mrandom
      let sdebugNxt = initDebug cops sdebug
          debugBarRngs = sdebugNxt {sdungeonRng = Nothing, smainRng = Nothing}
      modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                                 , sdebugSer = debugBarRngs }
      let speedup = speedupCOps sdebugNxt
      execUpdAtomic $ UpdRestartServer $ updateCOps speedup s
      updConn
      initPer
      reinitGame
      writeSaveAll False
  resetSessionStart
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
               Just (leader, _) -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
               Nothing -> if fleaderMode (gplayer fact) == LeaderNull
                             || EM.null (gvictims fact)
                          then return Nothing
                          else Just <$> getEntryArena fact
        factionD <- getsState sfactionD
        marenas <- mapM factionArena $ EM.elems factionD
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        let !_A = assert (not $ null arenas) ()  -- game over not caught earlier
        return $! arenas
  -- Start a clip (a part of a turn for which one or more frames
  -- will be generated). Do whatever has to be done
  -- every fixed number of time units, e.g., monster generation.
  -- Run the leader and other actors moves. Eventually advance the time
  -- and repeat.
  let loop arenasStart [] = do
        arenas <- arenasForLoop
        continue <- endClip arenasStart
        when continue (loop arenas arenas)
      loop arenasStart (arena : rest) = do
        handleActors arena
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
        loop arenas arenas
  loopNew

endClip :: (MonadAtomic m, MonadServer m, MonadServerReadRequest m)
        => [LevelId] -> m Bool
endClip arenas = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      writeSaveClips = rwriteSaveClips stdRuleset
      leadLevelClips = rleadLevelClips stdRuleset
      ageProcessed lid = EM.insertWith absoluteTimeAdd lid timeClip
      ageServer lid ser = ser {sprocessed = ageProcessed lid $ sprocessed ser}
  mapM_ (modifyServer . ageServer) arenas
  execUpdAtomic $ UpdAgeGame (Delta timeClip) arenas
  -- Perform periodic dungeon maintenance.
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r > 2) r
      clipMod = clipN `mod` clipInTurn
  when (clipN `mod` writeSaveClips == 0) $ do
    modifyServer $ \ser -> ser {swriteSave = False}
    writeSaveAll False
  when (clipN `mod` leadLevelClips == 0) leadLevelSwitch
  if clipMod == 1 then do
    -- Periodic activation only once per turn, for speed, but on all arenas.
    mapM_ applyPeriodicLevel arenas
    -- Add monsters each turn, not each clip.
    -- Do this on only one of the arenas to prevent micromanagement,
    -- e.g., spreading leaders across levels to bump monster generation.
    arena <- rndToAction $ oneOf arenas
    spawnMonster arena
    -- Check, once per turn, for benchmark game stop, after a set time.
    stopAfter <- getsServer $ sstopAfter . sdebugSer
    case stopAfter of
      Nothing -> return True
      Just stopA -> do
        exit <- elapsedSessionTimeGT stopA
        if exit then do
          tellAllClipPS
          gameExit
          return False  -- don't re-enter the game loop
        else return True
  else return True

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
applyPeriodicLevel lid = do
  discoEffect <- getsServer sdiscoEffect
  let applyPeriodicItem c aid iid =
        case EM.lookup iid discoEffect of
          Just ItemAspectEffect{jeffects, jaspects} ->
            when (IK.Periodic `elem` jaspects) $ do
              -- Check if the item is still in the bag (previous items act!).
              bag <- getsState $ getCBag c
              case iid `EM.lookup` bag of
                Nothing -> return ()  -- item dropped
                Just kit ->
                  -- In periodic activation, consider *only* recharging effects.
                  effectAndDestroy aid aid iid c True
                                   (allRecharging jeffects) jaspects kit
          _ -> assert `failure` (lid, aid, c, iid)
      applyPeriodicCStore aid cstore = do
        let c = CActor aid cstore
        bag <- getsState $ getCBag c
        mapM_ (applyPeriodicItem c aid) $ EM.keys bag
      applyPeriodicActor aid = do
        applyPeriodicCStore aid COrgan
        applyPeriodicCStore aid CEqp
  allActors <- getsState $ actorRegularAssocs (const True) lid
  mapM_ (\(aid, _) -> applyPeriodicActor aid) allActors

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less or equal to the end of current cut-off.
handleActors :: (MonadAtomic m, MonadServerReadRequest m)
             => LevelId -> m ()
handleActors lid = do
  -- The end of this clip, inclusive. This is used exclusively
  -- to decide which actors to process this time. Transparent to clients.
  timeCutOff <- getsServer $ EM.findWithDefault timeClip lid . sprocessed
  Level{lprio} <- getLevel lid
  quit <- getsServer squit
  factionD <- getsState sfactionD
  s <- getState
  let -- Actors of the same faction move together.
      notDead (_, b) = not $ actorDying b
      notProj (_, b) = not $ bproj b
      notLeader (aid, b) = Just aid /= fmap fst (gleader (factionD EM.! bfid b))
      order = Ord.comparing $
        notDead &&& notProj &&& bfid . snd &&& notLeader &&& bsymbol . snd
      (atime, as) = EM.findMin lprio
      ams = map (\a -> (a, getActorBody a s)) as
      mnext | EM.null lprio = Nothing  -- no actor alive, wait until it spawns
            | otherwise = if atime > timeCutOff
                          then Nothing  -- no actor is ready for another move
                          else Just $ minimumBy order ams
      startActor aid = execSfxAtomic $ SfxActorStart aid
  case mnext of
    _ | quit -> return ()
    Nothing -> return ()
    Just (aid, b) | bproj b && maybe True (null . fst) (btrajectory b) -> do
      startActor aid  -- try displaying, while still alive
      -- A projectile drops to the ground due to obstacles or range.
      -- The carried item is not destroyed, but drops to the ground.
      dieSer aid b False
      handleActors lid
    Just (aid, b) | bhp b <= 0 -> do
      startActor aid  -- try displaying, while still alive
      -- If @b@ is a projectile and it hits an actor,
      -- the carried item is destroyed and that's all.
      -- Otherwise, an actor dies, items drop to the ground
      -- and possibly a new leader is elected.
      dieSer aid b (bproj b)
      handleActors lid
    Just (aid, body) -> do
      -- If only waited previous turn and not killed nor pushed nor teleported,
      -- actor's appearance not changed, so no need to redisplay screen.
      -- Calm change not important enough to insert a frame.
      -- Projectile moves handled via animations. (TODO: also birth?)
      let appearanceUnchanged = waitedLastTurn body
                                && bhpDelta body == ResDelta 0 0
                                && boldlid body == blid body
                                && boldpos body == Just (bpos body)
      unless (bproj body || appearanceUnchanged) $ startActor aid
      let side = bfid body
          fact = factionD EM.! side
          mleader = gleader fact
          aidIsLeader = fmap fst mleader == Just aid
          mainUIactor = fhasUI (gplayer fact)
                        && (aidIsLeader
                            || fleaderMode (gplayer fact) == LeaderNull)
      queryUI <-
        if mainUIactor then do
          let underAI = isAIFact fact
          if underAI then do
            -- If UI client for the faction completely under AI control,
            -- ping often to sync frames and to catch ESC,
            -- which switches off Ai control.
            sendPingUI side
            fact2 <- getsState $ (EM.! side) . sfactionD
            let underAI2 = isAIFact fact2
            return $! not underAI2
          else return True
        else return False
      let setBWait hasWait aidNew = do
            bPre <- getsState $ getActorBody aidNew
            when (hasWait /= bwait bPre) $
              execUpdAtomic $ UpdWaitActor aidNew hasWait
      if isJust $ btrajectory body then do
        setTrajectory aid
        b2 <- getsState $ getActorBody aid
        unless (bproj b2 && actorDying b2) $
          advanceTime aid
      else if queryUI then do
        cmdS <- sendQueryUI side aid
        -- TODO: check that the command is legal first, report and reject,
        -- but do not crash (currently server asserts things and crashes)
        (aidNew, action) <- handleRequestUI side cmdS
        let hasWait (ReqUITimed ReqWait{}) = True
            hasWait (ReqUILeader _ _ cmd) = hasWait cmd
            hasWait _ = False
        maybe (return ()) (setBWait (hasWait cmdS)) aidNew
        -- Advance time once, after the leader switched perhaps many times.
        -- The following was true before, but now we badly want to avoid double
        -- moves against the UI player (especially deadly when using stairs),
        -- so this is no longer true:
          -- Sometimes this may result in a double move of the new leader,
          -- followed by a double pause. Or a fractional variant of that.
          -- In this setup, reading a scroll of Previous Leader is a free action
          -- for the old leader, but otherwise his time is undisturbed.
          -- He is able to move normally in the same turn, immediately
          -- after the new leader completes his move.
        -- So now we exchange times of the old and new leader.
        -- This permits an abuse, because a slow tank can be moved fast
        -- by alternating between it and many fast actors (until all of them
        -- get slowed down by this and none remain). But at least the sum
        -- of all times of a faction is conserved. And we avoid double moves
        -- against the UI player caused by his leader changes. There may still
        -- happen double moves caused by AI leader changes, but that's rare.
        -- The flip side is the possibility of multi-moves of the UI player
        -- as in the case of the tank, but at least the sum of times is OK.
        -- Warning: when the action is performed on the server,
        -- the time of the actor is different than when client prepared that
        -- action, so any client checks involving time should discount this.
        when (aidIsLeader && Just aid /= aidNew) $
          maybe (return ()) (swapTime aid) aidNew
        maybe (return ()) advanceTime aidNew
        action
        maybe (return ()) managePerTurn aidNew
      else do
        -- Clear messages in the UI client (if any), if the actor
        -- is a leader (which happens when a UI client is fully
        -- computer-controlled) or if faction is leaderless.
        -- We could record history more often, to avoid long reports,
        -- but we'd have to add -more- prompts.
        when mainUIactor $ execUpdAtomic $ UpdRecordHistory side
        cmdS <- sendQueryAI side aid
        (aidNew, action) <- handleRequestAI side aid cmdS
        let hasWait (ReqAITimed ReqWait{}) = True
            hasWait (ReqAILeader _ _ cmd) = hasWait cmd
            hasWait _ = False
        setBWait (hasWait cmdS) aidNew
        -- AI always takes time and so doesn't loop.
        advanceTime aidNew
        action
        managePerTurn aidNew
      handleActors lid

gameExit :: (MonadAtomic m, MonadServerReadRequest m) => m ()
gameExit = do
  -- Kill all clients, including those that did not take part
  -- in the current game.
  -- Clients exit not now, but after they print all ending screens.
  -- debugPrint "Server kills clients"
  killAllClients
  -- Verify that the saved perception is equal to future reconstructed.
  persAccumulated <- getsServer sper
  fovMode <- getsServer $ sfovMode . sdebugSer
  ser <- getServer
  pers <- getsState $ \s -> dungeonPerception (fromMaybe Digital fovMode) s ser
  let !_A = assert (persAccumulated == pers
                    `blame` "wrong accumulated perception"
                    `twith` (persAccumulated, pers)) ()
  return ()

restartGame :: (MonadAtomic m, MonadServerReadRequest m)
            => m () -> m () -> Maybe (GroupName ModeKind) ->  m ()
restartGame updConn loop mgameMode = do
  tellGameClipPS
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
-- | Save game on server and all clients. Clients are pinged first,
-- which greatly reduced the chance of saves being out of sync.
writeSaveAll :: (MonadAtomic m, MonadServerReadRequest m) => Bool -> m ()
writeSaveAll uiRequested = do
  bench <- getsServer $ sbenchmark . sdebugCli . sdebugSer
  when (uiRequested || not bench) $ do
    factionD <- getsState sfactionD
    let ping fid _ = do
          sendPingAI fid
          when (fhasUI $ gplayer $ factionD EM.! fid) $ sendPingUI fid
    mapWithKeyM_ ping factionD
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
        tgt <- getsState $ posToActors tpos (blid b)
        case tgt of
          [(target, _)] | not (bproj b) -> reqDisplace aid target
          _ -> reqMove aid d
        b2 <- getsState $ getActorBody aid
        unless (btrajectory b2 == Just (lv, speed)) $  -- cleared in reqMelee
          execUpdAtomic $ UpdTrajectory aid (btrajectory b2) (Just (lv, speed))
    Just ([], _) -> do  -- non-projectile actor stops flying
      let !_A = assert (not $ bproj b) ()
      execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
    _ -> assert `failure` "Nothing trajectory" `twith` (aid, b)
