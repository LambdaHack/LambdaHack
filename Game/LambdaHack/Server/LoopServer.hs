-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopServer (loopSer) where

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Frontend
import Game.LambdaHack.Server.EndServer
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.ProtocolServer
import Game.LambdaHack.Server.StartServer
import Game.LambdaHack.Server.State

-- | Start a game session. Loop, communicating with clients.
loopSer :: (MonadAtomic m, MonadServerReadRequest m)
        => DebugModeSer
        -> (Request -> m Bool)
        -> (FactionId -> ChanFrontend -> ChanServer ResponseUI Request -> IO ())
        -> (FactionId -> ChanServer ResponseAI RequestTimed -> IO ())
        -> Kind.COps
        -> m ()
loopSer sdebug handleRequest executorUI executorAI !cops = do
  -- Recover states and launch clients.
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser) | not $ snewGameSer sdebug -> do  -- run a restored game
      -- First, set the previous cops, to send consistent info to clients.
      let setPreviousCops = const cops
      execUpdAtomic $ UpdResumeServer $ updateCOps setPreviousCops sRaw
      putServer ser
      sdebugNxt <- initDebug cops sdebug
      modifyServer $ \ser2 -> ser2 {sdebugNxt}
      applyDebug
      updateConn executorUI executorAI
      initPer
      pers <- getsServer sper
      broadcastUpdAtomic $ \fid -> UpdResume fid (pers EM.! fid)
      -- Second, set the current cops and reinit perception.
      let setCurrentCops = const (speedupCOps (sallClear sdebugNxt) cops)
      -- @sRaw@ is correct here, because none of the above changes State.
      execUpdAtomic $ UpdResumeServer $ updateCOps setCurrentCops sRaw
      -- We dump RNG seeds here, in case the game wasn't run
      -- with --dumpInitRngs previously and we need to seeds.
      when (sdumpInitRngs sdebug) $ dumpRngs
    _ -> do  -- Starting a new game.
      -- Set up commandline debug mode
      let mrandom = case restored of
            Just (_, ser) -> Just $ srandom ser
            Nothing -> Nothing
      s <- gameReset cops sdebug mrandom
      sdebugNxt <- initDebug cops sdebug
      let debugBarRngs = sdebugNxt {sdungeonRng = Nothing, smainRng = Nothing}
      modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                                 , sdebugSer = debugBarRngs }
      let speedup = speedupCOps (sallClear sdebugNxt)
      execUpdAtomic $ UpdRestartServer $ updateCOps speedup s
      updateConn executorUI executorAI
      initPer
      reinitGame
      saveBkpAll False
  resetSessionStart
  -- Start a clip (a part of a turn for which one or more frames
  -- will be generated). Do whatever has to be done
  -- every fixed number of time units, e.g., monster generation.
  -- Run the leader and other actors moves. Eventually advance the time
  -- and repeat.
  let loop = do
        let factionArena fact = do
              case gleader fact of
               -- Even spawners and horrors need an active arena
               -- for their leader, or they start clogging stairs.
               Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
               Nothing -> return Nothing
        factionD <- getsState sfactionD
        marenas <- mapM factionArena $ EM.elems factionD
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        assert (not $ null arenas) skip  -- game over not caught earlier
        mapM_ (handleActors handleRequest) arenas
        quit <- getsServer squit
        if quit then do
          -- In case of game save+exit or restart, don't age levels (endClip)
          -- since possibly not all actors have moved yet.
          modifyServer $ \ser -> ser {squit = False}
          let updConn = updateConn executorUI executorAI
          endOrLoop loop (restartGame updConn loop) saveAndExit
        else do
          continue <- endClip arenas
          when continue loop
  loop

endClip :: (MonadAtomic m, MonadServer m, MonadServerReadRequest m)
        => [LevelId] -> m Bool
endClip arenas = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      saveBkpClips = rsaveBkpClips stdRuleset
      leadLevelClips = rleadLevelClips stdRuleset
  -- TODO: a couple messages each clip to many clients is too costly.
  -- Store these on a queue and sum times instead of sending,
  -- until a different command needs to be sent. Include HealActorA
  -- from regenerateLevelHP, but keep it before AgeGameA.
  -- TODO: this is also needed to keep savefiles small (undo info).
  mapM_ (\lid -> execUpdAtomic $ UpdAgeLevel lid timeClip) arenas
  execUpdAtomic $ UpdAgeGame timeClip
  -- Perform periodic dungeon maintenance.
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r > 2) r
      clipMod = clipN `mod` clipInTurn
  bkpSave <- getsServer sbkpSave
  when (bkpSave || clipN `mod` saveBkpClips == 0) $ do
    modifyServer $ \ser -> ser {sbkpSave = False}
    saveBkpAll False
  when (clipN `mod` leadLevelClips == 0) leadLevelFlip
  -- Regenerate HP and add monsters each turn, not each clip.
  -- Do this on only one of the arenas to prevent micromanagement,
  -- e.g., spreading leaders across levels to bump monster generation.
  if clipMod == 1 then do
    arena <- rndToAction $ oneOf arenas
    regenerateLevelHP arena
    generateMonster arena
    stopAfter <- getsServer $ sstopAfter . sdebugSer
    case stopAfter of
      Nothing -> return True
      Just stopA -> do
        exit <- elapsedSessionTimeGT stopA
        if exit then do
          tellAllClipPS
          saveAndExit
          return False  -- don't re-enter the game loop
        else return True
  else return True

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less than or equal to the current level time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. But most often we push exactly one frame or frame delay.
handleActors :: (MonadAtomic m, MonadServerReadRequest m)
             => (Request -> m Bool)
             -> LevelId
             -> m ()
handleActors handleRequest lid = do
  time <- getsState $ getLocalTime lid  -- the end of this clip, inclusive
  Level{lprio} <- getLevel lid
  quit <- getsServer squit
  factionD <- getsState sfactionD
  s <- getState
  let -- Actors of the same faction move together.
      -- TODO: insert wrt the order, instead of sorting
      isLeader (aid, b) = Just aid /= gleader (factionD EM.! bfid b)
      order = Ord.comparing $
        ((>= 0) . bhp . snd) &&& bfid . snd &&& isLeader &&& bsymbol . snd
      (atime, as) = EM.findMin lprio
      ams = map (\a -> (a, getActorBody a s)) as
      mnext | EM.null lprio = Nothing  -- no actor alive, wait until it spawns
            | otherwise = if atime > time
                          then Nothing  -- no actor is ready for another move
                          else Just $ minimumBy order ams
  case mnext of
    _ | quit -> return ()
    Nothing -> return ()
    Just (aid, b) | bhp b < 0 && bproj b -> do
      -- A projectile hits an actor. The carried item is destroyed.
      -- TODO: perhaps don't destroy if no effect (NoEffect),
      -- to help testing items. But OTOH, we want most items to have
      -- some effect, even silly, for flavour. Anyway, if the silly
      -- effect identifies an item, the hit is not wasted, so this makes sense.
      dieSer aid b True
      -- The attack animation for the projectile hit subsumes @DisplayPushD@,
      -- so not sending an extra @DisplayPushD@ here.
      handleActors handleRequest lid
    Just (aid, b) | maybe False null (btrajectory b) -> do
      assert (bproj b) skip
      execSfxAtomic $ SfxDisplayPush (bfid b)  -- show last position before drop
      -- A projectile drops to the ground due to obstacles or range.
      dieSer aid b False
      handleActors handleRequest lid
    Just (aid, b) | bhp b <= 0 && not (bproj b) -> do
      -- An actor dies. Items drop to the ground
      -- and possibly a new leader is elected.
      dieSer aid b False
      -- The death animation subsumes @DisplayPushD@, so not sending it here.
      handleActors handleRequest lid
    Just (aid, body) -> do
      let side = bfid body
          fact = factionD EM.! side
          mleader = gleader fact
          aidIsLeader = mleader == Just aid
      queryUI <-
        if aidIsLeader && playerUI (gplayer fact) then do
          let hasAiLeader = playerAiLeader $ gplayer fact
          if hasAiLeader then do
            -- If UI client for the faction completely under AI control,
            -- ping often to sync frames and to catch ESC,
            -- which switches off Ai control.
            sendPingUI side
            fact2 <- getsState $ (EM.! side) . sfactionD
            let hasAiLeader2 = playerAiLeader $ gplayer fact2
            return $! not hasAiLeader2
          else return True
        else return False
      let switchLeader cmdS = do
            -- TODO: check that the command is legal first, report and reject,
            -- but do not crash (currently server asserts things and crashes)
            let aidNew = aidOfRequest cmdS
            bPre <- getsState $ getActorBody aidNew
            let leadAtoms =
                  if aidNew /= aid  -- switched, so aid must be leader
                  then -- Only a leader can change his faction's leader
                       -- before the action is performed (e.g., via AI
                       -- switching leaders). Then, the action can change
                       -- the leader again (e.g., via killing the old leader).
                       assert (aidIsLeader
                               && not (bproj bPre)
                               && not (isSpawnFact fact)
                               `blame` (aid, body, aidNew, bPre, cmdS, fact))
                         [UpdLeadFaction side mleader (Just aidNew)]
                  else []
            mapM_ execUpdAtomic leadAtoms
            assert (bfid bPre == side
                    `blame` "client tries to move other faction actors"
                    `twith` (bPre, side)) skip
            return (aidNew, bPre)
          setBWait (ReqTimed ReqWait{}) aidNew bPre = do
            let fromWait = bwait bPre
            unless fromWait $ execUpdAtomic $ UpdWaitActor aidNew fromWait True
          setBWait _ aidNew bPre = do
            let fromWait = bwait bPre
            when fromWait $ execUpdAtomic $ UpdWaitActor aidNew fromWait False
          extraFrames bPre = do
            -- Generate extra frames if the actor has already moved during
            -- this clip, so his multiple moves would be collapsed
            -- in one frame.
            -- If the actor changes his speed this very turn,
            -- the test can fail, but it's a minor UI issue, so let it be.
            let previousClipEnd = timeAdd time $ timeNegate timeClip
                lastSingleMove = timeAddFromSpeed bPre previousClipEnd
            when (btime bPre > lastSingleMove) $
              broadcastSfxAtomic SfxDisplayPush
      if bproj body then do  -- TODO: perhaps check Track, not bproj
        execSfxAtomic $ SfxDisplayPush side
        let cmdS = ReqTimed $ ReqSetTrajectory aid
        timed <- handleRequest cmdS
        assert timed skip
        b <- getsState $ getActorBody aid
        -- Colliding with a wall or actor doesn't take time, because
        -- the projectile does not move (the move is blocked).
        -- Not advancing time forces dead projectiles to be destroyed ASAP.
        -- Otherwise it would be displayed in the same place twice.
        -- If ever needed this can be implemented properly by moving
        -- SetTrajectorySer out of RequestTimed.
        unless (bhp b < 0 || maybe False null (btrajectory b)) $ do
          advanceTime aid
          extraFrames b
      else if queryUI then do
        -- The client always displays a frame in this case.
        cmdS <- sendQueryUI side aid
        (aidNew, bPre) <- switchLeader cmdS
        timed <-
          if bhp bPre <= 0 && not (bproj bPre) then do
            execSfxAtomic
              $ SfxMsgFid side "You strain, fumble and faint from the exertion."
            return False
          else handleRequest cmdS
        setBWait cmdS aidNew bPre
        -- Advance time once, after the leader switched perhaps many times.
        -- TODO: this is correct only when all heroes have the same
        -- speed and can't switch leaders by, e.g., aiming a wand
        -- of domination. We need to generalize by displaying
        -- "(next move in .3s [RET]" when switching leaders.
        -- RET waits .3s and gives back control,
        -- Any other key does the .3s wait and the action from the key
        -- at once.
        when timed $ advanceTime aidNew
        extraFrames bPre
      else do
        -- Order the UI client (if any) corresponding to the AI client
        -- to display a new frame so that player does not see moves
        -- of all his AI party members cumulated in a single frame,
        -- but one by one.
        when (playerUI $ gplayer fact) $
          execSfxAtomic $ SfxDisplayPush side
        -- Clear messages in the UI client (if any), if the actor
        -- is a leader (which happens when a UI client is fully
        -- computer-controlled). We could record history more often,
        -- to avoid long reports, but we'd have to add -more- prompts.
        let mainUIactor = playerUI (gplayer fact) && aidIsLeader
        when mainUIactor $ execSfxAtomic $ SfxRecordHistory side
        cmdTimed <- sendQueryAI side aid
        let cmdS = ReqTimed cmdTimed
        (aidNew, bPre) <- switchLeader cmdS
        assert (not (bhp bPre <= 0 && not (bproj bPre))
                `blame` "AI switches to an incapacitated actor"
                `twith` (cmdS, bPre, side)) skip
        timed <- handleRequest cmdS
        assert timed skip
        setBWait cmdS aidNew bPre
        -- AI always takes time and so doesn't loop.
        advanceTime aidNew
        extraFrames bPre
      handleActors handleRequest lid

saveAndExit :: (MonadAtomic m, MonadServerReadRequest m) => m ()
saveAndExit = do
  cops <- getsState scops
  -- Save client and server data.
  saveBkpAll True
  -- debugPrint "Server saves game before exit"
  -- Kill all clients, including those that did not take part
  -- in the current game.
  -- Clients exit not now, but after they print all ending screens.
  -- debugPrint "Server kills clients"
  killAllClients
  -- Verify that the saved perception is equal to future reconstructed.
  persSaved <- getsServer sper
  fovMode <- getsServer $ sfovMode . sdebugSer
  pers <- getsState $ dungeonPerception cops
                                        (fromMaybe (Digital 12) fovMode)
  assert (persSaved == pers `blame` "wrong saved perception"
                            `twith` (persSaved, pers)) skip

restartGame :: (MonadAtomic m, MonadServerReadRequest m)
            => m () -> m () -> m ()
restartGame updConn loop = do
  tellGameClipPS
  cops <- getsState scops
  sdebugNxt <- getsServer sdebugNxt
  srandom <- getsServer srandom
  s <- gameReset cops sdebugNxt $ Just srandom
  let debugBarRngs = sdebugNxt {sdungeonRng = Nothing, smainRng = Nothing}
  modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                             , sdebugSer = debugBarRngs }
  execUpdAtomic $ UpdRestartServer s
  updConn
  initPer
  reinitGame
  saveBkpAll False
  loop

-- TODO: This can be improved by adding a timeout
-- and by asking clients to prepare
-- a save (in this way checking they have permissions, enough space, etc.)
-- and when all report back, asking them to commit the save.
-- | Save game on server and all clients. Clients are pinged first,
-- which greatly reduced the chance of saves being out of sync.
saveBkpAll :: (MonadAtomic m, MonadServerReadRequest m) => Bool -> m ()
saveBkpAll unconditional = do
  bench <- getsServer $ sbenchmark . sdebugSer
  when (unconditional || not bench) $ do
    factionD <- getsState sfactionD
    let ping fid _ = do
          sendPingAI fid
          when (playerUI $ gplayer $ factionD EM.! fid) $ sendPingUI fid
    mapWithKeyM_ ping factionD
    execUpdAtomic UpdSaveBkp
    saveServer
