{-# LANGUAGE GADTs #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopM
  ( loopSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , factionArena, arenasForLoop, handleFidUpd, loopUpd, endClip
  , manageCalmAndDomination, applyPeriodicLevel
  , handleTrajectories, hTrajectories, setTrajectory
  , handleActors, hActors, restartGame
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Ord as Ord

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Server.CommonM
import           Game.LambdaHack.Server.EndM
import           Game.LambdaHack.Server.HandleEffectM
import           Game.LambdaHack.Server.HandleRequestM
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.PeriodicM
import           Game.LambdaHack.Server.ProtocolM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.StartM
import           Game.LambdaHack.Server.State

-- | Start a game session, including the clients, and then loop,
-- communicating with the clients.
--
-- The loop is started in server state that is empty, see 'emptyStateServer'.
loopSer :: (MonadServerAtomic m, MonadServerReadRequest m)
        => ServerOptions
             -- ^ player-supplied server options
        -> (Bool -> FactionId -> ChanServer -> IO ())
             -- ^ function that initializes a client and runs its main loop
        -> m ()
loopSer serverOptions executorClient = do
  -- Recover states and launch clients.
  modifyServer $ \ser -> ser { soptionsNxt = serverOptions
                             , soptions = serverOptions }
  cops <- getsState scops
  let updConn = updateConn executorClient
  restored <- tryRestore
  case restored of
    Just (sRaw, ser) | not $ snewGameSer serverOptions -> do  -- a restored game
      execUpdAtomic $ UpdResumeServer
                    $ updateCOpsAndCachedData (const cops) sRaw
      putServer ser {soptionsNxt = serverOptions}
      applyDebug
      factionD <- getsState sfactionD
      let f fid = let cmd = UpdResumeServer
                            $ updateCOpsAndCachedData (const cops)
                            $ sclientStates ser EM.! fid
                  in execUpdAtomicFidCatch fid cmd
      mapM_ f $ EM.keys factionD
      updConn
      initPer
      pers <- getsServer sperFid
      mapM_ (\fid -> sendUpdate fid $ UpdResume fid (pers EM.! fid))
            (EM.keys factionD)
      -- We dump RNG seeds here, based on @soptionsNxt@, in case the game
      -- wasn't run with @--dumpInitRngs@ previously, but we need the seeds,
      -- e.g., to diagnose a crash.
      rngs <- getsServer srngs
      when (sdumpInitRngs serverOptions) $ dumpRngs rngs
    _ -> do  -- starting new game for this savefile (--newGame or fresh save)
      s <- gameReset serverOptions Nothing Nothing
             -- get RNG from item boost
      -- Set up commandline options.
      let optionsBarRngs =
            serverOptions {sdungeonRng = Nothing, smainRng = Nothing}
      modifyServer $ \ser -> ser { soptionsNxt = optionsBarRngs
                                 , soptions = optionsBarRngs }
      execUpdAtomic $ UpdRestartServer s
      updConn
      initPer
      reinitGame
      writeSaveAll False
  loopUpd updConn

factionArena :: MonadStateRead m => Faction -> m (Maybe LevelId)
factionArena fact = case gleader fact of
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
arenasForLoop = do
  factionD <- getsState sfactionD
  marenas <- mapM factionArena $ EM.elems factionD
  let arenas = ES.toList $ ES.fromList $ catMaybes marenas
      !_A = assert (not (null arenas)
                    `blame` "game over not caught earlier"
                    `swith` factionD) ()
  return $! arenas

handleFidUpd :: (MonadServerAtomic m, MonadServerReadRequest m)
             => Bool -> (FactionId -> m ()) -> FactionId -> Faction -> m Bool
{-# INLINE handleFidUpd #-}
handleFidUpd True _ _ _ = return True
handleFidUpd False updatePerFid fid fact = do
  -- Update perception on all levels at once,
  -- in case a leader is changed to actor on another
  -- (possibly not even currently active) level.
  updatePerFid fid
  fa <- factionArena fact
  arenas <- getsServer sarenas
  -- Move a single actor, starting on arena with leader, if available.
  -- The boolean result says if clip was aborted (due to save, restart, etc.).
  -- If the clip was aborted, we have the guarantee game state was not
  -- changed and so we can save without risk of affecting gameplay.
  let handle [] = return False
      handle (lid : rest) = do
        nonWaitMove <- handleActors lid fid
        swriteSave <- getsServer swriteSave
        if | nonWaitMove -> return False
           | swriteSave -> return True
           | otherwise -> handle rest
      myArenas = case fa of
        Just myArena -> myArena : delete myArena arenas
        Nothing -> arenas
  handle myArenas

-- | Handle a clip (the smallest fraction of a game turn for which a frame may
-- potentially be generated). Run the leader and other actors moves.
-- Eventually advance the time and repeat.
loopUpd :: forall m. (MonadServerAtomic m, MonadServerReadRequest m)
        => m () -> m ()
loopUpd updConn = do
  let updatePerFid :: FactionId -> m ()
      {-# NOINLINE updatePerFid #-}
      updatePerFid fid = do  -- {-# SCC updatePerFid #-} do
        perValid <- getsServer $ (EM.! fid) . sperValidFid
        mapM_ (\(lid, valid) -> unless valid $ updatePer fid lid)
              (EM.assocs perValid)
      handleFid :: Bool -> (FactionId, Faction) -> m Bool
      {-# NOINLINE handleFid #-}
      handleFid aborted (fid, fact) = handleFidUpd aborted updatePerFid fid fact
      loopUpdConn = do
        factionD <- getsState sfactionD
        -- Start handling with the single UI faction, to safely save&exit.
        -- Note that this hack fails if there are many UI factions
        -- (when we reenable multiplayer). Then players will request
        -- save&exit and others will vote on it and it will happen
        -- after the clip has ended, not at the start.
        aborted <- foldM handleFid False (EM.toDescList factionD)
        unless aborted $ do
          -- Projectiles are processed last, so that the UI leader
          -- can save&exit before the state is changed and the clip
          -- needs to be carried through.
          arenas <- getsServer sarenas
          mapM_ (\fid -> mapM_ (`handleTrajectories` fid) arenas)
                (EM.keys factionD)
          endClip updatePerFid  -- must be last, in case performs a bkp save
        quit <- getsServer squit
        if quit then do
          modifyServer $ \ser -> ser {squit = False}
          endOrLoop loopUpdConn (restartGame updConn loopUpdConn)
                    (writeSaveAll True)
        else
          loopUpdConn
  loopUpdConn

-- | Handle the end of every clip. Do whatever has to be done
-- every fixed number of clips, e.g., monster generation.
-- Advance time. Perform periodic saves, if applicable.
--
-- This is never run if UI requested save or save&exit and it's correct,
-- because we know nobody moved and no time was or needs to be advanced
-- and arenas are not changed. If there was game exit,
-- on game resume the first clip is performed with empty arenas,
-- so arena time is not updated either and neither anybody moves,
-- nor anything happen, but arenas are here correctly updated
endClip :: forall m. MonadServerAtomic m => (FactionId -> m ()) -> m ()
{-# INLINE endClip #-}
endClip updatePerFid = do
  cops <- getsState scops
  let RuleKind{rwriteSaveClips, rleadLevelClips} = getStdRuleset cops
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r >= 5) r
  -- We don't send a lot of useless info to the client if the game has already
  -- ended. At best wasteful, at worst the player sees strange messages.
  quit <- getsServer squit
  unless quit $ do
    -- I need to send time updates, because I can't add time to each command,
    -- because I'd need to send also all arenas, which should be updated,
    -- and this is too expensive data for each, e.g., projectile move.
    -- I send even if nothing changes so that UI time display can progress.
    -- Possibly @arenas@ are invalid here, but all moves were performed
    -- according to this value, so time should be replenished according
    -- to this value as well.
    -- This is crucial, because tiny time discrepancies can accumulate
    -- magnified by hunders of actors that share the clip slots due to the
    -- restriction that at most 1 faction member acts each clip.
    arenas <- getsServer sarenas
    execUpdAtomic $ UpdAgeGame arenas
    -- Perform periodic dungeon maintenance.
    when (clipN `mod` rleadLevelClips == 0) leadLevelSwitch
    case clipN `mod` clipInTurn of
      2 ->
        -- Periodic activation only once per turn, for speed,
        -- but on all active arenas. Calm updates and domination
        -- happen there as well.
        applyPeriodicLevel
      4 ->
        -- Add monsters each turn, not each clip.
        spawnMonster
      _ -> return ()
  -- @applyPeriodicLevel@ might have, e.g., dominated actors, ending the game.
  -- It could not have unended the game, though.
  quit2 <- getsServer squit
  unless quit2 $ do
    -- Possibly a leader change due to @leadLevelSwitch@, so update arenas here
    -- for 100% accuracy at least at the start of actor moves, before they
    -- change leaders as part of their moves.
    --
    -- After game resume, this is the first non-vacuus computation.
    -- Next call to @loopUpdConn@ will really moves actors and updates arena
    -- time, so we start in exactly the same place that UI save ended in.
    validArenas <- getsServer svalidArenas
    unless validArenas $ do
      arenasNew <- arenasForLoop
      modifyServer $ \ser -> ser {sarenas = arenasNew, svalidArenas = True}
    -- Update all perception for visual feedback and to make sure saving
    -- and resuming game doesn't affect gameplay (by updating perception).
    -- Perception updates in @handleFidUpd@ are not enough, because
    -- periodic actions could have invalidated them.
    factionD <- getsState sfactionD
    mapM_ updatePerFid (EM.keys factionD)
    -- Periodic save needs to be at the end, so that restore can start
    -- at the beginning.
    when (clipN `mod` rwriteSaveClips == 0) $ writeSaveAll False

-- | Check if the given actor is dominated and update his calm.
manageCalmAndDomination :: MonadServerAtomic m => ActorId -> Actor -> m ()
manageCalmAndDomination aid b = do
  fact <- getsState $ (EM.! bfid b) . sfactionD
  hiImpression <- highestImpression aid
  dominated <-
    if bcalm b == 0
       && fleaderMode (gplayer fact) /= LeaderNull
            -- animals/robots/human drones never Calm-dominated
    then maybe (return False) (dominateFidSfx aid) hiImpression
    else return False
  unless dominated $ do
    ar <- getsState $ getActorAspect aid
    newCalmDelta <- getsState $ regenCalmDelta b ar
    unless (newCalmDelta == 0) $
      -- Update delta for the current player turn.
      udpateCalm aid newCalmDelta

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: MonadServerAtomic m => m ()
applyPeriodicLevel = do
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
            itemFull@ItemFull{itemKind} <- getsState $ itemToFull iid
            when (IK.Periodic `elem` IK.ifeature itemKind) $
              -- In periodic activation, consider *only* recharging effects.
              -- Activate even if effects null, to possibly destroy item.
              effectAndDestroy False aid aid iid (CActor aid cstore) True
                               (IK.filterRecharging $ IK.ieffects itemKind)
                               (itemFull, kit)
      applyPeriodicActor (aid, b) =
        when (not (bproj b) && blid b `ES.member` arenasSet) $ do
          mapM_ (applyPeriodicItem aid COrgan borgan) $ EM.assocs $ borgan b
          mapM_ (applyPeriodicItem aid CEqp beqp) $ EM.assocs $ beqp b
          -- While we are at it, also update their calm.
          manageCalmAndDomination aid b
  allActors <- getsState sactorD
  mapM_ applyPeriodicActor $ EM.assocs allActors

handleTrajectories :: MonadServerAtomic m => LevelId -> FactionId -> m ()
handleTrajectories lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  getActorB <- getsState $ flip getActorBody
  let l = map (fst . snd)
          $ sortBy (Ord.comparing fst)
          $ filter (\(_, (_, b)) -> isJust (btrajectory b) || bhp b <= 0)
          $ map (\(a, atime) -> (atime, (a, getActorB a)))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  -- The actor body obtained above may be outdated before @hTrajectories@
  -- call (due to other actors following their trajectories),
  -- so it's only used to decide which actors are processed in this
  -- @handleTrajectories@ call and not passed to @hTrajectories@.
  -- If the actor no longer fulfills the criteria above, @hTrajectories@
  -- ignores it. If it starts fulfilling them, the recursive call
  -- to @handleTrajectories@ will detect that and process him later on.
  -- If the actor is no longer on the level or no longer belongs
  -- to the faction, it is nevertheless processed without a problem.
  -- We are guaranteed the actor still exists.
  mapM_ hTrajectories l
  unless (null l) $ handleTrajectories lid fid  -- for speeds > tile/clip

hTrajectories :: MonadServerAtomic m => ActorId -> m ()
{-# INLINE hTrajectories #-}
hTrajectories aid = do
  b1 <- getsState $ getActorBody aid
  if | actorDying b1 -> dieSer aid b1
     | isJust (btrajectory b1) -> do  -- don't advance time if no trajectory
       setTrajectory aid b1
       -- @setTrajectory@ might have affected @actorDying@, so we check again
       -- ASAP to make sure the body of the projectile (or pushed actor)
       -- doesn't block movement of other actors, but vanishes promptly.
       -- Bodies of actors that die not flying remain on the battlefied until
       -- their natural next turn, to give them a chance of rescue.
       -- Note that domination of pushed actors is not checked
       -- nor is their calm updated. They are helpless wrt movement,
       -- but also invulnerable in this respect.
       b2 <- getsState $ getActorBody aid
       if actorDying b2 then dieSer aid b2 else advanceTime aid 100 False
     | otherwise -> return ()  -- no longer fulfills citeria, ignore him
  -- if @actorDying@ due to @bhp b <= 0@:
  -- If @b@ is a projectile, it means hits an actor or is hit by actor.
  -- Then the carried item is destroyed and that's all.
  -- If @b@ is not projectile, it dies, his items drop to the ground
  -- and possibly a new leader is elected.
  --
  -- if @actorDying@ due to @btrajectory@ null:
  -- A projectile drops to the ground due to obstacles or range.
  -- The carried item is not destroyed, unless it's fragile,
  -- but drops to the ground.

-- | Manage trajectory of a projectile.
--
-- Colliding with a wall or actor doesn't take time, because
-- the projectile does not move (the move is blocked).
-- Not advancing time forces dead projectiles to be destroyed ASAP.
-- Otherwise, with some timings, it can stay on the game map dead,
-- blocking path of human-controlled actors and alarming the hapless human.
setTrajectory :: MonadServerAtomic m => ActorId -> Actor -> m ()
{-# INLINE setTrajectory #-}
setTrajectory aid b = do
  COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel $ blid b
  case btrajectory b of
    Just (d : lv, speed) -> do
      let tpos = bpos b `shift` d  -- target position
      if Tile.isWalkable coTileSpeedup $ lvl `at` tpos
      then do
        -- Hit clears trajectory of non-projectiles in reqMelee so no need here.
        -- Non-projectiles displace, to make pushing in crowds less lethal
        -- and chaotic and to avoid hitting harpoons when pulled by them.
        case posToAidsLvl tpos lvl of
          [target] | not (bproj b) -> reqDisplace aid target
          _ -> reqMove aid d
        b2 <- getsState $ getActorBody aid
        unless ((fst <$> btrajectory b2) == Just []) $  -- set in reqMelee
          execUpdAtomic $ UpdTrajectory aid (btrajectory b2) (Just (lv, speed))
      else do
        -- @Nothing@ trajectory of a projectile signals an obstacle hit.
        -- The second call of @actorDying@ above will catch the dead projectile.
        execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
        if bproj b then do
          -- Lose HP due to hitting an obstacle.
          when (bhp b > oneM) $ do
            execUpdAtomic $ UpdRefillHP aid minusM
        else do
          execSfxAtomic $ SfxCollideTile aid tpos
          mfail <- reqAlterFail aid tpos
          case mfail of
            Nothing -> return ()  -- too late to announce anything
            Just{} ->
              -- Altering failed, probably just a wall, so lose HP
              -- due to being pushed into an obstacle. Never kill in this way.
              when (bhp b > oneM) $ do
                execUpdAtomic $ UpdRefillHP aid minusM
                let effect = IK.RefillHP (-2)  -- -2 is a lie to ensure display
                execSfxAtomic $ SfxEffect (bfid b) aid effect (-1)
    Just ([], _) ->
      -- Non-projectile actor stops flying (a projectile with empty trajectory
      -- would be intercepted earlier on as dead).
      assert (not $ bproj b)
      $ execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
    _ -> error $ "Nothing trajectory" `showFailure` (aid, b)

handleActors :: (MonadServerAtomic m, MonadServerReadRequest m)
             => LevelId -> FactionId -> m Bool
handleActors lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  getActorB <- getsState $ flip getActorBody
  let l = map (fst . snd)
          $ sortBy (Ord.comparing fst)
          $ filter (\(_, (_, b)) -> isNothing (btrajectory b) && bhp b > 0)
          $ map (\(a, atime) -> (atime, (a, getActorB a)))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  -- The actor body obtained above may be outdated before @hActors@
  -- call gets to it (due to other actors on the list acting),
  -- so it's only used to decide which actors are processed in this call.
  -- If the actor is no longer on the level or no longer belongs
  -- to the faction, it is nevertheless processed without a problem
  -- (the client may act wrt slightly outdated Perception and that's all).
  -- We are guaranteed the actor still exists.
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  -- Leader acts first, so that UI leader can save&exit before state changes.
  hActors $ case mleader of
    Just aid | aid `elem` l -> aid : delete aid l
    _ -> l

hActors :: forall m. (MonadServerAtomic m, MonadServerReadRequest m)
        => [ActorId] -> m Bool
hActors [] = return False
hActors as@(aid : rest) = do
  b1 <- getsState $ getActorBody aid
  let side = bfid b1
      !_A = assert (not $ bproj b1) ()
  fact <- getsState $ (EM.! side) . sfactionD
  squit <- getsServer squit
  let mleader = gleader fact
      aidIsLeader = mleader == Just aid
      mainUIactor = fhasUI (gplayer fact)
                    && (aidIsLeader
                        || fleaderMode (gplayer fact) == LeaderNull)
      -- Checking squit, to avoid doubly setting faction status to Camping.
      mainUIunderAI = mainUIactor && isAIFact fact && not squit
      doQueryAI = not mainUIactor || isAIFact fact
  when mainUIunderAI $ do
    cmdS <- sendQueryUI side aid
    case fst cmdS of
      ReqUINop -> return ()
      ReqUIAutomate -> execUpdAtomic $ UpdAutoFaction side False
      ReqUIGameExit -> do
        reqGameExit aid
        -- This is not proper UI-forced save, but a timeout, so don't save
        -- and no need to abort turn.
        modifyServer $ \ser -> ser {swriteSave = False}
      _ -> error $ "" `showFailure` cmdS
  let mswitchLeader :: Maybe ActorId -> m ActorId
      {-# NOINLINE mswitchLeader #-}
      mswitchLeader (Just aidNew) = switchLeader side aidNew >> return aidNew
      mswitchLeader Nothing = return aid
  (aidNew, mtimed) <-
    if doQueryAI then do
      (cmd, maid) <- sendQueryAI side aid
      aidNew <- mswitchLeader maid
      mtimed <- handleRequestAI cmd
      return (aidNew, mtimed)
    else do
      (cmd, maid) <- sendQueryUI side aid
      aidNew <- mswitchLeader maid
      mtimed <- handleRequestUI side aidNew cmd
      return (aidNew, mtimed)
  case mtimed of
    Just (RequestAnyAbility timed) -> do
      nonWaitMove <- handleRequestTimed side aidNew timed
      -- Even if the actor got a free turn of time via a scroll,
      -- he will not act again this clip, only next clip.
      -- Clip is small, so not a big deal and it's faster and avoids
      -- complete game time freezes, e.g., due to an exploit.
      if nonWaitMove then return True else hActors rest
    Nothing -> do
      swriteSave <- getsServer swriteSave
      if swriteSave then return False else hActors as

restartGame :: MonadServerAtomic m
            => m () -> m () -> Maybe (GroupName ModeKind) -> m ()
restartGame updConn loop mgameMode = do
  soptionsNxt <- getsServer soptionsNxt
  srandom <- getsServer srandom
  s <- gameReset soptionsNxt mgameMode (Just srandom)
  let optionsBarRngs = soptionsNxt {sdungeonRng = Nothing, smainRng = Nothing}
  modifyServer $ \ser -> ser { soptionsNxt = optionsBarRngs
                             , soptions = optionsBarRngs }
  execUpdAtomic $ UpdRestartServer s
  updConn
  initPer
  reinitGame
  writeSaveAll False
  loop
