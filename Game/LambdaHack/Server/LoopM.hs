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
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
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
        -> Config
        -> (Maybe SessionUI -> Kind.COps -> FactionId -> ChanServer -> IO ())
             -- ^ the code to run for UI clients
        -> m ()
loopSer sdebug sconfig executorClient = do
  -- Recover states and launch clients.
  cops <- getsState scops
  let updConn = updateConn cops sconfig executorClient
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser) | not $ snewGameSer sdebug -> do  -- a restored game
      execUpdAtomic $ UpdResumeServer $ updateCOps (const cops) sRaw
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
      -- with --dumpInitRngs previously and we need the seeds.
      rngs <- getsServer srngs
      when (sdumpInitRngs sdebug) $ dumpRngs rngs
    _ -> do  -- Starting the first new game for this savefile.
      -- Set up commandline debug mode
      let mrandom = case restored of
            Just (_, ser) -> Just $ srandom ser
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
                    `twith` factionD) ()
  return $! arenas

handleFidUpd :: (MonadAtomic m, MonadServerReadRequest m)
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
  -- The boolean result says if turn was aborted (due to save, restart, etc.).
  -- If the turn was aborted, we have the guarantee game state was not
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

-- | Handle a clip (a part of a turn for which one or more frames
-- will be generated). Run the leader and other actors moves.
-- Eventually advance the time and repeat.
loopUpd :: forall m. (MonadAtomic m, MonadServerReadRequest m) => m () -> m ()
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
        -- after the turn has ended, not at the start.
        aborted <- foldM handleFid False (EM.toDescList factionD)
        unless aborted $ do
          -- Projectiles are processed last, so that the UI leader
          -- can save&exit before the state is changed and the turn
          -- needs to be carried through.
          arenas <- getsServer sarenas
          mapM_ (\fid -> mapM_ (\lid ->
            handleTrajectories lid fid) arenas) (EM.keys factionD)
          endClip updatePerFid  -- must be last, in case it performs a bkp save
        quit <- getsServer squit
        if quit then do
          modifyServer $ \ser -> ser {squit = False}
          endOrLoop loopUpdConn (restartGame updConn loopUpdConn)
                    gameExit (writeSaveAll True)
        else
          loopUpdConn
  loopUpdConn

-- | Handle the end of every clip. Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Advance time. Perform periodic saves, if applicable.
endClip :: forall m. (MonadAtomic m, MonadServer m)
        => (FactionId -> m ()) -> m ()
{-# INLINE endClip #-}
endClip updatePerFid = do
  Kind.COps{corule} <- getsState scops
  let RuleKind{rwriteSaveClips, rleadLevelClips} = Kind.stdRuleset corule
  time <- getsState stime
  let clipN = time `timeFit` timeClip
      clipInTurn = let r = timeTurn `timeFit` timeClip
                   in assert (r >= 5) r
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
      -- but on all active arenas. Calm updates and domination
      -- happen there as well.
      applyPeriodicLevel
    4 ->
      -- Add monsters each turn, not each clip.
      spawnMonster
    _ -> return ()
  -- Update all perception for visual feedback and to make sure
  -- saving a game doesn't affect gameplay (by updating perception).
  factionD <- getsState sfactionD
  mapM_ updatePerFid (EM.keys factionD)
  -- Save needs to be at the end, so that restore can start at the beginning.
  when (clipN `mod` rwriteSaveClips == 0) $ writeSaveAll False

-- | Check if the given actor is dominated and update his calm.
manageCalmAndDomination :: (MonadAtomic m, MonadServer m)
                        => ActorId -> Actor -> m ()
manageCalmAndDomination aid b = do
  fact <- getsState $ (EM.! bfid b) . sfactionD
  dominated <-
    if bcalm b == 0
       && bfidImpressed b /= bfid b
       && fleaderMode (gplayer fact) /= LeaderNull
            -- animals/robots never Calm-dominated
    then dominateFidSfx (bfidImpressed b) aid
    else return False
  unless dominated $ do
    actorAspect <- getsServer sactorAspect
    let ar = actorAspect EM.! aid
    newCalmDelta <- getsState $ regenCalmDelta b ar
    unless (newCalmDelta == 0) $
      -- Update delta for the current player turn.
      udpateCalm aid newCalmDelta

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: (MonadAtomic m, MonadServer m) => m ()
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
          -- While we are at it, also update their calm.
          manageCalmAndDomination aid b
  allActors <- getsState sactorD
  mapM_ applyPeriodicActor $ EM.assocs allActors

handleTrajectories :: (MonadAtomic m, MonadServer m)
                   => LevelId -> FactionId -> m ()
handleTrajectories lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  s <- getState
  let l = sortBy (Ord.comparing fst)
          $ filter (\(_, (_, b)) -> isJust (btrajectory b) || bhp b <= 0)
          $ map (\(a, atime) -> (atime, (a, getActorBody a s)))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  mapM_ (hTrajectories . snd) l
  unless (null l) $ handleTrajectories lid fid  -- for speeds > tile/clip

-- The body @b@ may be outdated by this time
-- (due to other actors following their trajectories)
-- but we decide death inspecting it --- last moment rescue
-- from projectiles or pushed actors doesn't work; too late.
-- Even if the actor got teleported to another level by this point,
-- we don't care, we set the trajectory, check death, etc.
hTrajectories :: (MonadAtomic m, MonadServer m) => (ActorId, Actor) -> m ()
{-# INLINE hTrajectories #-}
hTrajectories (aid, b) = do
  b2 <- if actorDying b then return b else do
          setTrajectory aid
          getsState $ getActorBody aid
  -- @setTrajectory@ might have affected @actorDying@, so we check again ASAP
  -- to make sure the body of the projectile (or pushed actor)
  -- doesn't block movement of other actors, but vanishes promptly.
  -- Bodies of actors that die in place remain on the battlefied until
  -- their natural next turn, to give them a chance of rescue.
  -- Note that domination of pushed actors is not checked
  -- nor is their calm updated. They are helpless wrt movement,
  -- but also invulnerable in this repsect.
  if actorDying b2 then dieSer aid b2 else advanceTime aid
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
setTrajectory :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
{-# INLINE setTrajectory #-}
setTrajectory aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  case btrajectory b of
    Just (d : lv, speed) ->
      if accessibleDir cops lvl (bpos b) d
      then do
        when (bproj b && length lv <= 1) $ do
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
        unless ((fst <$> btrajectory b2) == Just []) $  -- set in reqMelee
          execUpdAtomic $ UpdTrajectory aid (btrajectory b2) (Just (lv, speed))
      else do
        -- Lose HP due to flying into an obstacle.
        unless (bproj b) $  -- prevent spam
          execUpdAtomic $ UpdRefillHP aid minusM
        -- Nothing from non-empty trajectories signifies obstacle hit.
        execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
    Just ([], _) ->
      -- Non-projectile actor stops flying.
      assert (not $ bproj b)
      $ execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
    _ -> assert `failure` "Nothing trajectory" `twith` (aid, b)

handleActors :: (MonadAtomic m, MonadServerReadRequest m)
             => LevelId -> FactionId -> m Bool
handleActors lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  factionD <- getsState sfactionD
  s <- getState
  -- Leader acts first, so that UI leader can save&exit before state changes.
  let notLeader (aid, b) = Just aid /= gleader (factionD EM.! bfid b)
      l = sortBy (Ord.comparing $ \(aid, b) ->
                   (notLeader (aid, b), bsymbol b /= '@', bsymbol b, bcolor b))
          $ filter (\(_, b) -> isNothing (btrajectory b) && bhp b > 0)
          $ map (\(a, _) -> (a, getActorBody a s))
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  hActors fid l

hActors :: forall m. (MonadAtomic m, MonadServerReadRequest m)
        => FactionId -> [(ActorId, Actor)] -> m Bool
hActors _ [] = return False
hActors fid as@((aid, body) : rest) = do
  let side = bfid body
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
      _ -> assert `failure` cmdS  -- TODO: handle more
    -- Clear messages in the UI client, regardless if leaderless or not.
    execUpdAtomic $ UpdRecordHistory side
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
    -- TODO: check that the command is legal first, report and reject,
    -- but do not crash (currently server asserts things and crashes)
    Just (RequestAnyAbility timed) -> do
      nonWaitMove <- handleRequestTimed side aidNew timed
      if nonWaitMove then return True else hActors fid rest
    Nothing -> do
      swriteSave <- getsServer swriteSave
      if swriteSave then return False else hActors fid as

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

restartGame :: (MonadAtomic m, MonadServer m)
            => m () -> m () -> Maybe (GroupName ModeKind) -> m ()
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
writeSaveAll :: (MonadAtomic m, MonadServer m) => Bool -> m ()
writeSaveAll uiRequested = do
  bench <- getsServer $ sbenchmark . sdebugCli . sdebugSer
  when (uiRequested || not bench) $ do
    execUpdAtomic UpdWriteSave
    saveServer
