-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopM
  ( loopSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , factionArena, arenasForLoop, handleFidUpd, loopUpd, endClip
  , manageCalmAndDomination, applyPeriodicLevel
  , handleTrajectories, hTrajectories, advanceTrajectory
  , handleActors, hActors, handleUIunderAI, dieSer, restartGame
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client (ReqUI (..), Response (..))
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.CommonM
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
loopSer :: (MonadServerAtomic m, MonadServerComm m)
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
  let updConn startsNewGame = updateConn $ executorClient startsNewGame
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
      mapM_ (void <$> f) $ EM.keys factionD
      updConn False
      initPer
      pers <- getsServer sperFid
      let clear = const emptyPer
          persFid fid | sknowEvents serverOptions = EM.map clear (pers EM.! fid)
                      | otherwise = pers EM.! fid
      mapM_ (\fid -> sendUpdate fid $ UpdResume fid (persFid fid))
            (EM.keys factionD)
      arenasNew <- arenasForLoop
      modifyServer $ \ser2 -> ser2 {sarenas = arenasNew, svalidArenas = True}
      -- We dump RNG seeds here, based on @soptionsNxt@, in case the game
      -- wasn't run with @--dumpInitRngs@ previously, but we need the seeds,
      -- e.g., to diagnose a crash.
      rngs <- getsServer srngs
      when (sdumpInitRngs serverOptions) $ dumpRngs rngs
    _ -> do  -- starting new game for this savefile (--newGame or fresh save)
      factionDold <- getsState sfactionD
      s <- gameReset serverOptions Nothing Nothing
             -- get RNG from item boost
      -- Set up commandline options.
      let optionsBarRngs =
            serverOptions {sdungeonRng = Nothing, smainRng = Nothing}
      modifyServer $ \ser -> ser { soptionsNxt = optionsBarRngs
                                 , soptions = optionsBarRngs }
      execUpdAtomic $ UpdRestartServer s
      updConn True
      initPer
      reinitGame factionDold
      writeSaveAll False False
  loopUpd $ updConn True

factionArena :: MonadStateRead m => Faction -> m (Maybe LevelId)
factionArena fact = case gleader fact of
  -- Even spawners need an active arena for their leader,
  -- or they start clogging stairs.
  Just leader -> do
    b <- getsState $ getActorBody leader
    return $ Just $ blid b
  Nothing -> return Nothing
    -- This means Allure heroes can kill all aliens on lvl 4, retreat,
    -- hide and sleep on lvl 3 and they are guaranteed aliens don't spawn.
    -- However, animals still spawn, if slowly, and aliens resume
    -- spawning when heroes move on again.

arenasForLoop :: MonadStateRead m => m (ES.EnumSet LevelId)
{-# INLINE arenasForLoop #-}
arenasForLoop = do
  factionD <- getsState sfactionD
  marenas <- mapM factionArena $ EM.elems factionD
  let arenas = ES.fromList $ catMaybes marenas
      !_A = assert (not (ES.null arenas)
                    `blame` "game over not caught earlier"
                    `swith` factionD) ()
  return $! arenas

handleFidUpd :: forall m. (MonadServerAtomic m, MonadServerComm m)
             => (FactionId -> m ()) -> FactionId -> Faction -> m ()
{-# INLINE handleFidUpd #-}
handleFidUpd updatePerFid fid fact = do
  -- Update perception on all levels at once,
  -- in case a leader is changed to actor on another
  -- (possibly not even currently active) level.
  -- This runs for all factions even if save is requested by UI.
  -- Let players ponder new game state while the engine is busy saving.
  -- Also, this ensures perception before game save is exactly the same
  -- as at game resume, which is an invariant we check elsewhere.
  -- However, if perception is not updated after the action, the actor
  -- may not see his vicinity, so may not see enemy that displaces (or hits) him
  -- resulting in breaking the displace action and temporary leader loss,
  -- which is fine, though a bit alarming. So, we update it at the end.
  updatePerFid fid
  -- Move a single actor only. Note that the skipped actors are not marked
  -- as waiting. Normally they will act in the next clip or the next few,
  -- so that's natural. But if there are dozens of them, this is wierd.
  -- E.g., they don't move, but still make nearby foes lose Calm.
  -- However, for KISS, we leave it be.
  --
  -- Bail out if immediate loop break- requested by UI. No check
  -- for @sbreakLoop@ needed, for the same reasons as in @handleActors@.
  let handle :: [LevelId] -> m Bool
      handle [] = return False
      handle (lid : rest) = do
        breakASAP <- getsServer sbreakASAP
        if breakASAP
        then return False
        else do
          nonWaitMove <- handleActors lid fid
          if nonWaitMove
          then return True
          else handle rest
      killDying :: [LevelId] -> m ()
      killDying = mapM_ killDyingLid
      killDyingLid :: LevelId -> m ()
      killDyingLid lid = do
        localTime <- getsState $ getLocalTime lid
        levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
        let l = filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
            killAid (aid, _) = do
              b1 <- getsState $ getActorBody aid
              when (bhp b1 <= 0) $ dieSer aid b1
        mapM_ killAid l
  -- Start on arena with leader, if available. This is crucial to ensure
  -- that no actor (even ours) moves before UI declares save(&exit).
  fa <- factionArena fact
  arenas <- getsServer sarenas
  let myArenas = case fa of
        Just myArena -> myArena : delete myArena (ES.elems arenas)
        Nothing -> ES.elems arenas
  nonWaitMove <- handle myArenas
  breakASAP <- getsServer sbreakASAP
  unless breakASAP $ killDying myArenas
  -- We update perception at the end, see comment above. This is usually
  -- cheap, and when not, if it's AI faction, it's a waste, but if it's UI,
  -- that's exactly where it prevents lost attack messages, etc.
  -- If the move was a wait, perception unchanged, so no need to update,
  -- unless the actor starts sleeping, in which case his perception
  -- is reduced a bit later, so no harm done.
  when nonWaitMove $ updatePerFid fid

-- | Handle a clip (the smallest fraction of a game turn for which a frame may
-- potentially be generated). Run the leader and other actors moves.
-- Eventually advance the time and repeat.
loopUpd :: forall m. (MonadServerAtomic m, MonadServerComm m)
        => m () -> m ()
loopUpd updConn = do
  let updatePerFid :: FactionId -> m ()
      {-# NOINLINE updatePerFid #-}
      updatePerFid fid = do  -- {-# SCC updatePerFid #-} do
        perValid <- getsServer $ (EM.! fid) . sperValidFid
        mapM_ (\(lid, valid) -> unless valid $ updatePer fid lid)
              (EM.assocs perValid)
      handleFid :: (FactionId, Faction) -> m ()
      {-# NOINLINE handleFid #-}
      handleFid (fid, fact) = do
        breakASAP <- getsServer sbreakASAP
        -- Don't process other factions, even their perceptions,
        -- if UI saves and/or exits.
        unless breakASAP $ handleFidUpd updatePerFid fid fact
      loopConditionally = do
        factionD <- getsState sfactionD
        -- Update perception one last time to satisfy save/resume assertions,
        -- because we may get here at arbitrary moment due to game over
        -- and so have outdated perception.
        mapM_ updatePerFid (EM.keys factionD)
        modifyServer $ \ser -> ser { sbreakLoop = False
                                   , sbreakASAP = False }
        endOrLoop loopUpdConn (restartGame updConn loopUpdConn)
      loopUpdConn = do
        factionD <- getsState sfactionD
        -- Start handling actors with the single UI faction,
        -- to safely save/exit. Note that this hack fails if there are many UI
        -- factions (when we reenable multiplayer). Then players will request
        -- save&exit and others will vote on it and it will happen
        -- after the clip has ended, not at the start.
        -- Note that at most a single actor with a time-consuming action
        -- is processed per faction, so it's fair, but many loops are needed.
        let hasUI (_, fact) = fhasUI (gkind fact)
            (factionUI, factionsRest) = case break hasUI $ EM.assocs factionD of
              (noUI1, ui : noUI2) -> (ui, noUI1 ++ noUI2)
              _ -> error "no UI faction in the game"
        mapM_ handleFid $ factionUI : factionsRest
        breakASAP <- getsServer sbreakASAP
        breakLoop <- getsServer sbreakLoop
        if breakASAP || breakLoop
        then loopConditionally
        else do
          -- Projectiles are processed last and not at all if the UI leader
          -- decides to save or exit or restart or if there is game over.
          -- This and UI leader acting before any other ordinary actors
          -- ensures state is not changed and so the clip doesn't need
          -- to be carried through before save.
          arenas <- getsServer sarenas
          mapM_ (\fid -> mapM_ (`handleTrajectories` fid) $ ES.elems arenas)
                (EM.keys factionD)
          endClip updatePerFid  -- must be last, in case performs a bkp save
          -- The condition can be changed in @handleTrajectories@ by pushing
          -- onto an escape and in @endClip@.
          breakLoop2 <- getsServer sbreakLoop
          if breakLoop2
          then loopConditionally
          else loopUpdConn  -- process next iteration unconditionally
  loopUpdConn

-- | Handle the end of every clip. Do whatever has to be done
-- every fixed number of clips, e.g., monster generation.
-- Advance time. Perform periodic saves, if applicable.
--
-- This is never run if UI requested save or exit or restart and it's correct,
-- because we know nobody moved and no time was or needs to be advanced
-- and arenas are not changed. After game was saved and exited,
-- on game resume the first clip is performed with empty arenas,
-- so arena time is not updated and nobody moves, nor anything happens,
-- but arenas are here correctly updated.
endClip :: forall m. MonadServerAtomic m => (FactionId -> m ()) -> m ()
{-# INLINE endClip #-}
endClip updatePerFid = do
  COps{corule} <- getsState scops
  time <- getsState stime
  let clipN = time `timeFit` timeClip
  -- No check if @sbreakASAP@ is set, because then the function is not called.
  breakLoop <- getsServer sbreakLoop
  -- We don't send a lot of useless info to the client if the game has already
  -- ended. At best wasteful, at worst the player sees strange messages.
  unless breakLoop $ do
    -- I need to send time updates, because I can't add time to each command,
    -- because I'd need to send also all arenas, which should be updated,
    -- and this is too expensive data for each, e.g., projectile move.
    -- I send even if nothing changes so that UI time display can progress.
    -- Possibly @arenas@ are invalid here, but all moves were performed
    -- according to this value, so time should be replenished according
    -- to this value as well.
    -- This is crucial, because tiny time discrepancies can accumulate
    -- magnified by hunders of actors that share the clip slots due to the
    -- restriction that at most one faction member acts each clip.
    arenas <- getsServer sarenas
    execUpdAtomic $ UpdAgeGame arenas
    -- Perform periodic dungeon maintenance.
    when (clipN `mod` rleadLevelClips corule == 0) leadLevelSwitch
    case clipN `mod` clipsInTurn of
      0 ->
        -- Spawn monsters at most once per 3 turns.
        when (clipN `mod` (3 * clipsInTurn) == 0)
          spawnMonster
      4 ->
        -- Periodic activation only once per turn, for speed,
        -- but on all active arenas. Calm updates and domination
        -- happen there as well. Once per turn is too rare for accurate
        -- expiration of short conditions, e.g., 1-turn haste. TODO.
        applyPeriodicLevel
      _ -> return ()
  -- @applyPeriodicLevel@ might have, e.g., dominated actors, ending the game.
  -- It could not have unended the game, though.
  breakLoop2 <- getsServer sbreakLoop
  unless breakLoop2 $ do
    -- Possibly a leader change due to @leadLevelSwitch@, so update arenas here
    -- for 100% accuracy at least at the start of actor moves, before they
    -- change leaders as part of their moves.
    --
    -- After game resume, this is the first non-vacuus computation.
    -- Next call to @loopUpdConn@ really moves actors and updates arena times
    -- so we start in exactly the same place that UI save ended in.
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
  -- Saving on the browser causes a huge lag, hence autosave disabled.
#ifndef USE_JSFILE
  unless breakLoop2 $  -- if by chance requested and periodic saves coincide
    -- Periodic save needs to be at the end, so that restore can start
    -- at the beginning. Double save on first turn is avoided with @succ@.
    when (succ clipN `mod` rwriteSaveClips corule == 0) $
      writeSaveAll False False
#endif

-- | Check if the given actor is dominated and update his calm.
manageCalmAndDomination :: MonadServerAtomic m => ActorId -> Actor -> m ()
manageCalmAndDomination aid b = do
  performedDomination <-
    if bcalm b > 0 then return False else do  -- triggered by zeroed Calm
      hiImpression <- highestImpression b
      case hiImpression of
        Nothing -> return False
        Just (hiImpressionFid, hiImpressionK) -> do
          fact <- getsState $ (EM.! bfid b) . sfactionD
          if fhasPointman (gkind fact)
               -- animals/robots/human drones never Calm-dominated
             || hiImpressionK >= 10
               -- unless very high impression, e.g., in a dominated hero
          then dominateFidSfx aid aid (btrunk b) hiImpressionFid
          else return False
  unless performedDomination $ do
    newCalmDelta <- getsState $ regenCalmDelta aid b
    unless (newCalmDelta == 0) $
      -- Update delta for the current player turn.
      updateCalm aid newCalmDelta

-- | Trigger periodic items for all actors on the given level.
applyPeriodicLevel :: MonadServerAtomic m => m ()
applyPeriodicLevel = do
  arenas <- getsServer sarenas
  let applyPeriodicItem _ _ (_, (_, [])) = return ()
        -- periodic items always have at least one timer
      applyPeriodicItem aid cstore (iid, _) = do
        itemFull <- getsState $ itemToFull iid
        let arItem = aspectRecordFull itemFull
        when (IA.checkFlag Ability.Periodic arItem) $ do
          -- Check if the item is still in the bag (previous items act!).
          b2 <- getsState $ getActorBody aid
          bag <- getsState $ getBodyStoreBag b2 cstore
          case iid `EM.lookup` bag of
            Nothing -> return ()  -- item dropped
            Just (k, _) -> do
              -- Activate even if effects null or vacuous, to possibly
              -- destroy the item.
              let effApplyFlags = EffApplyFlags
                    { effToUse            = EffBare  -- no periodic crafting
                    , effVoluntary        = True
                    , effUseAllCopies     = k <= 1
                    , effKineticPerformed = False
                    , effActivation       = Ability.ActivationPeriodic
                    , effMayDestroy       = True
                    }
              void $ effectAndDestroyAndAddKill
                       effApplyFlags
                       aid aid aid iid (CActor aid cstore) itemFull
      applyPeriodicActor (aid, b) =
        -- While it's fun when projectiles flash or speed up mid-air,
        -- it's very exotic and quite time-intensive whenever hundreds
        -- of projectiles exist due to ongoing explosions.
        -- Nothing activates when actor dying to prevent a regenerating
        -- actor from resurrecting each turn, resulting in silly gameover stats.
        when (not (bproj b) && bhp b > 0 && blid b `ES.member` arenas) $ do
          -- Equipment goes first, to refresh organs before they expire,
          -- to avoid the message that organ expired.
          mapM_ (applyPeriodicItem aid CEqp) $ EM.assocs $ beqp b
          mapM_ (applyPeriodicItem aid COrgan) $ EM.assocs $ borgan b
          -- While we are at it, also update his Calm.
          manageCalmAndDomination aid b
  allActors <- getsState sactorD
  mapM_ applyPeriodicActor $ EM.assocs allActors

handleTrajectories :: MonadServerAtomic m => LevelId -> FactionId -> m ()
handleTrajectories lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . strajTime
  let l = sort $ map fst
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  -- The @strajTime@ map may be outdated before @hTrajectories@
  -- call (due to other actors following their trajectories),
  -- so it's only used to decide which actors are processed in this
  -- @handleTrajectories@ call. If an actor is added to the map,
  -- the recursive call to @handleTrajectories@ will detect that
  -- and process him later on.
  -- If the actor is no longer on the level or no longer belongs
  -- to the faction, it is nevertheless processed without a problem.
  -- We are guaranteed the actor still exists.
  mapM_ hTrajectories l
  -- Avoid frames between fadeout and fadein.
  breakLoop <- getsServer sbreakLoop
  unless (null l || breakLoop) $
    handleTrajectories lid fid  -- for speeds > tile/clip

hTrajectories :: MonadServerAtomic m => ActorId -> m ()
{-# INLINE hTrajectories #-}
hTrajectories aid = do
  b1 <- getsState $ getActorBody aid
  let removePushed b =
        -- No longer fulfills criteria and was not removed by dying; remove him.
        modifyServer $ \ser ->
          ser { strajTime =
                  EM.adjust (EM.adjust (EM.delete aid) (blid b)) (bfid b)
                            (strajTime ser)
              , strajPushedBy = EM.delete aid (strajPushedBy ser) }
      removeTrajectory b =
        -- Non-projectile actor stops flying (a projectile with empty trajectory
        -- would be intercepted earlier on as dead).
        -- Will be removed from @strajTime@ in recursive call
        -- to @handleTrajectories@.
        assert (not $ bproj b)
        $ execUpdAtomic $ UpdTrajectory aid (btrajectory b) Nothing
  breakLoop <- getsServer sbreakLoop
  if breakLoop then return ()  -- don't move if game over via pushing
  else if actorDying b1 then dieSer aid b1
  else case btrajectory b1 of
    Nothing -> removePushed b1
    Just ([], _) -> removeTrajectory b1 >> removePushed b1
    Just{} -> do
      advanceTrajectory aid b1
      -- Here, @advanceTrajectory@ might have affected @actorDying@,
      -- so we check again ASAP to make sure the body of the projectile
      -- (or pushed actor) doesn't block movement of other actors,
      -- but vanishes promptly.
      -- Bodies of actors that die not flying remain on the battlefied until
      -- their natural next turn, to give them a chance of rescue.
      -- Note that domination of pushed actors is not checked
      -- nor is their calm updated. They are helpless wrt movement,
      -- but also invulnerable in this respect.
      b2 <- getsState $ getActorBody aid
      if actorDying b2
      then dieSer aid b2
      else case btrajectory b2 of
        Nothing -> removePushed b2
        Just ([], _) -> removeTrajectory b2 >> removePushed b2
        Just{} -> -- delay next iteration only if still flying
          advanceTimeTraj aid
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

-- | Manage trajectory of a projectile or a pushed other actor.
--
-- Colliding with a wall or actor doesn't take time, because
-- the projectile does not move (the move is blocked).
-- Not advancing time forces dead projectiles to be destroyed ASAP.
-- Otherwise, with some timings, it can stay on the game map dead,
-- blocking path of human-controlled actors and alarming the hapless human.
advanceTrajectory :: MonadServerAtomic m => ActorId -> Actor -> m ()
advanceTrajectory aid b1 = do
  COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel $ blid b1
  arTrunk <- getsState $ (EM.! btrunk b1) . sdiscoAspect
  let registerKill killHow =
        -- Kill counts for each blast particle is TMI.
        when (bproj b1
              && not (IA.checkFlag Ability.Blast arTrunk)) $ do
          killer <- getsServer $ EM.findWithDefault aid aid . strajPushedBy
          addKillToAnalytics killer killHow (bfid b1) (btrunk b1)
  case btrajectory b1 of
    Just (d : lv, speed) -> do
      let tpos = bpos b1 `shift` d  -- target position
      if Tile.isWalkable coTileSpeedup $ lvl `at` tpos then do
           -- Hit will clear trajectories in @reqMelee@,
           -- so no need to do that here.
           execUpdAtomic $ UpdTrajectory aid (btrajectory b1) (Just (lv, speed))
           when (null lv) $ registerKill KillDropLaunch
           let occupied = occupiedBigLvl tpos lvl || occupiedProjLvl tpos lvl
               reqMoveHit = reqMoveGeneric False True aid d
               reqDisp = reqDisplaceGeneric False aid
           if | bproj b1 -> reqMoveHit  -- projectiles always hit
              | occupied ->
                -- Non-projectiles displace if they are ending their flight
                -- or if only a projectile is in the way.
                -- So, no chaos of displacing a whole line of enemies.
                case (posToBigLvl tpos lvl, posToProjsLvl tpos lvl) of
                  (Nothing, []) -> error "advanceTrajectory: not occupied"
                  (Nothing, [target]) -> reqDisp target
                  (Nothing, _) -> reqMoveHit  -- can't displace multiple
                  (Just target, []) ->
                    if null lv then reqDisp target else reqMoveHit
                  (Just _, _) -> reqMoveHit  -- can't displace multiple
              | otherwise -> reqMoveHit  -- if not occupied, just move
      else do
           -- Will be removed from @strajTime@ in recursive call
           -- to @handleTrajectories@.
           unless (bproj b1) $
             execSfxAtomic $ SfxCollideTile aid tpos
           embedsPre <- getsState $ getEmbedBag (blid b1) tpos
           -- No crafting by projectiles that bump tiles nor by pushed actors.
           -- The only way is if they land in a tile (are engulfed by it)
           -- and have enough skill. But projectiles transform when hitting,
           -- if terrain permits, not just bump off the obstacle.
           mfail <- reqAlterFail (not $ bproj b1) EffBare False aid tpos
           embedsPost <- getsState $ getEmbedBag (blid b1) tpos
           b2 <- getsState $ getActorBody aid
           let tpos2 = bpos b2 `shift` d  -- possibly another level and/or bpos
           lvl2 <- getLevel $ blid b2
           case mfail of
             Nothing | Tile.isWalkable coTileSpeedup $ lvl2 `at` tpos2 ->
               -- Too late to announce anything, but given that the way
               -- is opened, continue flight. Don't even normally lose any HP,
               -- because it's not a hard collision, but altering.
               -- However, if embed was possibly triggered/removed, lose HP.
               if embedsPre /= embedsPost && not (EM.null embedsPre) then
                 if bhp b2 > oneM then do
                   execUpdAtomic $ UpdRefillHP aid minusM
                   b3 <- getsState $ getActorBody aid
                   advanceTrajectory aid b3
                 else do
                   -- Projectile has too low HP to pierce; terminate its flight.
                   execUpdAtomic $ UpdTrajectory aid (btrajectory b2)
                                 $ Just ([], speed)
                   registerKill KillTileLaunch
               else
                 -- Try again with the cleared path and possibly actors
                 -- spawned in the way, etc.
                 advanceTrajectory aid b2
             _ -> do
               -- Altering failed to open the passage, probably just a wall,
               -- so lose HP due to being pushed into an obstacle.
               -- Never kill in this way.
               -- Note that sometimes this may come already after one faction
               -- wins the game and end game screens are show. This is OK-ish.
               -- @Nothing@ trajectory of signals an obstacle hit.
               -- If projectile, second call of @actorDying@ above
               -- will take care of dropping dead.
               execUpdAtomic $ UpdTrajectory aid (btrajectory b2) Nothing
               -- If projectile, losing HP due to hitting an obstacle
               -- not needed, because trajectory is halted, so projectile
               -- will die soon anyway
               if bproj b2
               then registerKill KillTileLaunch
               else when (bhp b2 > oneM) $ do
                 execUpdAtomic $ UpdRefillHP aid minusM
                 let effect = IK.RefillHP (-2)  -- -2 is a lie to ensure display
                 execSfxAtomic $ SfxEffect (bfid b2) aid (btrunk b2) effect (-1)
    _ -> error $ "Nothing or empty trajectory" `showFailure` (aid, b1)

handleActors :: (MonadServerAtomic m, MonadServerComm m)
             => LevelId -> FactionId -> m Bool
handleActors lid fid = do
  localTime <- getsState $ getLocalTime lid
  levelTime <- getsServer $ (EM.! lid) . (EM.! fid) . sactorTime
  let l = sort $ map fst
          $ filter (\(_, atime) -> atime <= localTime) $ EM.assocs levelTime
  -- The @sactorTime@ map may be outdated before @hActors@
  -- call (due to other actors on the list acting),
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

hActors :: forall m. (MonadServerAtomic m, MonadServerComm m)
        => [ActorId] -> m Bool
hActors [] = return False
hActors as@(aid : rest) = do
 b1 <- getsState $ getActorBody aid
 let !_A = assert (not $ bproj b1) ()
 if bhp b1 <= 0 then
   -- Will be killed in a later pass, making it possible to revive him now.
   hActors rest
 else do
  let side = bfid b1
  fact <- getsState $ (EM.! side) . sfactionD
  breakLoop <- getsServer sbreakLoop
  let mleader = gleader fact
      aidIsLeader = mleader == Just aid
      mainUIactor = fhasUI (gkind fact)
                    && (aidIsLeader || not (fhasPointman (gkind fact)))
      -- Checking @breakLoop@, to avoid doubly setting faction status to Camping
      -- in case AI-controlled UI client asks to exit game at exactly
      -- the same moment as natural game over was detected.
      mainUIunderAI = mainUIactor && gunderAI fact && not breakLoop
  when mainUIunderAI $
    handleUIunderAI side aid
  factNew <- getsState $ (EM.! side) . sfactionD
  let doQueryAI = not mainUIactor || gunderAI factNew
  breakASAP <- getsServer sbreakASAP
  -- If breaking out of the game loop, pretend there was a non-wait move.
  -- we don't need additionally to check @sbreakLoop@, because it occurs alone
  -- only via action of an actor and at most one action is performed here.
  if breakASAP then return True else do
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
        (cmd, maid) <- sendQueryUI RespQueryUI side aid
        aidNew <- mswitchLeader maid
        mtimed <- handleRequestUI side aidNew cmd
        return (aidNew, mtimed)
    case mtimed of
      Just timed -> do
        nonWaitMove <- handleRequestTimed side aidNew timed
        -- Even if the actor got a free turn of time via a scroll,
        -- he will not act again this clip, only next clip.
        -- Clip is small, so not a big deal and it's faster and avoids
        -- complete game time freezes, e.g., due to an exploit.
        if nonWaitMove then return True else hActors rest
      Nothing -> do
        breakASAP2 <- getsServer sbreakASAP
        -- If breaking out of the game lopp, pretend there was a non-wait move.
        if breakASAP2 then return True else hActors as

handleUIunderAI :: (MonadServerAtomic m, MonadServerComm m)
                => FactionId -> ActorId -> m ()
handleUIunderAI side aid = do
  cmdS <- sendQueryUI RespQueryUIunderAI side aid
  case fst cmdS of
    ReqUINop -> return ()
    ReqUIAutomate -> execUpdAtomic $ UpdAutoFaction side False
    ReqUIGameDropAndExit -> reqGameDropAndExit aid
    ReqUIGameSaveAndExit -> reqGameSaveAndExit aid
    _ -> error $ "" `showFailure` cmdS

dieSer :: MonadServerAtomic m => ActorId -> Actor -> m ()
dieSer aid b2 = do
  if bproj b2 then
    when (isJust $ btrajectory b2) $
      execUpdAtomic $ UpdTrajectory aid (btrajectory b2) Nothing
        -- needed only to ensure display of the last position of projectile
  else do
    kindId <- getsState $ getIidKindIdServer $ btrunk b2
    execUpdAtomic $ UpdRecordKill aid kindId 1
    -- At this point the actor's body exists and his items are not dropped.
    deduceKilled aid
    -- Most probabaly already done, but just in case (e.g., when actor
    -- created with 0 HP):
    electLeader (bfid b2) (blid b2) aid
  -- If an explosion blast, before the particle is destroyed, it tries
  -- to modify terrain with it as well as do some easy crafting,
  -- e.g., cooking on fire.
  arTrunk <- getsState $ (EM.! btrunk b2) . sdiscoAspect
  let spentProj = bproj b2 && EM.null (beqp b2)
      isBlast = IA.checkFlag Ability.Blast arTrunk
      -- Let thrown food cook in fire (crafting) and other projectiles
      -- transform terrain they fall onto. Big actors are inert at death.
      (effScope, bumping) = if bproj b2
                            then (EffBareAndOnCombine, False)
                            else (EffBare, True)
  when (not spentProj && isBlast) $
    void $ reqAlterFail bumping effScope False aid (bpos b2)
  b3 <- getsState $ getActorBody aid
  -- Items need to do dropped now, so that they can be transformed by effects
  -- of the embedded items, if they are activated.
  -- If the actor was a projectile and no effect was triggered by hitting
  -- an enemy, the item still exists and @OnSmash@ effects will be triggered.
  dropAllEquippedItems aid b3
  -- Also destroy, not just drop, all organs, to trigger any effects.
  -- Note that some effects may be invoked on an actor that has
  -- no trunk any more. Conditions are ignored to avoid spam about them ending.
  bag <- getsState $ getBodyStoreBag b3 COrgan
  discoAspect <- getsState sdiscoAspect
  let f = void <$$> dropCStoreItem False True COrgan aid b3 maxBound
      isCondition = IA.checkFlag Ability.Condition . (discoAspect EM.!)
  mapM_ (uncurry f) $ filter (not . isCondition . fst) $ EM.assocs bag
  -- As the last act of heroism, the actor (even if projectile)
  -- changes the terrain with its embedded items, if possible.
  -- Note that all the resulting effects are invoked on an actor that has
  -- no trunk any more.
  when (not spentProj && not isBlast) $
    void $ reqAlterFail bumping effScope False aid (bpos b2)
      -- old bpos; OK, safer
  b4 <- getsState $ getActorBody aid
  execUpdAtomic $ UpdDestroyActor aid b4 []

restartGame :: MonadServerAtomic m
            => m () -> m () -> Maybe (GroupName ModeKind) -> m ()
restartGame updConn loop mgameMode = do
  -- This goes only to the old UI client.
  execSfxAtomic SfxRestart
  soptionsNxt <- getsServer soptionsNxt
  srandom <- getsServer srandom
  factionDold <- getsState sfactionD
  -- Create new factions.
  s <- gameReset soptionsNxt mgameMode (Just srandom)
  -- Note how we also no longer assert exploration, because there may not be
  -- enough time left in the debug run to explore again in a new game.
  let optionsBarRngs = soptionsNxt { sdungeonRng = Nothing
                                   , smainRng = Nothing
                                   , sassertExplored = Nothing }
  modifyServer $ \ser -> ser { soptionsNxt = optionsBarRngs
                             , soptions = optionsBarRngs }
  -- This reaches only the intersection of old and new clients.
  execUpdAtomic $ UpdRestartServer s
  -- Spawn new clients, as needed, according to new factions.
  updConn
  initPer
  reinitGame factionDold
  -- Save a just started noConfirm game to preserve history of the just
  -- ended normal game, in case the user exits brutally.
  writeSaveAll False True
  loop
