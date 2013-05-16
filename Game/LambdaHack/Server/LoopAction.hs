{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer) where

import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Server.Action hiding (sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ServerSem
import Game.LambdaHack.Server.StartAction
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Assert

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
loopSer :: (MonadAtomic m, MonadServerConn m)
        => DebugModeSer
        -> (CmdSer -> m Bool)
        -> (FactionId -> Conn CmdClientUI -> IO ())
        -> (FactionId -> Conn CmdClientAI -> IO ())
        -> Kind.COps
        -> m ()
loopSer sdebugNxt cmdSerSem executorUI executorAI !cops = do
  -- Recover states.
  restored <- tryRestore cops
  case restored of
    Nothing -> do  -- Starting a new game.
      -- Set up commandline debug mode
      modifyServer $ \ser -> ser {sdebugNxt}
      s <- gameReset cops
      let speedup = speedupCOps (sallClear sdebugNxt)
      execCmdAtomic $ RestartServerA $ updateCOps speedup s
      initConn sdebugNxt executorUI executorAI
      reinitGame False
      -- Save ASAP in case of crashes and disconnects.
      saveBkpAll
    Just (sRaw, ser) -> do  -- Running a restored game.
      let setCops = const (speedupCOps (sallClear sdebugNxt) cops)
      execCmdAtomic $ ResumeServerA $ updateCOps setCops sRaw
      putServer ser {sdebugNxt}
      initConn sdebugNxt executorUI executorAI
      pers <- getsServer sper
      broadcastCmdAtomic $ \fid -> ResumeA fid (pers EM.! fid)
  -- Loop.
  let loop = do
        let run lid = handleActors cmdSerSem lid
            factionArena fact =
              case gleader fact of
                Nothing -> return Nothing
                Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
        factionD <- getsState sfactionD
        marenas <- mapM factionArena $ EM.elems factionD
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        assert (not $ null arenas) skip
        mapM_ run arenas
        endClip arenas
        endOrLoop loop
  loop

saveBkpAll :: (MonadAtomic m, MonadServer m) => m ()
saveBkpAll = do
  execCmdAtomic SaveBkpA
  saveGameBkp

endClip :: (MonadAtomic m, MonadServer m) => [LevelId] -> m ()
endClip arenas = do
  quit <- getsServer squit
  if quit then
    -- In case of save and exit, don't age levels, since possibly
    -- not all actors have moved yet.
    modifyServer $ \ser -> ser {squit = False}
  else do
    time <- getsState stime
    let clipN = time `timeFit` timeClip
        cinT = let r = timeTurn `timeFit` timeClip
               in assert (r > 2) r
        bkpFreq = cinT * 100
        clipMod = clipN `mod` cinT
    bkpSave <- getsServer sbkpSave
    when (bkpSave || clipN `mod` bkpFreq == 0) $ do
      modifyServer $ \ser -> ser {sbkpSave = False}
      execCmdAtomic SaveBkpA
      saveGameBkp
    -- Regenerate HP and add monsters each turn, not each clip.
    when (clipMod == 1) $ mapM_ regenerateLevelHP arenas
    when (clipMod == 2) $ mapM_ generateMonster arenas
    -- TODO: a couple messages each clip to many clients is too costly.
    -- Store these on a queue and sum times instead of sending,
    -- until a different command needs to be sent. Include HealActorA
    -- from regenerateLevelHP, but keep it before AgeGameA.
    mapM_ (\lid -> execCmdAtomic $ AgeLevelA lid timeClip) arenas
    execCmdAtomic $ AgeGameA timeClip

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less than or equal to the current level time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. But most often we push exactly one frame or frame delay.
handleActors :: (MonadAtomic m, MonadServerConn m)
             => (CmdSer -> m Bool)
             -> LevelId
             -> m ()
handleActors cmdSerSem lid = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState $ getLocalTime lid  -- the end of this clip, inclusive
  prio <- getsLevel lid lprio
  quit <- getsServer squit
  factionD <- getsState sfactionD
  s <- getState
  let -- Actors of the same faction move together.
      -- TODO: insert wrt the order, instead of sorting
      isLeader (aid, b) = not $ Just aid == gleader (factionD EM.! bfid b)
      order = Ord.comparing $
        ((>= 0) . bhp . snd) &&& bfid . snd &&& isLeader &&& bsymbol . snd
      (atime, as) = EM.findMin prio
      ams = map (\a -> (a, getActorBody a s)) as
      mnext | EM.null prio = Nothing  -- no actor alive, wait until it spawns
            | otherwise = if atime > time
                          then Nothing  -- no actor is ready for another move
                          else Just $ head $ sortBy order ams
  case mnext of
    _ | quit -> return ()
    Nothing -> return ()
    Just (aid, b) | bproj b && bhp b < 0 -> do
      -- A projectile hits an actor. The carried item is destroyed.
      ais <- getsState $ getActorItem aid
      execCmdAtomic $ DestroyActorA aid b ais
      -- The attack animation for the projectile hit subsumes @DisplayPushD@,
      -- so not sending an extra @DisplayPushD@ here.
      handleActors cmdSerSem lid
    Just (aid, b) | bhp b <= 0 && not (bproj b)
                    || maybe False null (bpath b) -> do
      -- An actor (projectile or not) ceases to exist.
      -- Items drop to the ground and possibly a new leader is elected.
      dieSer aid
      -- If it's a death, not a projectile drop, the death animation
      -- subsumes @DisplayPushD@, so not sending it here. ProjectileProjectile
      -- destruction is not important enough for an extra @DisplayPushD@.
      handleActors cmdSerSem lid
    Just (aid, body) -> do
      let side = bfid body
          fact = factionD EM.! side
          mleader = gleader fact
          usesAI = usesAIFact fact
          hasHumanLeader = isNothing $ gAiLeader fact
          queryUI = not usesAI || hasHumanLeader && Just aid == mleader
      -- Display a new frame so that player does not see moves of all his
      -- AI party members cumulated in a single frame, but one by one.
      execSfxAtomic $ DisplayPushD side
      -- TODO: check that the command is legal
      cmdS <- (if queryUI then sendQueryUI else sendQueryAI) side aid
      let timed = timedCmdSer cmdS
          leaderNew = aidCmdSer cmdS
          leadAtoms =
            if leaderNew /= aid
            then -- Only leader can change leaders  -- TODO: effLvlGoUp changes
                 assert (mleader == Just aid)
                   [LeadFactionA side mleader (Just leaderNew)]
            else []
      mapM_ execCmdAtomic leadAtoms
      bPre <- getsState $ getActorBody leaderNew
      -- Check if the client cheats, trying to move other faction actors.
      assert (bfid bPre == side `blame` (bPre, side)) skip
      notAborted <-
        if bhp bPre <= 0 && not (bproj bPre)
        then execFailure side "You strain, fumble and faint from the exertion."
        else cmdSerSem cmdS
      -- Advance time once, after the leader switched perhaps many times.
      -- TODO: this is correct only when all heroes have the same
      -- speed and can't switch leaders by, e.g., aiming a wand
      -- of domination. We need to generalize by displaying
      -- "(next move in .3s [RET]" when switching leaders.
      -- RET waits .3s and gives back control,
      -- Any other key does the .3s wait and the action from the key
      -- at once.
      when (timed && notAborted) $ advanceTime leaderNew
      -- Generate extra frames if the actor has already moved during
      -- this clip, so his multiple moves would be collapsed in one frame.
      -- If the actor changes his speed this very turn, the test can fail,
      -- but it's a minor UI issue, so let it be.
      let previousClipEnd = timeAdd time $ timeNegate timeClip
          lastSingleMove = timeAddFromSpeed coactor bPre previousClipEnd
      when (btime bPre > lastSingleMove) $
        broadcastSfxAtomic DisplayPushD
      nH <- nHumans
      -- TODO: do not fade out if all others are running (so the previous
      -- move was of the same faction) or if 2 moves in a row of a fast actor
      let fadeOut
            -- No UI, no time taken or at most one human player,
            -- so no need to visually mark the end of the move.
            | not queryUI || not timed || not notAborted || nH <= 1 = []
            | otherwise = [ FadeoutD side True
                          , FlushFramesD side
                          , FadeinD side True ]
      mapM_ execSfxAtomic $ fadeOut
      handleActors cmdSerSem lid

dieSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
dieSer aid = do  -- TODO: explode if a projectile holding a potion
  body <- getsState $ getActorBody aid
  let fid = bfid body
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  mleader <- electLeader fid (blid body) aid
  dropAllItems aid body
  execCmdAtomic $ DestroyActorA aid body {bbag = EM.empty} []
  spawning <- getsState $ flip isSpawningFaction fid
  when (not spawning && isNothing mleader) $ do
    oldSt <- getsState $ gquit . (EM.! fid) . sfactionD
    execCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Killed $ blid body)

-- | Drop all actor's items.
dropAllItems :: MonadAtomic m => ActorId -> Actor -> m ()
dropAllItems aid b = do
  let f (iid, k) = execCmdAtomic
                   $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                     (CFloor (blid b) (bpos b))
  mapM_ f $ EM.assocs $ bbag b

electLeader :: (MonadAtomic m, MonadServer m)
            => FactionId -> LevelId -> ActorId
            -> m (Maybe ActorId)
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  if isNothing mleader || mleader == Just aidDead then do
    actorD <- getsState sactorD
    let ours (_, b) = bfid b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorNotProjAssocs (== fid) lid
    Config{configFirstDeathEnds} <- getsServer sconfig
    spawning <- getsState $ flip isSpawningFaction fid
    let mleaderNew | configFirstDeathEnds && not spawning = Nothing
                   | otherwise = listToMaybe $ filter (/= aidDead)
                                 $ map fst $ onLevel ++ party
    execCmdAtomic $ LeadFactionA fid mleader mleaderNew
    return mleaderNew
  else return mleader

-- | Advance the move time for the given actor.
advanceTime :: MonadAtomic m => ActorId -> m ()
advanceTime aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  -- TODO: Add an option to block this for non-projectiles too.
  if bhp b < 0 && bproj b then return ()
  else do
    let speed = actorSpeed coactor b
        t = ticksPerMeter speed
    execCmdAtomic $ AgeActorA aid t

-- | Generate a monster, possibly.
generateMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
generateMonster lid = do
  cops@Kind.COps{cofact=Kind.Ops{okind}} <- getsState scops
  pers <- getsServer sper
  lvl@Level{ldepth} <- getsLevel lid id
  factionD <- getsState sfactionD
  s <- getState
  let f fid = fspawn (okind (gkind (factionD EM.! fid))) > 0
      spawns = actorNotProjList f lid s
  rc <- rndToAction $ monsterGenChance ldepth (length spawns)
  when rc $ do
    let allPers = ES.unions $ map (totalVisible . (EM.! lid)) $ EM.elems pers
    pos <- rndToAction $ rollSpawnPos cops allPers lid lvl s
    spawnMonsters [pos] lid

rollSpawnPos :: Kind.COps -> ES.EnumSet Point -> LevelId -> Level -> State
             -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible lid Level{ltile, lxsize, lstair} s = do
  let cminStairDist = chessDist lxsize (fst lstair) (snd lstair)
      inhabitants = actorNotProjList (const True) lid s
      isLit = Tile.isLit cotile
      distantAtLeast d p _ =
        all (\b -> chessDist lxsize (bpos b) p > d) inhabitants
  findPosTry 40 ltile
    [ \ _ t -> not (isLit t)  -- no such tiles on some maps
    , distantAtLeast cminStairDist
    , distantAtLeast $ cminStairDist `div` 2
    , \ p _ -> not $ p `ES.member` visible
    , distantAtLeast $ cminStairDist `div` 3
    , distantAtLeast $ cminStairDist `div` 4
    , distantAtLeast 3  -- otherwise a fast actor can walk and hit in one turn
    , \ p t -> Tile.hasFeature cotile F.Walkable t
               && unoccupied (actorList (const True) lid s) p
    ]

-- TODO: generalize to any list of items (or effects) applied to all actors
-- every turn. Specify the list per level in config.
-- TODO: use itemEffect or at least effectSem to get from Regeneration
-- to HealActorA. Also, Applying an item with Regeneration should do the same
-- thing, but immediately (and destroy the item).
-- | Possibly regenerate HP for all actors on the current level.
--
-- We really want leader selection to be a purely UI distinction,
-- so all actors need to regenerate, not just the leaders.
-- Actors on frozen levels don't regenerate. This prevents cheating
-- via sending an actor to a safe level and letting him regenerate there.
regenerateLevelHP :: MonadAtomic m => LevelId -> m ()
regenerateLevelHP lid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  time <- getsState $ getLocalTime lid
  s <- getState
  let pick (a, m) =
        let ak = okind $ bkind m
            itemAssocs = getActorItem a s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen itemAssocs of
                        Just (k, _)  -> k + 1
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0 || deltaHP <= 0
           then Nothing
           else Just a
  toRegen <-
    getsState $ catMaybes . map pick . actorNotProjAssocs (const True) lid
  mapM_ (\aid -> execCmdAtomic $ HealActorA aid 1) toRegen

-- | Continue or restart or exit the game.
endOrLoop :: (MonadAtomic m, MonadServerConn m) => m () -> m ()
endOrLoop loopServer = do
  factionD <- getsState sfactionD
  let f (_, Faction{gquit=Nothing}) = Nothing
      f (fid, Faction{gquit=Just quit}) = Just (fid, quit)
  processQuits loopServer $ mapMaybe f $ EM.assocs factionD

processQuits :: (MonadAtomic m, MonadServerConn m)
             => m () -> [(FactionId, (Bool, Status))] -> m ()
processQuits loopServer [] = loopServer  -- just continue
processQuits loopServer ((fid, quit) : quits) = do
  cops <- getsState scops
  factionD <- getsState sfactionD
  let faction = factionD EM.! fid
  total <- case gleader faction of
    Nothing -> return 0
    Just leader -> do
      b <- getsState $ getActorBody leader
      getsState $ snd . calculateTotal fid (blid b)
  case snd quit of
    status@Killed{} -> do
      let inGame fact = case gquit fact of
            Just (_, Killed{}) -> False
            Just (_, Victor) -> False
            _ -> True
          notSpawning fact = not $ isSpawningFact cops fact
          isActive fact = inGame fact && notSpawning fact
          inGameHuman fact = inGame fact && isHumanFact fact
          gameHuman = filter inGameHuman $ EM.elems factionD
          gameOver = case filter (isActive . snd) $ EM.assocs factionD of
            _ | null gameHuman -> True  -- no screensaver mode for now
            [] -> True  -- last ally dies, so cooperative game ends
            (fid1, fact1) : rest ->
              -- Competitive game ends when only one allied team remains.
              all (\(_, factRest) -> isAllied factRest fid1) rest
              -- Verify this was not a cooperative game.
              && not (isAllied fact1 fid)
      if gameOver then do
        registerScore status total
        restartGame False loopServer
      else
        processQuits loopServer quits
    status@Victor -> do
      registerScore status total
      restartGame False loopServer
    Restart -> restartGame True loopServer
    Camping -> do
      execCmdAtomic $ QuitFactionA fid (Just quit) Nothing
      execCmdAtomic SaveExitA
      saveGameSer
      -- Verify that the saved perception is equal to future reconstructed.
      persSaved <- getsServer sper
      configFov <- fovMode
      s <- getState
      let pers = dungeonPerception cops configFov s
      assert (persSaved == pers `blame` (persSaved, pers)) skip
      -- Con't call @loopServer@, that is, quit the game loop.

restartGame :: (MonadAtomic m, MonadServerConn m) => Bool -> m () -> m ()
restartGame quitter loopServer = do
  cops <- getsState scops
  nH <- nHumans
  when (nH <= 1) $ broadcastSfxAtomic $ \fid -> FadeoutD fid False
  s <- gameReset cops
  execCmdAtomic $ RestartServerA s
  initPer
  reinitGame quitter
  -- Save ASAP in case of crashes and disconnects.
  saveBkpAll
  loopServer
