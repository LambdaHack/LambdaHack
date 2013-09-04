{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text)

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
import Game.LambdaHack.Frontend
import Game.LambdaHack.Server.Action hiding (sendUpdateAI, sendUpdateUI)
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
loopSer :: (MonadAtomic m, MonadConnServer m)
        => DebugModeSer
        -> (CmdSer -> m Bool)
        -> (FactionId -> ChanFrontend -> ChanServer CmdClientUI -> IO ())
        -> (FactionId -> ChanServer CmdClientAI -> IO ())
        -> Kind.COps
        -> m ()
loopSer sdebugNxt cmdSerSem executorUI executorAI !cops = do
  -- Recover states and launch clients.
  restored <- tryRestore cops
  case restored of
    Nothing -> do  -- Starting a new game.
      -- Set up commandline debug mode
      modifyServer $ \ser -> ser {sdebugNxt}
      s <- gameReset cops "campaign"
      let speedup = speedupCOps (sallClear sdebugNxt)
      execCmdAtomic $ RestartServerA $ updateCOps speedup s
      applyDebug sdebugNxt
      updateConn executorUI executorAI
      initPer
      reinitGame
      -- Save ASAP in case of crashes and disconnects.
      saveBkpAll
    Just (sRaw, ser) -> do  -- Running a restored game.
      -- First, set the previous cops, to send consistent info to clients.
      let setPreviousCops = const cops
      execCmdAtomic $ ResumeServerA $ updateCOps setPreviousCops sRaw
      putServer ser {sdebugNxt}
      applyDebug sdebugNxt
      updateConn executorUI executorAI
      initPer
      pers <- getsServer sper
      broadcastCmdAtomic $ \fid -> ResumeA fid (pers EM.! fid)
      -- Second, set the current cops and reinit perception.
      let setCurrentCops = const (speedupCOps (sallClear sdebugNxt) cops)
      -- @sRaw@ is correct here, because none of the above changes State.
      execCmdAtomic $ ResumeServerA $ updateCOps setCurrentCops sRaw
      initPer
  -- Loop, communicating with clients.
  let loop = do
        let run lid = handleActors cmdSerSem lid
            factionArena fact = do
              let isSpawning = isSpawningFact cops fact
                  -- TODO; This is a significant advantage of human spawners;
                  -- perhaps we could instead auto-switch leaders
                  -- to the fist level non-spawner factions act on.
                  isHuman = isHumanFact fact
              case gleader fact of
                Just leader | isHuman || not isSpawning -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
                _ -> return Nothing
        factionD <- getsState sfactionD
        marenas <- mapM factionArena $ EM.elems factionD
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        assert (not $ null arenas) skip
        mapM_ run arenas
        quit <- getsServer squit
        if quit then do
          -- In case of game save+exit or restart, don't age levels (endClip)
          -- since possibly not all actors have moved yet.
          modifyServer $ \ser -> ser {squit = False}
          endOrLoop (updateConn executorUI executorAI) loop
        else do
          endClip arenas
          loop
  loop

saveBkpAll :: (MonadAtomic m, MonadServer m) => m ()
saveBkpAll = do
  execCmdAtomic SaveBkpA
  saveGameBkp

endClip :: (MonadAtomic m, MonadServer m) => [LevelId] -> m ()
endClip arenas = do
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
handleActors :: (MonadAtomic m, MonadConnServer m)
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
      -- TODO: perhaps don't destroy if no effect (NoEffect).
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
      -- TODO: check that the command is legal
      cmdS <- if queryUI then
                -- The client always displays a frame in this case.
                sendQueryUI side aid
              else do
                -- Order the UI client (if any) corresponding to the AI client
                -- to display a new frame so that player does not see moves
                -- of all his AI party members cumulated in a single frame,
                -- but one by one.
                execSfxAtomic $ DisplayPushD side
                sendQueryAI side aid
      let leaderNew = aidCmdSer cmdS
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
      timed <-
        if bhp bPre <= 0 && not (bproj bPre)
        then execFailure side "You strain, fumble and faint from the exertion."
        else cmdSerSem cmdS
      -- AI has to take time, otherwise it'd loop.
      assert (queryUI || timed `blame` (cmdS, timed, bPre)) skip
      -- Advance time once, after the leader switched perhaps many times.
      -- TODO: this is correct only when all heroes have the same
      -- speed and can't switch leaders by, e.g., aiming a wand
      -- of domination. We need to generalize by displaying
      -- "(next move in .3s [RET]" when switching leaders.
      -- RET waits .3s and gives back control,
      -- Any other key does the .3s wait and the action from the key
      -- at once.
      when timed $ advanceTime leaderNew
      -- Generate extra frames if the actor has already moved during
      -- this clip, so his multiple moves would be collapsed in one frame.
      -- If the actor changes his speed this very turn, the test can fail,
      -- but it's a minor UI issue, so let it be.
      let previousClipEnd = timeAdd time $ timeNegate timeClip
          lastSingleMove = timeAddFromSpeed coactor bPre previousClipEnd
      when (btime bPre > lastSingleMove) $
        broadcastSfxAtomic DisplayPushD
      handleActors cmdSerSem lid

dieSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
dieSer aid = do  -- TODO: explode if a projectile holding a potion
  body <- getsState $ getActorBody aid
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  electLeader (bfid body) (blid body) aid
  dropAllItems aid body
  execCmdAtomic $ DestroyActorA aid body {bbag = EM.empty} []
  deduceKilled body

-- | Drop all actor's items.
dropAllItems :: MonadAtomic m => ActorId -> Actor -> m ()
dropAllItems aid b = do
  let f iid k = execCmdAtomic
                $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                  (CFloor (blid b) (bpos b))
  mapActorItems_ f b

-- | Advance the move time for the given actor.
advanceTime :: MonadAtomic m => ActorId -> m ()
advanceTime aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  if bhp b < 0 && bproj b || maybe False null (bpath b)
    then
      -- Don't update move time, so move ASAP, so the projectile
      -- corpse vanishes ASAP.
      return ()
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
    , \ _ t -> Tile.hasFeature cotile F.CanActor t  -- in reachable area
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
  let approve (a, m) =
        let ak = okind $ bkind m
            itemAssocs = getActorItem a s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen itemAssocs of
                        Just (k, _)  -> k + 1
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0
              || deltaHP <= 0
              || bhp m <= 0
           then Nothing
           else Just a
  toRegen <-
    getsState $ catMaybes . map approve . actorNotProjAssocs (const True) lid
  mapM_ (\aid -> execCmdAtomic $ HealActorA aid 1) toRegen

-- | Continue or exit or restart the game.
endOrLoop :: (MonadAtomic m, MonadConnServer m) => m () -> m () -> m ()
endOrLoop updConn loopServer = do
  cops <- getsState scops
  factionD <- getsState sfactionD
  let inGame fact = case gquit fact of
        Nothing -> True
        Just Camping -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  let getQuitter fact = case gquit fact of
        Just (Restart t) -> Just t
        _ -> Nothing
      quitters = mapMaybe getQuitter $ EM.elems factionD
  let isCamper fact = case gquit fact of
        Just Camping -> True
        _ -> False
      campers = filter (isCamper . snd) $ EM.assocs factionD
  case (quitters, campers) of
    (t : _, _) -> restartGame t updConn loopServer
    _ | gameOver -> restartGame "campaign" updConn loopServer
    (_, []) -> loopServer  -- continue current game
    (_, _ : _) -> do  -- save game and exit
      -- Wipe out the quit flag for the savegame files.
      mapM_ (\(fid, _) -> execCmdAtomic
                          $ QuitFactionA fid (Just Camping) Nothing) campers
      -- Save client and server data.
      execCmdAtomic SaveExitA
      saveGameSer
      -- Kill all clients, including those that did not take part
      -- in the current game.
      -- Clients exit not now, but after they print all ending screens.
      killAllClients
      -- Verify that the saved perception is equal to future reconstructed.
      persSaved <- getsServer sper
      configFov <- fovMode
      pers <- getsState $ dungeonPerception cops configFov
      assert (persSaved == pers `blame` (persSaved, pers)) skip
      -- Don't call @loopServer@, that is, quit the game loop.

restartGame :: (MonadAtomic m, MonadConnServer m)
            => Text -> m () -> m () -> m ()
restartGame t updConn loopServer = do
  cops <- getsState scops
  s <- gameReset cops t
  execCmdAtomic $ RestartServerA s
  updConn
  initPer
  reinitGame
  -- Save ASAP in case of crashes and disconnects.
  saveBkpAll
  loopServer
