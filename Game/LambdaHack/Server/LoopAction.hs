{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer) where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Writer.Strict (WriterT, execWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action hiding (sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.AtomicSemSer
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.ServerSem
import Game.LambdaHack.Server.StartAction
import Game.LambdaHack.Server.State
import Game.LambdaHack.ServerCmd
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
loopSer :: (MonadAction m, MonadServerConn m)
        => DebugModeSer
        -> (CmdSer -> m [Atomic])
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
      gameReset cops
      initConn sdebugNxt executorUI executorAI
      execAtomic reinitGame
      -- Save ASAP in case of crashes and disconnects.
      saveBkpAll
    Just (gloRaw, ser) -> do  -- Running a restored game.
      putState $ updateCOps (const cops) gloRaw
      putServer ser {sdebugNxt}
      initConn sdebugNxt executorUI executorAI
      pers <- getsServer sper
      execAtomic $ broadcastCmdAtomic $ \fid -> ResumeA fid (pers EM.! fid)
  -- Loop.
  let loop = do
        let run arena = handleActors cmdSerSem arena
            factionArena fac =
              case gleader fac of
                Nothing -> return Nothing
                Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
        faction <- getsState sfaction
        marenas <- mapM factionArena $ EM.elems faction
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        assert (not $ null arenas) skip
        mapM_ run arenas
        execAtomic $ endClip arenas
        endOrLoop loop
  loop

execAtomic :: (MonadAction m, MonadServerConn m)
           => WriterT [Atomic] m () -> m ()
execAtomic m = do
 cmds <- execWriterT m
 mapM_ atomicSendSem cmds

saveBkpAll :: (MonadAction m, MonadServerConn m) => m ()
saveBkpAll = do
  atomicSendSem $ CmdAtomic SaveBkpA
  saveGameBkp

endClip :: MonadServer m => [LevelId] -> WriterT [Atomic] m ()
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
      tellCmdAtomic SaveBkpA
      saveGameBkp
    -- Regenerate HP and add monsters each turn, not each clip.
    when (clipMod == 1) $ mapM_ generateMonster arenas
    when (clipMod == 2) $ mapM_ regenerateLevelHP arenas
    -- TODO: a couple messages each clip to many clients is too costly
    mapM_ (\lid -> tellCmdAtomic $ AgeLevelA lid timeClip) arenas
    tellCmdAtomic $ AgeGameA timeClip

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less than or equal to the current level time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. But most often we push exactly one frame or frame delay.
handleActors :: (MonadAction m, MonadServerConn m)
             => (CmdSer -> m [Atomic])
             -> LevelId
             -> m ()
handleActors cmdSerSem arena = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState $ getLocalTime arena  -- the end of this clip, inclusive
  prio <- getsLevel arena lprio
  quit <- getsServer squit
  faction <- getsState sfaction
  s <- getState
  let -- Actors of the same faction move together.
      -- TODO: insert wrt the order, instead of sorting
      isLeader (aid, b) = not $ Just aid == gleader (faction EM.! bfaction b)
      order = Ord.comparing $
        ((>= 0) . bhp . snd) &&& bfaction . snd &&& isLeader &&& bsymbol . snd
      (atime, as) = EM.findMin prio
      ams = map (\a -> (a, getActorBody a s)) as
      mnext | EM.null prio = Nothing  -- no actor alive, wait until it spawns
            | otherwise = if atime > time
                          then Nothing  -- no actor is ready for another move
                          else Just $ head $ sortBy order ams
  case mnext of
    _ | quit -> return ()
    Nothing -> return ()
      -- TODO: let clients insert DisplayDelayD once per clip based on time
    Just (aid, b) | bproj b && bhp b < 0 -> do
      -- A projectile hits an actor. The carried item is destroyed.
      ais <- getsState $ getActorItem aid
      execAtomic $ tellCmdAtomic $ DestroyActorA aid b ais
      -- The attack animation for the projectile hit subsumes @DisplayPushD@,
      -- so not sending an extra @DisplayPushD@ here.
      handleActors cmdSerSem arena
    Just (aid, b) | bhp b <= 0 && not (bproj b)
                    || maybe False null (bpath b) -> do
      -- An actor (projectile or not) ceases to exist.
      -- Items drop to the ground and possibly a new leader is elected.
      execAtomic $ dieSer aid
      -- If it's a death, not a projectile drop, the death animation
      -- subsumes @DisplayPushD@, so not sending it here. ProjectileProjectile
      -- destruction is not important enough for an extra @DisplayPushD@.
      handleActors cmdSerSem arena
    Just (aid, body) -> do
      -- TODO: let clients insert this once per clip based on time
      execAtomic $ broadcastSfxAtomic DisplayPushD
      let side = bfaction body
          fac = faction EM.! side
          mleader = gleader fac
          usesAI = usesAIFact fac
          hasHumanLeader = isNothing $ gAiLeader fac
          queryUI = not usesAI || hasHumanLeader && Just aid == mleader
      -- TODO: check that the command is legal
      cmdS <- (if queryUI then sendQueryUI else sendQueryAI) side aid
      let timed = timedCmdSer cmdS
          leaderNew = aidCmdSer cmdS
          leadAtoms =
            if leaderNew /= aid
            then -- Only leader can change leaders  -- TODO: effLvlGoUp changes
                 assert (mleader == Just aid)
                   [CmdAtomic (LeadFactionA side mleader (Just leaderNew))]
            else []
      bPre <- getsState $ getActorBody leaderNew
      -- Check if the client cheats, trying to move other faction actors.
      assert (bfaction bPre == side `blame` (bPre, side)) skip
      atoms <- cmdSerSem cmdS
      nH <- nHumans
      -- TODO: do not fade out if all other are running (so the previous
      -- move was of the same actor) or if 2 moves in a row of a fast actor
      let fadeOut
            -- No UI, no time taken or at most one human player,
            -- so no need to visually mark the end of the move.
            | not queryUI || not timed || nH <= 1 = []
            | otherwise = [ SfxAtomic $ FadeoutD side True
                          , SfxAtomic $ FlushFramesD side
                          , SfxAtomic $ FadeinD side True ]
      -- Advance time once, after the leader switched perhaps many times.
      -- TODO: this is correct only when all heroes have the same
      -- speed and can't switch leaders by, e.g., aiming a wand
      -- of domination. We need to generalize by displaying
      -- "(next move in .3s [RET]" when switching leaders.
      -- RET waits .3s and gives back control,
      -- Any other key does the .3s wait and the action form the key
      -- at once.
      advanceAtoms <- if timed
                      then advanceTime leaderNew
                      else return []
      mapM_ atomicSendSem $ leadAtoms ++ atoms ++ fadeOut ++ advanceAtoms
      -- Generate extra frames if the actor has already moved during
      -- this clip, so his multiple moves would be collapsed in one frame.
      -- If the actor just change his speed this turn, the test can fail,
      -- but it's a minor UI issue, so let it be.
      let previousClipEnd = timeAdd time $ timeNegate timeClip
          lastSingleMove = timeAddFromSpeed coactor bPre previousClipEnd
      when (btime bPre > lastSingleMove) $
        execAtomic $ broadcastSfxAtomic DisplayPushD
      handleActors cmdSerSem arena

dieSer :: MonadServer m => ActorId -> WriterT [Atomic] m ()
dieSer aid = do  -- TODO: explode if a projectile holding a potion
  body <- getsState $ getActorBody aid
  let fid = bfaction body
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  mleader <- electLeader fid (blid body) aid
  dropAllItems aid body
  tellCmdAtomic $ DestroyActorA aid body {bbag = EM.empty} []
  spawning <- getsState $ flip isSpawningFaction fid
  when (not spawning && isNothing mleader) $ do
    oldSt <- getsState $ gquit . (EM.! fid) . sfaction
    tellCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Killed $ blid body)

-- | Drop all actor's items.
dropAllItems :: MonadActionRO m => ActorId -> Actor -> WriterT [Atomic] m ()
dropAllItems aid b = do
  let f (iid, k) = tellCmdAtomic
                   $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                     (CFloor (blid b) (bpos b))
  mapM_ f $ EM.assocs $ bbag b

electLeader :: MonadServer m
            => FactionId -> LevelId -> ActorId
            -> WriterT [Atomic] m (Maybe ActorId)
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfaction
  if isNothing mleader || mleader == Just aidDead then do
    actorD <- getsState sactorD
    let ours (_, b) = bfaction b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorNotProjAssocs (== fid) lid
    Config{configFirstDeathEnds} <- getsServer sconfig
    spawning <- getsState $ flip isSpawningFaction fid
    let mleaderNew | configFirstDeathEnds && not spawning = Nothing
                   | otherwise = listToMaybe $ filter (/= aidDead)
                                 $ map fst $ onLevel ++ party
    tellCmdAtomic $ LeadFactionA fid mleader mleaderNew
    return mleaderNew
  else return mleader

-- | Advance the move time for the given actor.
advanceTime :: MonadActionRO m => ActorId -> m [Atomic]
advanceTime aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  -- TODO: Add an option to block this for non-projectiles too.
  if bhp b < 0 && bproj b then return [] else do
    let speed = actorSpeed coactor b
        t = ticksPerMeter speed
    return [CmdAtomic $ AgeActorA aid t]

-- | Generate a monster, possibly.
generateMonster :: MonadServer m => LevelId -> WriterT [Atomic] m ()
generateMonster arena = do
  cops@Kind.COps{cofact=Kind.Ops{okind}} <- getsState scops
  pers <- getsServer sper
  lvl@Level{ldepth} <- getsLevel arena id
  faction <- getsState sfaction
  s <- getState
  let f fid = fspawn (okind (gkind (faction EM.! fid))) > 0
      spawns = actorNotProjList f arena s
  rc <- rndToAction $ monsterGenChance ldepth (length spawns)
  when rc $ do
    let allPers = ES.unions $ map (totalVisible . (EM.! arena)) $ EM.elems pers
    pos <- rndToAction $ rollSpawnPos cops allPers arena lvl s
    spawnMonsters [pos] arena

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
regenerateLevelHP :: MonadServer m => LevelId -> WriterT [Atomic] m ()
regenerateLevelHP arena = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  time <- getsState $ getLocalTime arena
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
    getsState $ catMaybes . map pick . actorNotProjAssocs (const True) arena
  mapM_ (\aid -> tellCmdAtomic $ HealActorA aid 1) toRegen

-- | Continue or restart or exit the game.
endOrLoop :: (MonadAction m, MonadServerConn m) => m () -> m ()
endOrLoop loopServer = do
  faction <- getsState sfaction
  let f (_, Faction{gquit=Nothing}) = Nothing
      f (fid, Faction{gquit=Just quit}) = Just (fid, quit)
  processQuits loopServer $ mapMaybe f $ EM.assocs faction

processQuits :: (MonadAction m, MonadServerConn m)
             => m () -> [(FactionId, (Bool, Status))] -> m ()
processQuits loopServer [] = loopServer  -- just continue
processQuits loopServer ((fid, quit) : quits) = do
  cops <- getsState scops
  faction <- getsState sfaction
  let fac = faction EM.! fid
  total <- case gleader fac of
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
          isAllied fid1 fact = fid1 `elem` gally fact
          inGameHuman fact = inGame fact && isHumanFact fact
          gameHuman = filter inGameHuman $ EM.elems faction
          gameOver = case filter (isActive . snd) $ EM.assocs faction of
            _ | null gameHuman -> True  -- no screensaver mode for now
            [] -> True
            (fid1, fact1) : rest ->
              -- Competitive game ends when only one allied team remains.
              all (isAllied fid1 . snd) rest
              -- Cooperative game continues until the last ally dies.
              && not (isAllied fid fact1)
      if gameOver then do
        registerScore status total
        restartGame loopServer
      else
        processQuits loopServer quits
    status@Victor -> do
      registerScore status total
      restartGame loopServer
    Restart -> restartGame loopServer
    Camping -> do
      execAtomic $ do
        tellCmdAtomic $ QuitFactionA fid (Just quit) Nothing
        tellCmdAtomic SaveExitA
      saveGameSer
      -- Con't call @loopServer@, that is, quit the game loop.

restartGame :: (MonadAction m, MonadServerConn m) => m () -> m ()
restartGame loopServer = do
  cops <- getsState scops
  nH <- nHumans
  when (nH <= 1) $ execAtomic $ broadcastSfxAtomic $ \fid -> FadeoutD fid False
  gameReset cops
  initPer
  execAtomic reinitGame
  -- Save ASAP in case of crashes and disconnects.
  saveBkpAll
  loopServer
