{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer) where

import Control.Arrow (second, (&&&))
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, execWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdAtomicSem
import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.Config
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
loopSer :: forall m . (MonadAction m, MonadServerConn m)
        => DebugModeSer
        -> (CmdSer -> m [Atomic])
        -> (FactionId -> Conn CmdClientUI -> IO ())
        -> (FactionId -> Conn CmdClientAI -> IO ())
        -> Kind.COps
        -> m ()
loopSer sdebugNxt cmdSerSem executorUI executorAI cops = do
  -- Recover states.
  restored <- tryRestore cops
  -- TODO: use the _msg somehow
  case restored of
    Right _msg -> do  -- Starting a new game.
      -- Set up commandline debug mode
      modifyServer $ \ser -> ser {sdebugNxt}
      gameReset cops
    Left (gloRaw, ser, _msg) -> do  -- Running a restored game.
      putState $ updateCOps (const cops) gloRaw
      putServer ser {sdebugNxt}
  -- Set up connections
  connServer
  -- Launch clients.
  launchClients executorUI executorAI
  -- Init COps and perception according to debug from savegame.
  debugSerOld <- getsServer sdebugSer
  modifyState $ updateCOps $ speedupCOps (sallClear debugSerOld)
  -- Apply debug options that don't need a new game.
  modifyServer $ \ser ->
    ser {sdebugSer = (sdebugSer ser) { sniffIn = sniffIn sdebugNxt
                                     , sniffOut = sniffOut sdebugNxt
                                     , sallClear = sallClear sdebugNxt
                                     , stryFov = stryFov sdebugNxt }}
  -- Set up COps according to new debug.
  debugSerNew <- getsServer sdebugSer
  modifyState $ updateCOps $ speedupCOps (sallClear debugSerNew)
  -- Detect if it's resume or fresh start.
  quit <- getsServer squit
  if isJust quit then do  -- game restored from a savefile
    initPer
    pers <- getsServer sper
    cmds <- execWriterT $ broadcastCmdAtomic $ \fid ->
      ResumeA fid (pers EM.! fid)
    mapM_ atomicSendSem cmds
    modifyServer $ \ser -> ser {squit = Nothing}
  else do  -- game restarted
    cmds <- execWriterT reinitGame
    mapM_ atomicSendSem cmds
    -- Save ASAP in case of crashes and disconnects.
    saveBkpAll
  let cinT = let r = timeTurn `timeFit` timeClip
             in assert (r > 2) r
      bkpFreq = cinT * 100
  -- Loop.
  let loop :: Int -> m ()
      loop clipN = do
        let clipMod = clipN `mod` cinT
            run arena = handleActors cmdSerSem arena timeZero
            factionArena fac =
              case gleader fac of
                Nothing -> return Nothing
                Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
        faction <- getsState sfaction
        marenas <- mapM factionArena $ EM.elems faction
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
        mapM_ run arenas
        quitS <- getsServer squit
        case quitS of
          Just q ->
            handleSave q (loop (clipN + 1))
          Nothing -> do
            when (clipN `mod` bkpFreq == 0) saveBkpAll
            -- Regenerate HP and add monsters each turn, not each clip.
            when (clipMod == 1) $ mapM_ generateMonster arenas
            when (clipMod == 2) $ mapM_ regenerateLevelHP arenas
            mapM_ ageLevel arenas
            endOrLoop (loop (clipN + 1))
  loop 1

ageLevel :: (MonadAction m, MonadServerConn m) => LevelId -> m ()
ageLevel lid = atomicSendSem $ CmdAtomic $ AgeLevelA lid timeClip

saveBkpAll :: (MonadAction m, MonadServerConn m) => m ()
saveBkpAll = do
  atomicSendSem $ CmdAtomic SaveBkpA
  saveGameBkp

initPer :: MonadServer m => m ()
initPer = do
  cops <- getsState scops
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  let tryFov = stryFov $ sdebugSer ser
      fovMode = fromMaybe (configFovMode config) tryFov
      pers = dungeonPerception cops fovMode glo
  modifyServer $ \ser1 -> ser1 {sper = pers}

seenAtomicCli :: Bool -> FactionId -> Perception -> PosAtomic -> Bool
seenAtomicCli knowEvents fid per posAtomic =
  case posAtomic of
    PosLevel _ ps -> knowEvents || all (`ES.member` totalVisible per) ps
    PosOnly fid2 -> fid == fid2
    PosAndSer fid2 -> fid == fid2
    PosAll -> True

seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosOnly _ -> False
    _ -> True

atomicServerSem :: MonadAction m => PosAtomic -> Atomic -> m ()
atomicServerSem posAtomic atomic =
  when (seenAtomicSer posAtomic) $
    case atomic of
      CmdAtomic cmd -> cmdAtomicSem cmd
      SfxAtomic _ -> return ()

-- | Send an atomic action to all clients that can see it.
atomicSendSem :: (MonadAction m, MonadServerConn m) => Atomic -> m ()
atomicSendSem atomic = do
  -- Gather data from the old state.
  sOld <- getState
  persOld <- getsServer sper
  (ps, resets, atomicBroken, psBroken, psLoud) <-
    case atomic of
      CmdAtomic cmd -> do
        ps <- posCmdAtomic cmd
        resets <- resetsFovAtomic cmd
        atomicBroken <- breakCmdAtomic cmd
        psBroken <- mapM posCmdAtomic atomicBroken
        psLoud <- mapM loudCmdAtomic atomicBroken
        return (ps, resets, atomicBroken, psBroken, psLoud)
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        return (ps, Just [], [], [], [])
  let atomicPsBroken = zip3 atomicBroken psBroken psLoud
  -- TODO: assert also that the sum of psBroken is equal to ps
  -- TODO: with deep equality these assertions can be expensive. Optimize.
  assert (case ps of
            PosLevel{} -> True
            _ -> resets == Just []
                 && (null atomicBroken
                     || fmap CmdAtomic atomicBroken == [atomic])) skip
  -- Perform the action on the server.
  atomicServerSem ps atomic
  -- Send some actions to the clients, one faction at a time.
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  let sendA fid cmd = do
        sendUpdateUI fid $ CmdAtomicUI cmd
        sendUpdateAI fid $ CmdAtomicAI cmd
      sendUpdate fid (CmdAtomic cmd) = sendA fid cmd
      sendUpdate fid (SfxAtomic sfx) = sendUpdateUI fid $ SfxAtomicUI sfx
      breakSend fid perNew = do
        let send2 (atomic2, ps2, loud2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid $ CmdAtomic atomic2
                else when loud2 $
                       sendUpdate fid
                       $ SfxAtomic $ BroadcastD "You hear some noises."
        mapM_ send2 atomicPsBroken
      anySend fid perOld perNew = do
        let startSeen = seenAtomicCli knowEvents fid perOld ps
            endSeen = seenAtomicCli knowEvents fid perNew ps
        if startSeen && endSeen
          then sendUpdate fid atomic
          else breakSend fid perNew
      send fid = case ps of
        PosLevel arena _ -> do
          let perOld = persOld EM.! fid EM.! arena
              resetsFid = maybe True (fid `elem`) resets
          if resetsFid then do
            resetFidPerception fid arena
            perNew <- getPerFid fid arena
            let inPer = diffPer perNew perOld
                inPA = perActor inPer
                outPer = diffPer perOld perNew
                outPA = perActor outPer
            if EM.null outPA && EM.null inPA
              then anySend fid perOld perOld
              else do
                sendA fid $ PerceptionA arena outPA inPA
                mapM_ (sendA fid) $ atomicRemember arena inPer sOld
                anySend fid perOld perNew
          else anySend fid perOld perOld
        -- In the following cases, from the assertion above,
        -- @resets@ is false here and broken atomic has the same ps.
        PosOnly fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosAndSer fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosAll -> sendUpdate fid atomic
  faction <- getsState sfaction
  mapM_ send $ EM.keys faction

atomicRemember :: LevelId -> Perception -> State -> [CmdAtomic]
atomicRemember lid inPer s =
  let inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      pMaybe p = maybe Nothing (\x -> Just (p, x))
      inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p (lfloor lvl)) inFov
      fItem p (iid, k) = SpotItemA iid (getItemBody iid s) k (CFloor lid p)
      fBag (p, bag) = map (fItem p) $ EM.assocs bag
      inItem = concatMap fBag inFloor
      -- No @outItem@, for items that became out of sight. The client will
      -- create these atomic actions based on @outPer@, if required.
      -- Any client that remembers out of sight items, OTOH,
      -- will create atomic actions that forget remembered items
      -- that are revealed not to be there any more (no @SpotItemA@ for them).
      inPrio = mapMaybe (\p -> posToActor p lid s) inFov
      fActor aid = SpotActorA aid (getActorBody aid s) (getActorItem aid s)
      inActor = map fActor inPrio
      -- No @outActor@, for the same reason as with @outItem@.
      inTileMap = map (\p -> (p, ltile lvl Kind.! p)) inFov
      -- No @outTlie@, for the same reason as above.
      atomicTile = if null inTileMap then [] else [SpotTileA lid inTileMap]
  in inItem ++ inActor ++ atomicTile

-- TODO: switch levels alternating between player factions,
-- if there are many and on distinct levels.
-- TODO: If a faction has no actors left in the dungeon,
-- announce game end for this faction. Not sure if here's the right place.
-- TODO: Let the spawning factions that remain duke it out somehow,
-- if the player requests to see it.
-- | If no actor of a non-spawning faction on the level,
-- switch levels. If no level to switch to, end game globally.
-- TODO: instead check if a non-spawn faction has Nothing leader. Equivalent.
checkEndGame :: (MonadAction m, MonadServerConn m) => m ()
checkEndGame = do
  -- Actors on the current level go first so that we don't switch levels
  -- unnecessarily.
  as <- getsState $ EM.elems . sactorD
  glo <- getState
  let aNotSp = filter (not . isSpawningFaction glo . bfaction) as
  case aNotSp of
    [] -> gameOver (error "checkEndGame") (error "checkEndGame") True  -- TODO: should send to all factions
    _ : _ -> return ()

-- | End game, showing the ending screens, if requested.
gameOver :: MonadAction m => FactionId -> LevelId -> Bool -> m ()
gameOver fid arena showEndingScreens = do
  deepest <- getsLevel arena ldepth  -- TODO: use deepest visited instead of current
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateFaction (EM.adjust upd fid)
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- error "TODO: sum over all levels? getsState getTime"
    let (bag, total) = calculateTotal fid arena s
        failMsg | timeFit time timeTurn < 300 =
          "That song shall be short."
                | total < 100 =
          "Born poor, dies poor."
                | deepest < 4 && total < 500 =
          "This should end differently."
                | deepest < depth - 1 =
          "This defeat brings no dishonour."
                | deepest < depth =
          "That is your name. 'Almost'."
                | otherwise =
          "Dead heroes make better legends."
        currencyName = MU.Text $ oname $ ouniqGroup "currency"
        _loseMsg = makePhrase
          [ failMsg
          , "You left"
          , MU.NWs total currencyName
          , "and some junk." ]
    if EM.null bag
      then do
        let upd2 f = f {gquit = Just (True, Killed arena)}
        modifyState $ updateFaction (EM.adjust upd2 fid)
      else do
        -- TODO: do this for the killed factions, not for side
--        go <- sendQueryUI fid $ ConfirmShowItemsFloorCli loseMsg bag
--        when go $ do
          let upd2 f = f {gquit = Just (True, Killed arena)}
          modifyState $ updateFaction (EM.adjust upd2 fid)

-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less than or equal to the current time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. Otherwise we push exactly one frame or frame delay.
-- We start by updating perception, because the selected level of dungeon
-- has changed since last time (every change, whether by human or AI
-- or @generateMonster@ is followd by a call to @handleActors@).
handleActors :: (MonadAction m, MonadServerConn m)
             => (CmdSer -> m [Atomic])
             -> LevelId
             -> Time  -- ^ start time of current subclip, exclusive
             -> m ()
handleActors cmdSerSem arena subclipStart = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState $ getTime arena  -- the end time of this clip, inclusive
  prio <- getsLevel arena lprio
  quitS <- getsServer squit
  faction <- getsState sfaction
  s <- getState
  let mnext =
        let -- Actors of the same faction move together.
            -- TODO: insert wrt order, instead of sorting
            isLeader (a1, b1) =
              not $ Just a1 == gleader (faction EM.! bfaction b1)
            order = Ord.comparing $ ((>= 0) . bhp . snd) &&& bfaction . snd
                                    &&& isLeader &&& bsymbol . snd
            (atime, as) = EM.findMin prio
            ams = map (\a -> (a, getActorBody a s)) as
            (actor, m) = head $ sortBy order ams
        in if atime > time
           then Nothing  -- no actor is ready for another move
           else Just (actor, m)
  case mnext of
    _ | isJust quitS -> return ()
    Nothing -> do
-- Disabled until the code is stable, not to pollute commands debug logs:
--      when (subclipStart == timeZero) $
--        mapM_ atomicSendSem $ map (Right . DisplayDelayD) $ EM.keys faction
      return ()
    Just (aid, b) | bhp b <= 0 && not (bproj b) || bhp b < 0
                    || maybe False null (bpath b) -> do
      atoms <-
        if bproj b && bhp b < 0  -- a projectile hitting an actor
        then do
          -- Items are destroyed.
          ais <- getsState $ getActorItem aid
          return [CmdAtomic $ DestroyActorA aid b ais]
        else
          -- Items drop to the ground and new leader elected.
          execWriterT $ dieSer aid
      mapM_ atomicSendSem atoms
      -- Death or projectile impact are serious, new subclip.
      handleActors cmdSerSem arena (btime b)
    Just (actor, body) -> do
      let hasLeader fid = isJust $ gleader $ faction EM.! fid
          allPush = map (SfxAtomic . DisplayPushD)
                    $ filter hasLeader $ EM.keys faction
            -- TODO: too often, at least in multiplayer
      mapM_ atomicSendSem allPush
      let side = bfaction body
          fac = faction EM.! side
          mleader = gleader fac
          isHuman = isHumanFact fac
          usesAI = usesAIFact fac
          hasHumanLeader = isNothing $ gAiLeader fac
      if not usesAI || hasHumanLeader && Just actor == mleader
        then do
          -- TODO: check that the command is legal, that is, correct side, etc.
          cmdS <- sendQueryUI side actor
          atoms <- cmdSerSem cmdS
          let isFailure cmd =
                case cmd of SfxAtomic FailureD{} -> True; _ -> False
              aborted = all isFailure atoms
              timed = timedCmdSer cmdS
              leaderNew = aidCmdSer cmdS
              leadAtoms =
                if leaderNew /= actor
                then [CmdAtomic (LeadFactionA side mleader (Just leaderNew))]
                else []
          nH <- nHumans
          -- TODO: do not fade out if all other are running (so the previous
          -- move was of the same actor)
          let fadeOut | nH > 1 =
                -- More than one human player, mark end of turn.
                [ SfxAtomic $ FadeoutD side True
                , SfxAtomic $ FlushFramesD side
                , SfxAtomic $ FadeinD side True ]
                   | otherwise =
                -- At most one human player, no need to send anything.
                []
          advanceAtoms <- if aborted || not timed
                          then return []
                          else fmap (++ fadeOut) $ advanceTime leaderNew
          mapM_ atomicSendSem $ leadAtoms ++ atoms ++ advanceAtoms
          if aborted then handleActors cmdSerSem arena subclipStart
          else do
            -- Advance time once, after the leader switched perhaps many times.
            -- TODO: this is correct only when all heroes have the same
            -- speed and can't switch leaders by, e.g., aiming a wand
            -- of domination. We need to generalize by displaying
            -- "(next move in .3s [RET]" when switching leaders.
            -- RET waits .3s and gives back control,
            -- Any other key does the .3s wait and the action form the key
            -- at once. This requires quite a bit of refactoring
            -- and is perhaps better done when the other factions have
            -- selected leaders as well.
            bNew <- getsState $ getActorBody leaderNew
            -- Human moves always start a new subclip.
            -- TODO: send messages with time (or at least DisplayPushCli)
            -- and then send DisplayPushCli only to actors that see _pos.
            -- Right now other need it too, to notice the delay.
            -- This will also be more accurate since now unseen
            -- simultaneous moves also generate delays.
            -- TODO: when changing leaders of different levels, if there's
            -- abort, a turn may be lost. Investigate/fix.
            handleActors cmdSerSem arena (btime bNew)
        else do
          cmdS <- sendQueryAI side actor
          atoms <- cmdSerSem cmdS
          let isFailure cmd =
                case cmd of SfxAtomic FailureD{} -> True; _ -> False
              aborted = all isFailure atoms
              timed = timedCmdSer cmdS
              leaderNew = aidCmdSer cmdS
              leadAtoms =
                if leaderNew /= actor
                then -- Only leader can change leaders
                     assert (mleader == Just actor)
                       [CmdAtomic (LeadFactionA side mleader (Just leaderNew))]
                else []
          advanceAtoms <- if aborted || not timed
                          then return []
                          else advanceTime leaderNew
          mapM_ atomicSendSem $ leadAtoms ++ atoms ++ advanceAtoms
          let subclipStartDelta = timeAddFromSpeed coactor body subclipStart
          if not aborted && isHuman && not (bproj body)
             || subclipStart == timeZero
             || btime body > subclipStartDelta
            then do
              -- Start a new subclip if its our own faction moving
              -- or it's another faction, but it's the first move of
              -- this whole clip or the actor has already moved during
              -- this subclip, so his multiple moves would be collapsed.
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              bNew <- getsState $ getActorBody leaderNew
              handleActors cmdSerSem arena (btime bNew)
            else
              -- No new subclip.
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              handleActors cmdSerSem arena subclipStart

dieSer :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
dieSer aid = do  -- TODO: explode if a projectile holding a potion
  body <- getsState $ getActorBody aid
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  electLeader (bfaction body) (blid body) aid
  dropAllItems aid body
  tellCmdAtomic $ DestroyActorA aid body {bbag = EM.empty} []
--  Config{configFirstDeathEnds} <- getsServer sconfig

-- | Drop all actor's items.
dropAllItems :: MonadActionRO m => ActorId -> Actor -> WriterT [Atomic] m ()
dropAllItems aid b = do
  let f (iid, k) = tellCmdAtomic
                   $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                     (CFloor (blid b) (bpos b))
  mapM_ f $ EM.assocs $ bbag b

electLeader :: MonadActionRO m
            => FactionId -> LevelId -> ActorId -> WriterT [Atomic] m ()
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfaction
  when (isNothing mleader || mleader == Just aidDead) $ do
    actorD <- getsState sactorD
    let ours (_, b) = bfaction b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorNotProjAssocs (== fid) lid
    let mleaderNew = listToMaybe $ filter (/= aidDead)
                     $ map fst $ onLevel ++ party
    tellCmdAtomic $ LeadFactionA fid mleader mleaderNew

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

-- | Save game and perhaps exit.
handleSave :: (MonadAction m, MonadServerConn m)
           => Bool -> m () -> m ()
handleSave q loopServer = do
  if q then do
      -- Save and display in parallel.
    --      mv <- liftIO newEmptyMVar
    saveGameSer
    --      liftIO $ void
    --        $ forkIO (Save.saveGameSer config s ser `finally` putMVar mv ())
    -- 7.6        $ forkFinally (Save.saveGameSer config s ser) (putMVar mv ())
    --      tryIgnore $ do
    --        handleScores False Camping total
    --        broadcastUI [] $ MoreFullCli "See you soon, stronger and braver!"
            -- TODO: show the above
    atomicSendSem $ CmdAtomic SaveExitA
    --      liftIO $ takeMVar mv  -- wait until saved
          -- Do nothing, that is, quit the game loop.
  else do
    saveBkpAll
    modifyServer $ \ser1 -> ser1 {squit = Nothing}
    loopServer

-- | Continue or restart or exit the game.
endOrLoop :: (MonadAction m, MonadServerConn m)
          => m () -> m ()
endOrLoop loopServer = do
  checkEndGame
  faction <- getsState sfaction
  let f (_, Faction{gquit=Nothing}) = Nothing
      f (fid, Faction{gquit=Just quit}) = Just (fid, quit)
  case mapMaybe f $ EM.assocs faction of
    [] -> loopServer  -- just continue
    (fid, quit) : _ -> do
      fac <- getsState $ (EM.! fid) . sfaction
      _total <- case gleader fac of
        Nothing -> return 0
        Just leader -> do
          b <- getsState $ getActorBody leader
          getsState $ snd . calculateTotal fid (blid b)
      -- The first, boolean component of quit determines
      -- if ending screens should be shown, the other argument describes
      -- the cause of the disruption of game flow.
      case quit of
        (_showScreens, _status@Killed{}) -> do
--           -- TODO: rewrite; handle killed faction, if human, mostly ignore if not
--           nullR <- undefined -- sendQueryCli fid NullReportCli
--           unless nullR $ do
--             -- Display any leftover report. Suggest it could be the death cause.
--             broadcastUI $ MoreBWUI "Who would have thought?"
--           tryWith
--             (\ finalMsg ->
--               let highScoreMsg = "Let's hope another party can save the day!"
--                   msg = if T.null finalMsg then highScoreMsg else finalMsg
--               in broadcastUI $ MoreBWUI msg
--               -- Do nothing, that is, quit the game loop.
--             )
--             (do
--                when showScreens $ handleScores fid True status total
--                go <- undefined  -- sendQueryUI fid
-- --                     $ ConfirmMoreBWUI "Next time will be different."
--                when (not go) $ abortWith "You could really win this time."
               restartGame loopServer
        (_showScreens, _status@Victor) -> do
          -- nullR <- undefined -- sendQueryCli fid NullReportCli
          -- unless nullR $ do
          --   -- Display any leftover report. Suggest it could be the master move.
          --   broadcastUI $ MoreFullUI "Brilliant, wasn't it?"
          -- when showScreens $ do
          --   tryIgnore $ handleScores fid True status total
          --   broadcastUI $ MoreFullUI "Can it be done better, though?"
          restartGame loopServer
        (_, Restart) -> restartGame loopServer
        (_, Camping) -> assert `failure` (fid, quit)

restartGame :: (MonadAction m, MonadServerConn m) => m () -> m ()
restartGame loopServer = do
  cops <- getsState scops
  -- TODO: this is too hacky still (funBroadcastCli instead of atomicSendSem)
  nH <- nHumans
  when (nH <= 1) $ do
    cmds <- execWriterT $ broadcastSfxAtomic $ \fid -> FadeoutD fid False
    mapM_ atomicSendSem cmds
  gameReset cops
  cmds <- execWriterT reinitGame
  mapM_ atomicSendSem cmds
  -- Save ASAP in case of crashes and disconnects.
  saveBkpAll
  loopServer

reinitGame :: MonadServer m => WriterT [Atomic] m ()
reinitGame = do
  Kind.COps{ coitem=Kind.Ops{okind}, corule } <- getsState scops
  initPer
  pers <- getsServer sper
  knowMap <- getsServer $ sknowMap . sdebugSer
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which really needs to be updated
  -- at this point to keep clients in sync with server improvements.
  fromGlobal <- getsState localFromGlobal
  glo <- getState
  let defLoc | knowMap = glo
             | otherwise = fromGlobal
  discoS <- getsServer sdisco
  let misteriousSymbols = ritemProject $ Kind.stdRuleset corule
      sdisco = let f ik = isymbol (okind ik) `notElem` misteriousSymbols
               in EM.filter f discoS
  broadcastCmdAtomic $ \fid -> RestartA fid sdisco (pers EM.! fid) defLoc
  populateDungeon
  broadcastSfxAtomic $ \fid -> FadeinD fid False

createFactions :: Kind.COps -> Config -> Rnd FactionDict
createFactions Kind.COps{ cofact=Kind.Ops{opick, okind}
                        , costrat=Kind.Ops{opick=sopick} } config = do
  let g isHuman (gname, fType) = do
        gkind <- opick fType (const True)
        let fk = okind gkind
            genemy = []  -- fixed below
            gally = []  -- fixed below
            gquit = Nothing
        gAiLeader <-
          if isHuman
          then return Nothing
          else fmap Just $ sopick (fAiLeader fk) (const True)
        gAiMember <- fmap Just $ sopick (fAiMember fk) (const True)
        let gleader = Nothing
        return Faction{..}
  lHuman <- mapM (g True) (configHuman config)
  lComputer <- mapM (g False) (configComputer config)
  let rawFs = zip [toEnum 1..] $ lHuman ++ lComputer
      isOfType fType fact =
        let fk = okind $ gkind fact
        in case lookup fType $ ffreq fk of
          Just n | n > 0 -> True
          _ -> False
      enemyAlly fact =
        let f fType = filter (isOfType fType . snd) rawFs
            fk = okind $ gkind fact
            setEnemy = ES.fromList $ map fst $ concatMap f $ fenemy fk
            setAlly  = ES.fromList $ map fst $ concatMap f $ fally fk
            genemy = ES.toList setEnemy
            gally = ES.toList $ setAlly ES.\\ setEnemy
        in fact {genemy, gally}
  return $! EM.fromDistinctAscList $ map (second enemyAlly) rawFs

gameReset :: (MonadAction m, MonadServer m) => Kind.COps -> m ()
gameReset cops@Kind.COps{coitem, corule} = do
  -- Rules config reloaded at each new game start.
  -- Taking the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  (sconfig, dungeonSeed, srandom) <- mkConfigRules corule
  let rnd :: Rnd (FactionDict, FlavourMap, Discovery, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        faction <- createFactions cops sconfig
        sflavour <- dungeonFlavourMap coitem
        (sdisco, sdiscoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops sconfig
        return (faction, sflavour, sdisco, sdiscoRev, freshDng)
  let (faction, sflavour, sdisco, sdiscoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshDepth faction cops
      defSer = emptyStateServer {sdisco, sdiscoRev, sflavour, srandom, sconfig}
  putState defState
  sdebugNxt <- getsServer sdebugNxt
  putServer defSer {sdebugNxt, sdebugSer = sdebugNxt}

-- TODO: use rollSpawnPos in the inner loop
-- | Find starting postions for all factions. Try to make them distant
-- from each other and from any stairs.
findEntryPoss :: Kind.COps -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lstair} k =
  let cminStairDist = chessDist lxsize (fst lstair) (snd lstair)
      dist l poss cmin =
        all (\pos -> chessDist lxsize l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        np <- findPosTry 20 ltile  -- 20 only, for unpredictability
                [ \ l _ -> dist l ps $ 2 * cminStairDist
                , \ l _ -> dist l ps cminStairDist
                , \ l _ -> dist l ps $ cminStairDist `div` 2
                , \ l _ -> dist l ps $ cminStairDist `div` 4
                , const (Tile.hasFeature cotile F.Walkable)
                ]
        nps <- tryFind (np : ps) (n - 1)
        return $ np : nps
      stairPoss = [fst lstair, snd lstair]
  in tryFind stairPoss k

-- Spawn initial actors. Clients should notice that so that they elect leaders.
populateDungeon :: MonadServer m => WriterT [Atomic] m ()
populateDungeon = do
  cops@Kind.COps{cotile} <- getsState scops
  let initialItems (lid, Level{ltile, litemNum}) =
        replicateM litemNum $ do
          pos <- rndToAction
                 $ findPos ltile (const (Tile.hasFeature cotile F.Boring))
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapM_ initialItems $ EM.assocs dungeon
  -- TODO entryLevel should be defined per-faction in content
  let entryLevel = initialLevel
  lvl <- getsLevel entryLevel id
  faction <- getsState sfaction
  config <- getsServer sconfig
  let notSpawning (_, fact) = not $ isSpawningFact cops fact
      needInitialCrew = map fst $ filter notSpawning $ EM.assocs faction
      heroNames = configHeroNames config : repeat []
      initialHeroes (side, ppos, heroName) = do
        psFree <- getsState $ nearbyFreePoints cotile ppos entryLevel
        let ps = take (1 + configExtraHeroes config) $ zip [1..] psFree
        laid <- forM ps $ \ (n, p) ->
          addHero side p entryLevel heroName (Just n)
        mleader <- getsState $ gleader . (EM.! side) . sfaction
        when (mleader == Nothing) $
          tellCmdAtomic $ LeadFactionA side Nothing (Just $ head laid)
  entryPoss <- rndToAction $ findEntryPoss cops lvl (length needInitialCrew)
  mapM_ initialHeroes $ zip3 needInitialCrew entryPoss heroNames

-- * Assorted helper functions

-- TODO: send this to a faction, whenever a monster is generated
-- and the faction was empty before: SfxAtomicUI $ FadeinD fid False
--
-- | Generate a monster, possibly.
generateMonster :: (MonadAction m, MonadServerConn m) => LevelId -> m ()
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
    let allPers =
          ES.unions $ map (totalVisible . (EM.! arena)) $ EM.elems pers
    pos <- rndToAction $ rollSpawnPos cops allPers arena lvl s
    cmds <- execWriterT $ spawnMonsters [pos] arena
    mapM_ atomicSendSem cmds

-- | Create a new monster on the level, at a random position.
rollSpawnPos :: Kind.COps -> ES.EnumSet Point -> LevelId -> Level -> State
             -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible lid lvl s = do
  let inhabitants = actorNotProjList (const True) lid s
      isLit = Tile.isLit cotile
      distantAtLeast d =
        \ l _ -> all (\ h -> chessDist (lxsize lvl) (bpos h) l > d) inhabitants
  findPosTry 40 (ltile lvl)
    [ \ _ t -> not (isLit t)
    , distantAtLeast 15
    , \ l t -> not (isLit t) || distantAtLeast 15 l t
    , distantAtLeast 10
    , \ l _ -> not $ l `ES.member` visible
    , distantAtLeast 5
    , \ l t -> Tile.hasFeature cotile F.Walkable t
               && unoccupied (actorList (const True) lid s) l
    ]

-- | Possibly regenerate HP for all actors on the current level.
--
-- We really want hero selection to be a purely UI distinction,
-- so all heroes need to regenerate, not just the leader.
-- Only the heroes on the current level regenerate (others are frozen
-- in time together with their level). This prevents cheating
-- via sending one hero to a safe level and waiting there.
regenerateLevelHP :: (MonadAction m, MonadServerConn m) => LevelId -> m ()
regenerateLevelHP arena = do
  Kind.COps{ coitem
           , coactor=Kind.Ops{okind}
           } <- getsState scops
  time <- getsState $ getTime arena
  discoS <- getsServer sdisco
  s <- getState
  let pick (a, m) =
        let ak = okind $ bkind m
            itemAssocs = getActorItem a s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen coitem discoS itemAssocs of
                        Just (_, _i)  -> 5 {-* jpower i-} -- TODO: use effect
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0 || deltaHP <= 0
           then Nothing
           else Just a
  toRegen <-
    getsState $ catMaybes . map pick .actorNotProjAssocs (const True) arena
  mapM_ (\aid -> atomicSendSem $ CmdAtomic $ HealActorA aid 1) toRegen

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level.
_addSmell :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
_addSmell aid = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getTime $ blid b
  oldS <- getsLevel (blid b) $ (EM.lookup $ bpos b) . lsmell
  let newTime = timeAdd time smellTimeout
  tellCmdAtomic $ AlterSmellA (blid b) [(bpos b, (oldS, Just newTime))]
