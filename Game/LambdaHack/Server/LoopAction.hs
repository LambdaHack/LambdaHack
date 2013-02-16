{-# LANGUAGE OverloadedStrings #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer, cmdAtomicBroad) where

import Control.Arrow (second)
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, execWriterT, runWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Text as T
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
loopSer :: forall m . (MonadAction m, MonadServerChan m)
        => (LevelId -> CmdSer -> m ())
        -> (FactionId -> ConnCli -> Bool -> IO ())
        -> Kind.COps
        -> m ()
loopSer cmdSer executorC cops = do
  -- Recover states.
  restored <- tryRestore cops
  -- TODO: use the _msg somehow
  case restored of
    Right _msg ->  -- Starting a new game.
      gameReset cops
    Left (gloRaw, ser, _msg) -> do  -- Running a restored game.
      putState $ updateCOps (const cops) gloRaw
      putServer ser
  -- Set up connections
  connServer
  -- Launch clients.
  launchClients executorC
  -- Send init messages.
  initPer
  pers <- getsServer sper
  defLoc <- getsState localFromGlobal
  quit <- getsServer squit
  case quit of
    Nothing -> do  -- game restarted
      funBroadcastCli (\fid -> RestartCli (pers EM.! fid) defLoc)
      populateDungeon
      -- TODO: factor out common parts from restartGame and restoreOrRestart
      cmdAtomicBroad initialLevel $ Left SyncA
      -- Save ASAP in case of crashes and disconnects.
      saveGameBkp
    _ -> do  -- game restored from a savefile
      funBroadcastCli (\fid -> ContinueSavedCli (pers EM.! fid))
  modifyServer $ \ser1 -> ser1 {squit = Nothing}
  let cinT = let r = timeTurn `timeFit` timeClip
             in assert (r > 2) r
  -- Loop.
  let loop :: (Maybe FactionId) -> Int -> m ()
      loop prevHuman clipN = do
        let h pHuman arena = do
              -- Regenerate HP and add monsters each turn, not each clip.
              when (clipN `mod` cinT == 0) $ generateMonster arena
              when (clipN `mod` cinT == 1) $ regenerateLevelHP arena
              when (clipN `mod` cinT == 2) checkEndGame
              pHumanOut <- handleActors cmdSer arena timeZero pHuman
              modifyState $ updateTime arena $ timeAdd timeClip
              return pHumanOut
        let f fac = do
              case gleader fac of
                Nothing -> return Nothing
                Just leader -> do
                  b <- getsState $ getActorBody leader
                  return $ Just $ blid b
        faction <- getsState sfaction
        marenas <- mapM f $ EM.elems faction
        let arenas = ES.toList $ ES.fromList $ catMaybes marenas
            mapH pHuman [] = return pHuman
            mapH pHuman (arena : as) = do
              pHumanOut <- h pHuman arena
              mapH pHumanOut as
        pHumanOut <- mapH prevHuman arenas
        endOrLoop prevHuman (loop pHumanOut (clipN + 1))
  loop Nothing 1

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

atomicSem :: MonadAction m => Atomic -> m ()
atomicSem atomic = case atomic of
  Left cmd -> cmdAtomicSem cmd
  Right _ -> return ()

cmdAtomicBroad :: (MonadAction m, MonadServerChan m)
               => LevelId -> Atomic -> m ()
cmdAtomicBroad arena atomic = do
  lvlOld <- getsLevel arena id
  actorDOld <- getsState sactorD
  itemDOld <- getsState sitemD
  factionOld <- getsState sfaction
  atomicSem atomic
  lvlNew <- getsLevel arena id
  actorDNew <- getsState sactorD
  itemDNew <- getsState sitemD
  factionNew <- getsState sfaction
  (pStart, pEnd) <- case atomic of
    Left cmd -> posCmdAtomic cmd
    Right desc -> posDescAtomic desc
  let sendRem fid cmdRem = do
        sendUpdateCli fid cmdRem
        sendUpdateCliAI fid cmdRem
      sendA fid cmd = do
        sendUpdateUI fid $ CmdAtomicUI cmd
        sendUpdateCliAI fid $ CmdAtomicCli cmd
      sendUpdate fid (Left cmd) = sendA fid cmd
      sendUpdate fid (Right desc) = sendUpdateUI fid $ DescAtomicUI desc
      vis per = all (`ES.member` totalVisible per)
      send fid = do
        perOld <- getPerFid fid arena
        resets <- case atomic of
          Left cmd -> resetsFovAtomic fid cmd
          Right _ -> return False
        perNew <-
          if resets then do
            resetFidPerception fid arena
            perNew <- getPerFid fid arena
            return perNew
          else return perOld
        let startSeen = either id (vis perOld) pStart
            endSeen = either id (vis perNew) pEnd
            seen = startSeen && endSeen
        if resets then do
          if seen then do
            sendRem fid $
              RememberPerCli perNew lvlOld arena actorDOld itemDOld factionOld
            sendUpdate fid atomic
          else
            sendRem fid $
              RememberPerCli perNew lvlNew arena actorDNew itemDNew factionNew
        else do
          if seen then
            sendUpdate fid atomic
          else
            sendRem fid $
              RememberCli lvlNew arena actorDNew itemDNew factionNew
  faction <- getsState sfaction
  mapM_ send $ EM.keys faction

-- TODO: switch levels alternating between player factions,
-- if there are many and on distinct levels.
-- TODO: If a faction has no actors left in the dungeon,
-- announce game end for this faction. Not sure if here's the right place.
-- TODO: Let the spawning factions that remain duke it out somehow,
-- if the player requests to see it.
-- | If no actor of a non-spawning faction on the level,
-- switch levels. If no level to switch to, end game globally.
-- TODO: instead check if a non-spawn faction has Nothing leader. Equivalent.
checkEndGame ::  (MonadAction m, MonadServerChan m) => m ()
checkEndGame = do
  -- Actors on the current level go first so that we don't switch levels
  -- unnecessarily.
  as <- getsState $ EM.elems . sactorD
  glo <- getState
  let aNotSp = filter (not . isSpawningFaction glo . bfaction) as
  case aNotSp of
    [] -> gameOver undefined undefined True  -- TODO: should send to all factions
    _ : _ -> return ()

-- | End game, showing the ending screens, if requested.
gameOver :: (MonadAction m, MonadServerChan m)
         => FactionId -> LevelId -> Bool -> m ()
gameOver fid arena showEndingScreens = do
  deepest <- getsLevel arena ldepth  -- TODO: use deepest visited instead of current
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateFaction (EM.adjust upd fid)
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- undefined  -- TODO: sum over all levels? getsState getTime
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
handleActors :: (MonadAction m, MonadServerChan m)
             => (LevelId -> CmdSer -> m ())
             -> LevelId
             -> Time  -- ^ start time of current subclip, exclusive
             -> Maybe FactionId
             -> m (Maybe FactionId)
handleActors cmdSer arena subclipStart prevHuman = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState $ getTime arena  -- the end time of this clip, inclusive
  prio <- getsLevel arena lprio
  gquit <- case prevHuman of
    Just fid -> getsState $ gquit . (EM.! fid) . sfaction
    Nothing -> return Nothing
  quit <- getsServer squit
  s <- getState
  let mnext = if EM.null prio  -- wait until any actor spawned
              then Nothing
              else let -- Actors of the same faction move together.
--                       order = Ord.comparing (btime . snd &&& bfaction . snd)
                       (atime, as) = EM.findMin prio
                       actor = head as
                       m = getActorBody actor s
                   in if atime > time
                      then Nothing  -- no actor is ready for another move
                      else Just (actor, m)
  case mnext of
    _ | isJust quit || isJust gquit -> return prevHuman
    Nothing -> do
      when (subclipStart == timeZero) $
        broadcastUI DisplayDelayUI
      return prevHuman
    Just (actor, body) | bhp body <= 0 && not (bproj body) -> do
      cmdSer arena $ DieSer actor
      -- Death is serious, new subclip.
      handleActors cmdSer arena (btime body) prevHuman
    Just (actor, body) -> do
      broadcastUI DisplayPushUI  -- TODO: too often
      let side = bfaction body
      isHuman <- getsState $ flip isHumanFaction side
      mleader <- getsState $ gleader . (EM.! side) . sfaction
      if Just actor == mleader && isHuman
        then do
          nHuman <- if isHuman && Just side /= prevHuman
                    then do
                      newRuns <- sendQueryCli side IsRunningCli
                      if newRuns
                        then return prevHuman
                        else do
                          case prevHuman of
                            Just fid -> do
                              b <- sendQueryUI fid $ FlushFramesUI side
                              if b
                                then return $ Just side
                                else return prevHuman
                            Nothing -> return $ Just side
                    else return prevHuman
          -- TODO: check that the command is legal, that is, the leader
          -- is acting, etc. Or perhaps instead have a separate type
          -- of actions for humans. OTOH, AI is controlled by the servers
          -- so the generated commands are assumed to be legal.
          cmdS <- sendQueryUI side HandleHumanUI
          tryWith (\msg -> do
                      sendUpdateCli side $ ShowMsgCli msg
                      sendUpdateUI side DisplayPushUI
                      handleActors cmdSer arena subclipStart nHuman
                  ) $ do
            mapM_ (cmdSer arena) cmdS
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
            mleaderNew <- getsState $ gleader . (EM.! side) . sfaction
            quitNew <- getsServer squit
            bodyNew <- case mleaderNew of
              Just leaderNew | any timedCmdSer cmdS && isNothing quitNew -> do
                bNew <- getsState $ getActorBody leaderNew
                advanceTime (blid bNew) leaderNew
                return bNew
              _ -> return body
            -- Human moves always start a new subclip.
            -- _pos <- getsState $ bpos . getActorBody (fromMaybe actor leaderNew)
            -- TODO: send messages with time (or at least DisplayPushCli)
            -- and then send DisplayPushCli only to actors that see _pos.
            -- Right now other need it too, to notice the delay.
            -- This will also be more accurate since now unseen
            -- simultaneous moves also generate delays.
            handleActors cmdSer arena (btime bodyNew) nHuman
        else do
--          recordHistory
          advanceTime arena actor  -- advance time while the actor still alive
          let subclipStartDelta = timeAddFromSpeed coactor body subclipStart
          if isHuman && not (bproj body)
             || subclipStart == timeZero
             || btime body > subclipStartDelta
            then do
              -- Start a new subclip if its our own faction moving
              -- or it's another faction, but it's the first move of
              -- this whole clip or the actor has already moved during
              -- this subclip, so his multiple moves would be collapsed.
              cmdS <- sendQueryCliAI side $ HandleAICli actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                               then return ()
                               else assert `failure` msg <> "in AI"
                      )
                      (cmdSer arena cmdS)
              handleActors cmdSer arena (btime body) prevHuman
            else do
              -- No new subclip.
              cmdS <- sendQueryCliAI side $ HandleAICli actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                               then return ()
                               else assert `failure` msg <> "in AI"
                      )
                      (cmdSer arena cmdS)
              handleActors cmdSer arena subclipStart prevHuman

-- | Advance the move time for the given actor.
advanceTime :: MonadAction m => LevelId -> ActorId -> m ()
advanceTime lid aid = do
  Kind.COps{coactor} <- getsState scops
  b <- getsState $ getActorBody aid
  let oldTime = btime b
      newTime = timeAddFromSpeed coactor b oldTime
      upd body = body {btime = newTime}
  modifyState $ updateActorBody aid upd
  let rm Nothing = assert `failure` aid
      rm (Just l) = let l2 = delete aid l
                    in if null l2 then Nothing else Just l2
  updateLevel lid $ updatePrio $ EM.alter rm oldTime
  let add Nothing = Just [aid]
      add (Just l) = Just $ aid : l
  updateLevel lid $ updatePrio $ EM.alter add newTime

-- | Continue or restart or exit the game.
endOrLoop :: (MonadAction m, MonadServerChan m)
          => Maybe FactionId -> m () -> m ()
endOrLoop Nothing loopServer = loopServer
endOrLoop (Just fid) loopServer = do
  quitS <- getsServer squit
  fac <- getsState $ (EM.! fid) . sfaction
  let quitG = gquit fac
  total <- case gleader fac of
    Nothing -> return 0
    Just leader -> do
      b <- getsState $ getActorBody leader
      getsState $ snd . calculateTotal fid (blid b)
  -- The first, boolean component of quit determines
  -- if ending screens should be shown, the other argument describes
  -- the cause of the disruption of game flow.
  case (quitS, quitG) of
    (Just _, _) -> do
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
      broadcastCli GameDisconnectCli
--      liftIO $ takeMVar mv  -- wait until saved
      -- Do nothing, that is, quit the game loop.
    (Nothing, Just (showScreens, status@Killed{})) -> do
      -- TODO: rewrite; handle killed faction, if human, mostly ignore if not
      nullR <- sendQueryCli fid NullReportCli
      unless nullR $ do
        -- Display any leftover report. Suggest it could be the death cause.
        broadcastUI $ MoreBWUI "Who would have thought?"
      tryWith
        (\ finalMsg ->
          let highScoreMsg = "Let's hope another party can save the day!"
              msg = if T.null finalMsg then highScoreMsg else finalMsg
          in broadcastUI $ MoreBWUI msg
          -- Do nothing, that is, quit the game loop.
        )
        (do
           when showScreens $ handleScores fid True status total
           go <- sendQueryUI fid
                 $ ConfirmMoreBWUI "Next time will be different."
           when (not go) $ abortWith "You could really win this time."
           restartGame loopServer
        )
    (Nothing, Just (showScreens, status@Victor)) -> do
      nullR <- sendQueryCli fid NullReportCli
      unless nullR $ do
        -- Display any leftover report. Suggest it could be the master move.
        broadcastUI $ MoreFullUI "Brilliant, wasn't it?"
      when showScreens $ do
        tryIgnore $ handleScores fid True status total
        broadcastUI $ MoreFullUI "Can it be done better, though?"
      restartGame loopServer
    (Nothing, Just (_, Restart)) -> restartGame loopServer
    (Nothing, _) -> loopServer  -- just continue

restartGame :: (MonadAction m, MonadServerChan m) => m () -> m ()
restartGame loopServer = do
  cops <- getsState scops
  gameReset cops
  initPer
  pers <- getsServer sper
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which really needs to be updated
  -- at this point to keep clients in sync with server improvements.
  defLoc <- getsState localFromGlobal
  funBroadcastCli (\fid -> RestartCli (pers EM.! fid) defLoc)
  populateDungeon
  cmdAtomicBroad initialLevel $ Left $ SyncA
  saveGameBkp
  broadcastCli $ ShowMsgCli "This time for real."
  broadcastUI DisplayPushUI
  loopServer

createFactions :: Kind.COps -> Config -> Rnd FactionDict
createFactions Kind.COps{ cofact=Kind.Ops{opick, okind}
                        , costrat=Kind.Ops{opick=sopick} } config = do
  let g isHuman (gname, fType) = do
        gkind <- opick fType (const True)
        let fk = okind gkind
            genemy = []  -- fixed below
            gally  = []  -- fixed below
            gquit = Nothing
        gAiLeader <-
          if isHuman
          then return Nothing
          else fmap Just $ sopick (fAiLeader fk) (const True)
        gAiMember <- sopick (fAiMember fk) (const True)
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
gameReset cops@Kind.COps{coitem, corule, cotile} = do
  -- Rules config reloaded at each new game start.
  -- Taking the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  (sconfig, dungeonSeed, random) <- mkConfigRules corule
  let rnd :: Rnd (FactionDict, FlavourMap, Discoveries, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        faction <- createFactions cops sconfig
        flavour <- dungeonFlavourMap coitem
        (discoS, discoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops sconfig
        return (faction, flavour, discoS, discoRev, freshDng)
  let (faction, flavour, discoS, discoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshDepth discoS faction cops
      defSer = defStateServer discoRev flavour random sconfig
  putState defState
  putServer defSer
  -- Clients have no business noticing initial item creation, so we can
  -- do this here and evaluate with atomicSem, without notifying clients.
  let initialItems (lid, (Level{ltile}, citemNum)) = do
        nri <- rndToAction $ rollDice citemNum
        replicateM nri $ do
          pos <- rndToAction
                 $ findPos ltile (const (Tile.hasFeature cotile F.Boring))
          cmds <- execWriterT $ createItems 1 pos lid
          mapM_ atomicSem cmds
  mapM_ initialItems itemCounts

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
populateDungeon :: (MonadAction m, MonadServerChan m) => m ()
populateDungeon = do
  -- TODO entryLevel should be defined per-faction in content
  let entryLevel = initialLevel
  lvl <- getsLevel entryLevel id
  cops <- getsState scops
  faction <- getsState sfaction
  config <- getsServer sconfig
  let notSpawning (_, fact) = not $ isSpawningFact cops fact
      needInitialCrew = map fst $ filter notSpawning $ EM.assocs faction
      heroNames = configHeroNames config : repeat []
      initialHeroes (side, ppos, heroName) = do
        replicateM_ (1 + configExtraHeroes config) $ do
          cmds <- execWriterT $ addHero side ppos entryLevel heroName
          mapM_ (cmdAtomicBroad entryLevel) cmds
  entryPoss <- rndToAction $ findEntryPoss cops lvl (length needInitialCrew)
  mapM_ initialHeroes $ zip3 needInitialCrew entryPoss heroNames

-- * Assorted helper functions

-- | Generate a monster, possibly.
generateMonster :: (MonadAction m, MonadServerChan m) => LevelId -> m ()
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
    (_, cmds) <- runWriterT $ spawnMonsters 1 pos arena
    mapM_ (cmdAtomicBroad arena) cmds

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
regenerateLevelHP :: (MonadAction m, MonadServerChan m) => LevelId -> m ()
regenerateLevelHP arena = do
  Kind.COps{ coitem
           , coactor=Kind.Ops{okind}
           } <- getsState scops
  time <- getsState $ getTime arena
  discoS <- getsState sdisco
  s <- getState
  let pick (a, m) =
        let ak = okind $ bkind m
            items = getActorItem a s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen coitem discoS items of
                        Just i  -> 5 * jpower i
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0 || deltaHP <= 0
           then Nothing
           else Just a
  toRegen <-
    getsState $ catMaybes . map pick .actorNotProjAssocs (const True) arena
  mapM_ (\aid -> cmdAtomicBroad arena $ Left $ HealActorA aid 1) toRegen

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level.
_addSmell :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
_addSmell aid = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getTime $ blid b
  oldS <- getsLevel (blid b) $ (EM.lookup $ bpos b) . lsmell
  let newTime = timeAdd time smellTimeout
  tellCmdAtomic $ AlterSmellA (blid b) [(bpos b, (oldS, Just newTime))]
