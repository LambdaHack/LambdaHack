-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer) where

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Frontend
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.HandleRequestServer
import Game.LambdaHack.Server.MonadServer hiding (sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.StartAction
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Frequency

-- | Start a game session. Loop, communicating with clients.
loopSer :: (MonadAtomic m, MonadConnServer m)
        => DebugModeSer
        -> (Request -> m Bool)
        -> (FactionId -> ChanFrontend -> ChanServer ResponseUI Request
            -> IO ())
        -> (FactionId -> ChanServer ResponseAI RequestTimed
            -> IO ())
        -> Kind.COps
        -> m ()
loopSer sdebug handleRequest executorUI executorAI !cops = do
  -- Recover states and launch clients.
  restored <- tryRestore cops sdebug
  case restored of
    Just (sRaw, ser) | not $ snewGameSer sdebug -> do  -- run a restored game
      -- First, set the previous cops, to send consistent info to clients.
      let setPreviousCops = const cops
      execCmdAtomic $ ResumeServerA $ updateCOps setPreviousCops sRaw
      putServer ser
      sdebugNxt <- initDebug cops sdebug
      modifyServer $ \ser2 -> ser2 {sdebugNxt}
      applyDebug
      updateConn executorUI executorAI
      initPer
      pers <- getsServer sper
      broadcastCmdAtomic $ \fid -> ResumeA fid (pers EM.! fid)
      -- Second, set the current cops and reinit perception.
      let setCurrentCops = const (speedupCOps (sallClear sdebugNxt) cops)
      -- @sRaw@ is correct here, because none of the above changes State.
      execCmdAtomic $ ResumeServerA $ updateCOps setCurrentCops sRaw
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
      execCmdAtomic $ RestartServerA $ updateCOps speedup s
      updateConn executorUI executorAI
      initPer
      reinitGame
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
          endOrLoop (updateConn executorUI executorAI) loop
        else do
          continue <- endClip arenas
          when continue loop
  loop

initDebug :: MonadReadState m => Kind.COps -> DebugModeSer -> m DebugModeSer
initDebug Kind.COps{corule} sdebugSer = do
  let stdRuleset = Kind.stdRuleset corule
  return $!
    (\dbg -> dbg {sfovMode =
        sfovMode dbg `mplus` Just (rfovMode stdRuleset)}) .
    (\dbg -> dbg {ssavePrefixSer =
        ssavePrefixSer dbg `mplus` Just (rsavePrefix stdRuleset)})
    $ sdebugSer

endClip :: (MonadAtomic m, MonadServer m, MonadConnServer m)
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
  mapM_ (\lid -> execCmdAtomic $ AgeLevelA lid timeClip) arenas
  execCmdAtomic $ AgeGameA timeClip
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
handleActors :: (MonadAtomic m, MonadConnServer m)
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
      execSfxAtomic $ DisplayPushD (bfid b)  -- show last position before drop
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
                         [LeadFactionA side mleader (Just aidNew)]
                  else []
            mapM_ execCmdAtomic leadAtoms
            assert (bfid bPre == side
                    `blame` "client tries to move other faction actors"
                    `twith` (bPre, side)) skip
            return (aidNew, bPre)
          setBWait (ReqTimed ReqWait{}) aidNew bPre = do
            let fromWait = bwait bPre
            unless fromWait $ execCmdAtomic $ WaitActorA aidNew fromWait True
          setBWait _ aidNew bPre = do
            let fromWait = bwait bPre
            when fromWait $ execCmdAtomic $ WaitActorA aidNew fromWait False
          extraFrames bPre = do
            -- Generate extra frames if the actor has already moved during
            -- this clip, so his multiple moves would be collapsed
            -- in one frame.
            -- If the actor changes his speed this very turn,
            -- the test can fail, but it's a minor UI issue, so let it be.
            let previousClipEnd = timeAdd time $ timeNegate timeClip
                lastSingleMove = timeAddFromSpeed bPre previousClipEnd
            when (btime bPre > lastSingleMove) $
              broadcastSfxAtomic DisplayPushD
      if bproj body then do  -- TODO: perhaps check Track, not bproj
        execSfxAtomic $ DisplayPushD side
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
              $ MsgFidD side "You strain, fumble and faint from the exertion."
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
          execSfxAtomic $ DisplayPushD side
        -- Clear messages in the UI client (if any), if the actor
        -- is a leader (which happens when a UI client is fully
        -- computer-controlled). We could record history more often,
        -- to avoid long reports, but we'd have to add -more- prompts.
        let mainUIactor = playerUI (gplayer fact) && aidIsLeader
        when mainUIactor $ execSfxAtomic $ RecordHistoryD side
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

dieSer :: (MonadAtomic m, MonadServer m) => ActorId -> Actor -> Bool -> m ()
dieSer aid b hit = do
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  if bproj b then do
    dropAllItems aid b hit
    b2 <- getsState $ getActorBody aid
    execCmdAtomic $ DestroyActorA aid b2 []
  else do
    execCmdAtomic $ RecordKillA aid 1
    electLeader (bfid b) (blid b) aid
    deduceKilled b
    dropAllItems aid b False
    b2 <- getsState $ getActorBody aid
    execCmdAtomic $ DestroyActorA aid b2 []

-- | Drop all actor's items. If the actor hits another actor and this
-- collision results in all item being dropped, all items are destroyed.
-- If the actor does not hit, but dies, only fragile items are destroyed
-- and only if the actor was a projectile (and so died by dropping
-- to the ground due to exceeded range or bumping off an obstacle).
dropAllItems :: (MonadAtomic m, MonadServer m)
             => ActorId -> Actor -> Bool -> m ()
dropAllItems aid b hit = do
  Kind.COps{coitem, corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  discoS <- getsServer sdisco
  let container = CActor aid CEqp
      loseInv = do
        let g iid k = execCmdAtomic
                      $ MoveItemA iid k (CActor aid CInv) container
        mapActorInv_ g b
  if not rsharedInventory then loseInv
  else do
    fact <- getsState $ (EM.! bfid b) . sfactionD
    case gleader fact of
      Nothing -> loseInv
      Just leader -> do
        let g iid k = execCmdAtomic
                      $ MoveItemA iid k (CActor aid CInv) (CActor leader CInv)
        mapActorInv_ g b
  let isDestroyed item = hit || bproj b && isFragile coitem discoS item
      f iid k = do
        item <- getsState $ getItemBody iid
        if isDestroyed item then
          case isExplosive coitem discoS item of
            Nothing -> execCmdAtomic $ DestroyItemA iid item k container
            Just cgroup -> do
              let ik = fromJust $ jkind discoS item
              execCmdAtomic $ DiscoverA (blid b) (bpos b) iid ik
              execCmdAtomic $ DestroyItemA iid item k container
              explodeItem aid b cgroup
        else
          execCmdAtomic $ MoveItemA iid k container (CActor aid CGround)
  mapActorInv_ f b

explodeItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> Actor -> Text -> m ()
explodeItem aid b cgroup = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth} <- getLevel $ blid b
  depth <- getsState sdepth
  let itemFreq = toFreq "shrapnel group" [(1, cgroup)]
  (item, n1, _) <- rndToAction
                   $ newItem coitem flavour discoRev itemFreq ldepth depth
  let container = CActor aid CEqp
  iid <- registerItem item n1 container False
  let Point x y = bpos b
      projectN n = replicateM_ n $ do
        tpxy <- rndToAction $ do
          border <- randomR (1, 4)
          -- We pick a point at the border, not inside, to have a uniform
          -- distribution for the points the line goes through at each distance
          -- from the source. Otherwise, e.g., the points on cardinal
          -- and diagonal lines from the source would be more common.
          case border :: Int of
            1 -> fmap (Point (x - 10)) $ randomR (y - 10, y + 10)
            2 -> fmap (Point (x + 10)) $ randomR (y - 10, y + 10)
            3 -> fmap (flip Point (y - 10)) $ randomR (x - 10, x + 10)
            4 -> fmap (flip Point (y + 10)) $ randomR (x - 10, x + 10)
            _ -> assert `failure` border
        let eps = px tpxy + py tpxy
        mfail <- projectFail aid tpxy eps iid CEqp True
        case mfail of
          Nothing -> return ()
          Just ProjectBlockTerrain -> return ()
          Just failMsg -> execFailure aid failMsg
  projectN n1
  bag2 <- getsState $ beqp . getActorBody aid
  let mn2 = EM.lookup iid bag2
  maybe skip projectN mn2  -- assume all shrapnels bounce off obstacles once
  bag3 <- getsState $ beqp . getActorBody aid
  let mn3 = EM.lookup iid bag3
  maybe skip (\k -> execCmdAtomic $ LoseItemA iid item k container) mn3

-- | Advance the move time for the given actor and his status effects
-- that are updated once per his move (as opposed to once per a time unit).
advanceTime :: MonadAtomic m => ActorId -> m ()
advanceTime aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let t = ticksPerMeter $ bspeed b
  execCmdAtomic $ AgeActorA aid t
  -- Calm or worry actor by enemies felt (even if not seen)
  -- on the level within 3 tiles.
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorNotProjList (isAtWar fact) (blid b)
  let closeFoes = filter ((<= 3) . chessDist (bpos b) . bpos) allFoes
      calmMax = maxDice $ acalm $ okind $ bkind b
      calmCur = bcalm b
      deltaCalm = if null closeFoes
                  then max 0 $ min 1 (calmMax - calmCur)
                  else max (-1) (-calmCur)
  when (deltaCalm /= 0) $ execCmdAtomic $ CalmActorA aid deltaCalm

-- | Generate a monster, possibly.
generateMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
generateMonster lid = do
  cops <- getsState scops
  pers <- getsServer sper
  lvl@Level{ldepth} <- getLevel lid
  s <- getState
  let f fid = isSpawnFaction fid s
      spawns = actorNotProjList f lid s
  depth <- getsState sdepth
  rc <- rndToAction $ monsterGenChance ldepth depth (length spawns)
  factionD <- getsState sfactionD
  when rc $ do
    time <- getsState $ getLocalTime lid
    let freq = toFreq "spawn"
               $ map (\(fid, fact) -> (playerSpawn $ gplayer fact, fid))
               $ EM.assocs factionD
    mfid <- if nullFreq freq then
              return Nothing
            else fmap Just $ rndToAction $ frequency freq
    case mfid of
      Nothing -> return ()  -- no faction spawns
      Just fid -> do
        let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                      $ EM.elems $ EM.delete fid pers  -- expensive :(
        pos <- rndToAction $ rollSpawnPos cops allPers lid lvl fid s
        spawnMonsters [pos] lid time fid

rollSpawnPos :: Kind.COps -> ES.EnumSet Point
             -> LevelId -> Level -> FactionId -> State
             -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible
             lid Level{ltile, lxsize, lysize} fid s = do
  let factionDist = max lxsize lysize - 5
      inhabitants = actorNotProjList (/= fid) lid s
      as = actorList (const True) lid s
      isLit = Tile.isLit cotile
      distantAtLeast d p _ =
        all (\b -> chessDist (bpos b) p > d) inhabitants
  findPosTry 40 ltile
    ( \p t -> Tile.isWalkable cotile t
              && unoccupied as p)
    [ \_ t -> not (isLit t)  -- no such tiles on some maps
    , distantAtLeast factionDist
    , distantAtLeast $ factionDist `div` 2
    , \p _ -> not $ p `ES.member` visible
    , distantAtLeast $ factionDist `div` 3
    , \_ t -> Tile.hasFeature cotile F.CanActor t  -- in reachable area
    , distantAtLeast $ factionDist `div` 4
    , distantAtLeast 3  -- otherwise a fast actor can walk and hit in one turn
    ]

-- TODO: generalize to any list of items (or effects) applied to all actors
-- every turn. Specify the list per level in config.
-- TODO: use itemEffect or at least effectSem to get from Regeneration
-- to HealActorA. Also, Applying an item with Regeneration should do the same
-- thing, but immediately (and destroy the item).
-- | Possibly regenerate HP for all actors on the current level.
--
-- We really want leader picking to be a purely UI distinction,
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
            eqpAssocs = getEqpAssocs m s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen eqpAssocs of
                        Just (k, _)  -> k + 1
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0
              || deltaHP <= 0
              || bhp m <= 0
           then Nothing
           else Just a
  toRegen <- getsState $ mapMaybe approve . actorNotProjAssocs (const True) lid
  mapM_ (\aid -> execCmdAtomic $ HealActorA aid 1) toRegen

leadLevelFlip :: (MonadAtomic m, MonadServer m) => m ()
leadLevelFlip = do
  Kind.COps{cotile} <- getsState scops
  let canFlip fact = playerAiLeader (gplayer fact)
                     || isSpawnFact fact
      flipFaction fact | not $ canFlip fact = return ()
      flipFaction fact = do
        case gleader fact of
          Nothing -> return ()
          Just leader -> do
            body <- getsState $ getActorBody leader
            lvl2 <- getLevel $ blid body
            let leaderStuck = waitedLastTurn body
                t = lvl2 `at` bpos body
            -- Keep the leader: he is on stairs and not stuck
            -- and we don't want to clog stairs or get pushed to another level.
            unless (not leaderStuck && Tile.isStair cotile t) $ do
              actorD <- getsState sactorD
              let ourLvl (lid, lvl) =
                    ( lid
                    , EM.size (lfloor lvl)
                    , actorNotProjAssocsLvl (== bfid body) lvl actorD )
              ours <- getsState $ map ourLvl . EM.assocs . sdungeon
              -- Non-humans, being born in the dungeon, have a rough idea of
              -- the number of items left on the level and will focus
              -- on levels they started exploring and that have few items
              -- left. This is to to explore them completely, leave them
              -- once and for all and concentrate forces on another level.
              -- In addition, sole stranded actors tend to become leaders
              -- so that they can join the main force ASAP.
              let freqList = [ (k, (lid, a))
                             | (lid, itemN, (a, b) : rest) <- ours
                             , bhp b > 0  -- drama levels skipped
                             , not leaderStuck || lid /= blid body
                             , let len = 1 + (min 10 $ length rest)
                                   k = 1000000 `div` (3 * itemN + len) ]
              unless (null freqList) $ do
                (lid, a) <- rndToAction $ frequency
                                        $ toFreq "leadLevel" freqList
                unless (lid == blid body) $
                  execCmdAtomic
                  $ LeadFactionA (bfid body) (Just leader) (Just a)
  factionD <- getsState sfactionD
  mapM_ flipFaction $ EM.elems factionD

-- | Continue or exit or restart the game.
endOrLoop :: (MonadAtomic m, MonadConnServer m) => m () -> m () -> m ()
endOrLoop updConn loopServer = do
  factionD <- getsState sfactionD
  let inGame fact = case gquit fact of
        Nothing -> True
        Just Status{stOutcome=Camping} -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  let getQuitter fact = case gquit fact of
        Just Status{stOutcome=Restart, stInfo} -> Just stInfo
        _ -> Nothing
      quitters = mapMaybe getQuitter $ EM.elems factionD
  let isCamper fact = case gquit fact of
        Just Status{stOutcome=Camping} -> True
        _ -> False
      campers = filter (isCamper . snd) $ EM.assocs factionD
  case (quitters, campers) of
    (sgameMode : _, _) -> do
      modifyServer $ \ser -> ser {sdebugNxt = (sdebugNxt ser) {sgameMode}}
      restartGame updConn loopServer
    _ | gameOver -> restartGame updConn loopServer
    ([], []) -> loopServer  -- continue current game
    ([], _ : _) -> do
      -- Wipe out the quit flag for the savegame files.
      mapM_ (\(fid, fact) ->
              execCmdAtomic
              $ QuitFactionA fid Nothing (gquit fact) Nothing) campers
      saveAndExit
      -- Don't call @loopServer@, that is, quit the game loop.
      -- debugPrint "Server loop finished"

saveAndExit :: (MonadAtomic m, MonadConnServer m) => m ()
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

restartGame :: (MonadAtomic m, MonadConnServer m)
            => m () -> m () -> m ()
restartGame updConn loopServer = do
  tellGameClipPS
  cops <- getsState scops
  sdebugNxt <- getsServer sdebugNxt
  srandom <- getsServer srandom
  s <- gameReset cops sdebugNxt $ Just srandom
  let debugBarRngs = sdebugNxt {sdungeonRng = Nothing, smainRng = Nothing}
  modifyServer $ \ser -> ser { sdebugNxt = debugBarRngs
                             , sdebugSer = debugBarRngs }
  execCmdAtomic $ RestartServerA s
  updConn
  initPer
  reinitGame
  loopServer
