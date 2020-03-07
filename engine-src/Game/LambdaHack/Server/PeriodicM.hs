-- | Server operations performed periodically in the game loop
-- and related operations.
module Game.LambdaHack.Server.PeriodicM
  ( spawnMonster, addAnyActor
  , advanceTime, advanceTimeTraj, overheadActorTime, swapTime
  , updateCalm, leadLevelSwitch
  , endOrLoop
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , rollSpawnPos, gameExit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import           Data.Ord

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.CommonM
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ProtocolM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

-- | Spawn, possibly, a monster according to the level's actor groups.
-- We assume heroes are never spawned.
spawnMonster :: MonadServerAtomic m => m ()
spawnMonster = do
  COps{cocave} <- getsState scops
  arenas <- getsServer sarenas
  -- Do this on only one of the arenas to prevent micromanagement,
  -- e.g., spreading leaders across levels to bump monster generation.
  arena <- rndToAction $ oneOf arenas
  Level{lkind, ldepth, lbig} <- getLevel arena
  let ck = okind cocave lkind
  if | CK.cactorCoeff ck == 0 || null (CK.cactorFreq ck) -> return ()
     | EM.size lbig >= 300 ->  -- probably not so rare, but debug anyway
       -- Gameplay consideration: not fun to slog through so many actors.
       -- Caves rarely start with more than 100.
       debugPossiblyPrint "Server: spawnMonster: too many big actors on level"
     | otherwise -> do
       totalDepth <- getsState stotalDepth
       lvlSpawned <- getsServer $ fromMaybe 0 . EM.lookup arena . snumSpawned
       rc <- rndToAction
             $ monsterGenChance ldepth totalDepth lvlSpawned (CK.cactorCoeff ck)
       when rc $ do
         modifyServer $ \ser ->
           ser {snumSpawned = EM.insert arena (lvlSpawned + 1)
                              $ snumSpawned ser}
         localTime <- getsState $ getLocalTime arena
         maid <- addAnyActor False lvlSpawned (CK.cactorFreq ck) arena
                             localTime Nothing
         case maid of
           Nothing -> return ()  -- suspect content; server debug elsewhere
           Just aid -> do
             b <- getsState $ getActorBody aid
             mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
             when (isNothing mleader) $ setFreshLeader (bfid b) aid

addAnyActor :: MonadServerAtomic m
            => Bool -> Int -> Freqs ItemKind -> LevelId -> Time -> Maybe Point
            -> m (Maybe ActorId)
addAnyActor summoned lvlSpawned actorFreq lid time mpos = do
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- that contains the fixed properties of all actors of that kind.
  cops <- getsState scops
  lvl <- getLevel lid
  factionD <- getsState sfactionD
  freq <- prepareItemKind lvlSpawned lid actorFreq
  m2 <- rollItemAspect freq lid
  case m2 of
    Nothing -> do
      debugPossiblyPrint "Server: addAnyActor: trunk failed to roll"
      return Nothing
    Just (itemKnownRaw, (itemFullRaw, kit)) -> do
      (fid, _) <- rndToAction $ oneOf $
                    possibleActorFactions (itemKind itemFullRaw) factionD
      pers <- getsServer sperFid
      let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                    $ EM.elems $ EM.delete fid pers  -- expensive :(
          -- Checking skill would be more accurate, but skills can be
          -- inside organs, equipment, condition organs, created organs, etc.
          freqNames = map fst $ IK.ifreq $ itemKind itemFullRaw
          mobile = IK.MOBILE `elem` freqNames
          aquatic = IK.AQUATIC `elem` freqNames
      mrolledPos <- case mpos of
        Just{} -> return mpos
        Nothing -> do
          rollPos <-
            getsState $ rollSpawnPos cops allPers mobile aquatic lid lvl fid
          rndToAction rollPos
      case mrolledPos of
        Just pos ->
          Just <$> registerActor summoned itemKnownRaw (itemFullRaw, kit)
                                 fid pos lid time
        Nothing -> do
          debugPossiblyPrint
            "Server: addAnyActor: failed to find any free position"
          return Nothing

rollSpawnPos :: COps -> ES.EnumSet Point
             -> Bool -> Bool -> LevelId -> Level -> FactionId -> State
             -> Rnd (Maybe Point)
rollSpawnPos COps{coTileSpeedup} visible
             mobile aquatic lid lvl@Level{larea} fid s = do
  let inhabitants = foeRegularList fid lid s
      nearInh !df !p = all (\ !b -> df $ chessDist (bpos b) p) inhabitants
      distantMiddle !d !p = chessDist p (middlePoint larea) < d
      condList | mobile =
        [ nearInh (<= 50)  -- don't spawn very far from foes
        , nearInh (<= 100)
        ]
               | otherwise =
        [ distantMiddle 8
        , distantMiddle 16
        , distantMiddle 24
        , distantMiddle 26
        , distantMiddle 28
        , distantMiddle 30
        ]
  -- Not considering TK.OftenActor, because monsters emerge from hidden ducts,
  -- which are easier to hide in crampy corridors that lit halls.
  findPosTry2 (if mobile then 500 else 50) lvl
    ( \p !t -> Tile.isWalkable coTileSpeedup t
               && not (Tile.isNoActor coTileSpeedup t)
               && not (occupiedBigLvl p lvl)
               && not (occupiedProjLvl p lvl) )
    (map (\f p _ -> f p) condList)
    (\ !p t -> nearInh (> 4) p  -- otherwise actors in dark rooms swarmed
               && not (p `ES.member` visible)  -- visibility and plausibility
               && (not aquatic || Tile.isAquatic coTileSpeedup t))
    [ \ !p _ -> nearInh (> 3) p
                && not (p `ES.member` visible)
    , \ !p _ -> nearInh (> 2) p  -- otherwise actors hit on entering level
                && not (p `ES.member` visible)
    , \ !p _ -> not (p `ES.member` visible)
    ]

-- | Advance the move time for the given actor.
advanceTime :: MonadServerAtomic m => ActorId -> Int -> Bool -> m ()
advanceTime aid percent breakStasis = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let t = timeDeltaPercent (ticksPerMeter $ gearSpeed actorMaxSk) percent
  -- @t@ may be negative; that's OK.
  modifyServer $ \ser ->
    ser {sactorTime = ageActor (bfid b) (blid b) aid t $ sactorTime ser}
  when breakStasis $
    modifyServer $ \ser ->
      ser {sactorStasis = ES.delete aid (sactorStasis ser)}
             -- actor moved, so he broke the time stasis, he can be
             -- paralyzed as well as propelled again

-- | Advance the trajectory following time for the given actor.
advanceTimeTraj :: MonadServerAtomic m => ActorId -> m ()
advanceTimeTraj aid = do
  b <- getsState $ getActorBody aid
  let speedTraj = case btrajectory b of
        Nothing -> error $ "" `showFailure` b
        Just (_, speed) -> speed
      t = ticksPerMeter speedTraj
  -- @t@ may be negative; that's OK.
  modifyServer $ \ser ->
    ser {strajTime = ageActor (bfid b) (blid b) aid t $ strajTime ser}

-- | Add communication overhead time delta to all non-projectile, non-dying
-- faction's actors, except the leader. Effectively, this limits moves
-- of a faction on a level to 10, regardless of the number of actors
-- and their speeds. To avoid animals suddenly acting extremely sluggish
-- whenever monster's leader visits a distant arena that has a crowd
-- of animals, overhead applies only to actors on the same level.
-- Since the number of active levels is limited, this bounds the total moves
-- per turn of each faction as well.
--
-- Leader is immune from overhead and so he is faster than other faction
-- members and of equal speed to leaders of other factions (of equal
-- base speed) regardless how numerous the faction is.
-- Thanks to this, there is no problem with leader of a numerous faction
-- having very long UI turns, introducing UI lag.
overheadActorTime :: MonadServerAtomic m => FactionId -> LevelId -> m ()
overheadActorTime fid lid = do
  -- Only non-projectiles processed, because @strajTime@ ignored.
  actorTimeFid <- getsServer $ (EM.! fid) . sactorTime
  let actorTimeLid = actorTimeFid EM.! lid
  getActorB <- getsState $ flip getActorBody
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  let f !aid !time =
        let body = getActorB aid
        in if bhp body > 0  -- speed up all-move-at-once carcass removal
              && Just aid /= mleader  -- leader fast, for UI to be fast
           then timeShift time (Delta timeClip)
           else time
      actorTimeLid2 = EM.mapWithKey f actorTimeLid
      actorTimeFid2 = EM.insert lid actorTimeLid2 actorTimeFid
  modifyServer $ \ser ->
    ser {sactorTime = EM.insert fid actorTimeFid2 $ sactorTime ser}

-- | Swap the relative move times of two actors (e.g., when switching
-- a UI leader). Notice that their trajectory move times are not swapped.
swapTime :: MonadServerAtomic m => ActorId -> ActorId -> m ()
swapTime source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  slvl <- getsState $ getLocalTime (blid sb)
  tlvl <- getsState $ getLocalTime (blid tb)
  btime_sb <-
    getsServer $ (EM.! source) . (EM.! blid sb) . (EM.! bfid sb) . sactorTime
  btime_tb <-
    getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
  let lvlDelta = slvl `timeDeltaToFrom` tlvl
      bDelta = btime_sb `timeDeltaToFrom` btime_tb
      sdelta = timeDeltaSubtract lvlDelta bDelta
      tdelta = timeDeltaReverse sdelta
  -- Equivalent, for the assert:
  let !_A = let sbodyDelta = btime_sb `timeDeltaToFrom` slvl
                tbodyDelta = btime_tb `timeDeltaToFrom` tlvl
                sgoal = slvl `timeShift` tbodyDelta
                tgoal = tlvl `timeShift` sbodyDelta
                sdelta' = sgoal `timeDeltaToFrom` btime_sb
                tdelta' = tgoal `timeDeltaToFrom` btime_tb
            in assert (sdelta == sdelta' && tdelta == tdelta'
                       `blame` ( slvl, tlvl, btime_sb, btime_tb
                               , sdelta, sdelta', tdelta, tdelta' )) ()
  when (sdelta /= Delta timeZero) $ modifyServer $ \ser ->
    ser {sactorTime = ageActor (bfid sb) (blid sb) source sdelta $ sactorTime ser}
  when (tdelta /= Delta timeZero) $ modifyServer $ \ser ->
    ser {sactorTime = ageActor (bfid tb) (blid tb) target tdelta $ sactorTime ser}

updateCalm :: MonadServerAtomic m => ActorId -> Int64 -> m ()
updateCalm target deltaCalm = do
  tb <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  let calmMax64 = xM $ Ability.getSk Ability.SkMaxCalm actorMaxSk
  execUpdAtomic $ UpdRefillCalm target deltaCalm
  when (bcalm tb < calmMax64
        && bcalm tb + deltaCalm >= calmMax64) $
    return ()
    -- We don't dominate the actor here, because if so, players would
    -- disengage after one of their actors is dominated and wait for him
    -- to regenerate Calm. This is unnatural and boring. Better fight
    -- and hope he gets his Calm again to 0 and then defects back.
    -- We could instead tell here that Calm is fully regenerated,
    -- but that would be too verbose.

leadLevelSwitch :: MonadServerAtomic m => m ()
leadLevelSwitch = do
  COps{cocave} <- getsState scops
  factionD <- getsState sfactionD
  let canSwitch fact = fst (autoDungeonLevel fact)
                       -- a hack to help AI, until AI client can switch levels
                       || case fleaderMode (gplayer fact) of
                            LeaderNull -> False
                            LeaderAI _ -> True
                            LeaderUI _ -> False
      flipFaction (_, fact) | not $ canSwitch fact = return ()
      flipFaction (fid, fact) =
        case gleader fact of
          Nothing -> return ()
          Just leader -> do
            body <- getsState $ getActorBody leader
            let !_A = assert (fid == bfid body) ()
            s <- getsServer $ (EM.! fid) . sclientStates
            let leaderStuck = actorWaits body
                oursRaw =
                  [ ((lid, lvl), (allSeen, as))
                  | (lid, lvl) <- EM.assocs $ sdungeon s
                  , lid /= blid body || not leaderStuck
                  , let asRaw = -- Drama levels ignored, hence @Regular@.
                                fidActorRegularAssocs fid lid s
                        isAlert (_, b) = case bwatch b of
                          WWatch -> True
                          WWait n -> n == 0
                          WSleep -> False
                          WWake -> True  -- probably in danger
                        (alert, relaxed) = partition isAlert asRaw
                        as = alert ++ relaxed  -- best switch leader to alert
                  , not (null as)
                  , let allSeen =
                          lexpl lvl <= lseen lvl
                          || CK.cactorCoeff (okind cocave $ lkind lvl) > 150
                             && not (fhasGender $ gplayer fact)
                  ]
                (oursSeen, oursNotSeen) = partition (fst . snd) oursRaw
                -- Monster AI changes leadership mostly to move from level
                -- to level and, in particular, to quickly bring troops
                -- to the frontline level and so prevent human from killing
                -- monsters at numerical advantage.
                -- However, an AI boss that can't move between levels
                -- disrupts this by hogging leadership. To prevent that,
                -- assuming the boss resides below the frontline level,
                -- only the two shallowest levels that are not yet fully
                -- explored are considered to choose the new leader from.
                -- This frontier moves as the levels are explored or emptied
                -- and sometimes the level with the boss is counted among
                -- them, but it never happens in the crucial periods when
                -- AI armies are transferred from level to level.
                f ((lid, _), _) = abs $ fromEnum lid
                ours = oursSeen ++ take 2 (sortBy (comparing f) oursNotSeen)
            -- Actors on desolate levels (not many own or enemy non-projectiles)
            -- tend to become (or stay) leaders so that they can join the main
            -- force where it matters ASAP. Unfortunately, this keeps hero
            -- scouts as leader, but foes spawn very fast early on ,
            -- so they give back leadership rather quickly to let others follow.
            -- We count non-mobile and sleeping actors, because they may
            -- be dangerous, especially if adjacent to stairs.
            let freqList = [ (k, (lid, aid))
                           | ((lid, lvl), (_, (aid, _) : _)) <- ours
                           , let len = min 20 (EM.size $ lbig lvl)
                                 k = 1000000 `div` (1 + len) ]
                closeToFactStash (fid2, fact2) = case gstash fact2 of
                  Just (lid, pos) ->
                    isFoe fid (factionD EM.! fid) fid2
                    && lid == blid body
                    && chessDist pos (bpos body) <= 1  -- visible
                  Nothing -> False
                closeToEnemyStash = any closeToFactStash $ EM.assocs factionD
            unless (closeToEnemyStash || null freqList) $ do
              (lid, a) <- rndToAction $ frequency
                                      $ toFreq "leadLevel" freqList
              unless (lid == blid body) $  -- flip levels rather than actors
                setFreshLeader fid a
  mapM_ flipFaction $ EM.assocs factionD

-- | Continue or exit or restart the game.
endOrLoop :: (MonadServerAtomic m, MonadServerComm m)
          => m () -> (Maybe (GroupName ModeKind) -> m ())
          -> m ()
endOrLoop loop restart = do
  factionD <- getsState sfactionD
  let inGame fact = case gquit fact of
        Nothing -> True
        Just Status{stOutcome=Camping} -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  let getQuitter fact = case gquit fact of
        Just Status{stOutcome=Restart, stNewGame} -> stNewGame
        _ -> Nothing
      quitters = mapMaybe getQuitter $ EM.elems factionD
      restartNeeded = gameOver || not (null quitters)
  let isCamper fact = case gquit fact of
        Just Status{stOutcome=Camping} -> True
        _ -> False
      campers = filter (isCamper . snd) $ EM.assocs factionD
  -- Wipe out the quit flag for the savegame files.
  mapM_ (\(fid, fact) ->
    execUpdAtomic $ UpdQuitFaction fid (gquit fact) Nothing Nothing) campers
  swriteSave <- getsServer swriteSave
  sstopAfterGameOver <-
    getsServer $ sstopAfterGameOver . soptions
  when swriteSave $ do
    modifyServer $ \ser -> ser {swriteSave = False}
    writeSaveAll True
  if | gameOver && sstopAfterGameOver -> gameExit
     | restartNeeded -> do
       execSfxAtomic SfxRestart
       restart (listToMaybe quitters)
     | not $ null campers -> gameExit  -- and @loop@ is not called
     | otherwise -> loop  -- continue current game

gameExit :: (MonadServerAtomic m, MonadServerComm m) => m ()
gameExit = do
--  debugPossiblyPrint "Server: Verifying all perceptions."
  -- Verify that the possibly not saved caches are equal to future
  -- reconstructed. Otherwise, save/restore would change game state.
  -- This is done even in released binaries, because it only prolongs
  -- game shutdown a bit. The same checks at each periodic game save
  -- would icrease the game saving lag, so they are normally avoided.
  verifyCaches
  -- Kill all clients, including those that did not take part
  -- in the current game.
  -- Clients exit not now, but after they print all ending screens.
--  debugPossiblyPrint "Server: Killing all clients."
  killAllClients
--  debugPossiblyPrint "Server: All clients killed."
  return ()
