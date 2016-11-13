-- | Server operations performed periodically in the game loop
-- and related operations.
module Game.LambdaHack.Server.PeriodicM
  ( spawnMonster, addAnyActor, dominateFidSfx
  , advanceTime, overheadActorTime, swapTime, managePerTurn
  , leadLevelSwitch, udpateCalm
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Int (Int64)

import Game.LambdaHack.Atomic
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- TODO: civilians would have 'it' pronoun
-- | Sapwn, possibly, a monster according to the level's actor groups.
-- We assume heroes are never spawned.
spawnMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
{-# INLINE spawnMonster #-}
spawnMonster lid = do
  totalDepth <- getsState stotalDepth
  -- TODO: eliminate the defeated and victorious faction from lactorFreq;
  -- then fcanEscape and fneverEmpty make sense for spawning factions
  Level{ldepth, lactorCoeff, lactorFreq} <- getLevel lid
  lvlSpawned <- getsServer $ fromMaybe 0 . EM.lookup lid . snumSpawned
  rc <- rndToAction
        $ monsterGenChance ldepth totalDepth lvlSpawned lactorCoeff
  when rc $ do
    modifyServer $ \ser ->
      ser {snumSpawned = EM.insert lid (lvlSpawned + 1) $ snumSpawned ser}
    localTime <- getsState $ getLocalTime lid
    maid <- addAnyActor lactorFreq lid localTime Nothing
    case maid of
      Nothing -> return ()
      Just aid -> do
        b <- getsState $ getActorBody aid
        mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
        when (isNothing mleader) $ supplantLeader (bfid b) aid

addAnyActor :: (MonadAtomic m, MonadServer m)
            => Freqs ItemKind -> LevelId -> Time -> Maybe Point
            -> m (Maybe ActorId)
addAnyActor actorFreq lid time mpos = do
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- contains the constant properties.
  cops <- getsState scops
  lvl <- getLevel lid
  factionD <- getsState sfactionD
  lvlSpawned <- getsServer $ fromMaybe 0 . EM.lookup lid . snumSpawned
  m4 <- rollItem lvlSpawned lid actorFreq
  case m4 of
    Nothing -> return Nothing
    Just (itemKnown, trunkFull, itemDisco, seed, _) -> do
      let ik = itemKind itemDisco
          freqNames = map fst $ IK.ifreq ik
          f fact = fgroup (gplayer fact)
          factNames = map f $ EM.elems factionD
          fidName = case freqNames `intersect` factNames of
            [] -> head factNames  -- fall back to an arbitrary faction
            fName : _ -> fName
          g (_, fact) = fgroup (gplayer fact) == fidName
          mfid = find g $ EM.assocs factionD
          fid = fst $ fromMaybe (assert `failure` (factionD, fidName)) mfid
      pers <- getsServer sperFid
      let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                    $ EM.elems $ EM.delete fid pers  -- expensive :(
          mobile = any (`elem` freqNames) ["mobile", "horror"]
      pos <- case mpos of
        Just pos -> return pos
        Nothing -> do
          fact <- getsState $ (EM.! fid) . sfactionD
          rollPos <- getsState $ rollSpawnPos cops allPers mobile lid lvl fact
          rndToAction rollPos
      let container = CTrunk fid lid pos
      trunkId <- registerItem trunkFull itemKnown seed container False
      addActorIid trunkId trunkFull False fid pos lid id "it" time

rollSpawnPos :: Kind.COps -> ES.EnumSet Point
             -> Bool -> LevelId -> Level -> Faction -> State
             -> Rnd Point
rollSpawnPos Kind.COps{coTileSpeedup} visible
             mobile lid lvl@Level{ltile, lxsize, lysize} fact s = do
  let inhabitants = actorRegularList (isAtWar fact) lid s
      distantSo df p _ = all (\b -> df $ chessDist (bpos b) p) inhabitants
      middlePos = Point (lxsize `div` 2) (lysize `div` 2)
      distantMiddle d p _ = chessDist p middlePos < d
      condList | mobile =
        [ distantSo (<= 10)  -- try hard to harass enemies
        , distantSo (<= 15)
        , distantSo (<= 20)
        ]
               | otherwise =
        [ distantMiddle 5
        , distantMiddle 10
        , distantMiddle 20
        , distantMiddle 50
        , distantMiddle 100
        ]
  -- Not considering TK.OftenActor, because monsters emerge from hidden ducts,
  -- which are easier to hide in crampy corridors that lit halls.
  findPosTry2 (if mobile then 500 else 100) ltile
    ( \p t -> Tile.isWalkable coTileSpeedup t
              && not (Tile.isNoActor coTileSpeedup t)
              && null (posToAidsLvl p lvl))
    condList
    (\p t -> distantSo (> 4) p t  -- otherwise actors in dark rooms swarmed
             && not (p `ES.member` visible))  -- visibility and plausibility
    [ \p t -> distantSo (> 3) p t
              && not (p `ES.member` visible)
    , \p t -> distantSo (> 2) p t -- otherwise actors hit on entering level
              && not (p `ES.member` visible)
    , \p _ -> not (p `ES.member` visible)
    ]

dominateFidSfx :: (MonadAtomic m, MonadServer m)
               => FactionId -> ActorId -> m Bool
dominateFidSfx fid target = do
  tb <- getsState $ getActorBody target
  -- Actors that don't move freely can't be dominated, for otherwise,
  -- when they are the last survivors, they could get stuck
  -- and the game wouldn't end.
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      actorMaxSk = aSkills ar
      -- Check that the actor can move, also between levels and through doors.
      -- Otherwise, it's too awkward for human player to control.
      canMove = EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
                && EM.findWithDefault 0 Ability.AbTrigger actorMaxSk > 0
                && EM.findWithDefault 0 Ability.AbAlter actorMaxSk > 0
  if canMove && not (bproj tb)
    then do
      let execSfx = execSfxAtomic
                    $ SfxEffect (bfidImpressed tb) target IK.Dominate 0
      execSfx
      dominateFid fid target
      execSfx
      return True
    else
      return False

dominateFid :: (MonadAtomic m, MonadServer m)
            => FactionId -> ActorId -> m ()
dominateFid fid target = do
  Kind.COps{cotile} <- getsState scops
  tb0 <- getsState $ getActorBody target
  electLeader (bfid tb0) (blid tb0) target
  fact <- getsState $ (EM.! bfid tb0) . sfactionD
  -- Prevent the faction's stash from being lost in case they are not spawners.
  when (isNothing $ gleader fact) $ moveStores target CSha CInv
  tb <- getsState $ getActorBody target
  deduceKilled target tb
  -- TODO: some messages after game over below? Compare with dieSer.
  ais <- getsState $ getCarriedAssocs tb
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
  btime <-
    getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
  execUpdAtomic $ UpdLoseActor target tb ais
  let bNew = tb { bfid = fid
                , bfidImpressed = bfid tb
                , bcalm = max 0 $ xM (aMaxCalm ar) `div` 2 }
  execUpdAtomic $ UpdSpotActor target bNew ais
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime fid (blid tb) target btime
                      $ sactorTime ser}
  let discoverSeed (iid, cstore) = do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        item <- getsState $ getItemBody iid
        Level{ldepth} <- getLevel $ jlid item
        let c = CActor target cstore
        execUpdAtomic $ UpdDiscoverSeed c iid seed ldepth
      aic = getCarriedIidCStore tb
  mapM_ discoverSeed aic
  mleaderOld <- getsState $ gleader . (EM.! fid) . sfactionD
  -- Keep the leader if he is on stairs. We don't want to clog stairs.
  keepLeader <- case mleaderOld of
    Nothing -> return False
    Just leaderOld -> do
      body <- getsState $ getActorBody leaderOld
      lvl <- getLevel $ blid body
      return $! Tile.isStair cotile $ lvl `at` bpos body
  unless keepLeader $
    -- Focus on the dominated actor, by making him a leader.
    supplantLeader fid target

-- | Advance the move time for the given actor
advanceTime :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
advanceTime !aid = do
  b <- getsState $ getActorBody aid
  btime_b <- getsServer $ (EM.! aid) . (EM.! blid b) . (EM.! bfid b) . sactorTime
  localTime <- getsState $ getLocalTime (blid b)
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! aid
      actorTurn = ticksPerMeter $ bspeed b ar
      halfStandardTurn = timeDeltaDiv (Delta timeTurn) 2
      -- Dead bodies stay around for only a half of standard turn,
      -- even if paralyzed (that is, wrt local time).
      -- Projectiles that hit actors or are hit by actors vanish at once
      -- not to block actor's path, e.g., for Pull effect.
      t | bhp b > 0 = actorTurn
        | otherwise =
        let delta = if bproj b then Delta timeZero else halfStandardTurn
            localPlusDelta = localTime `timeShift` delta
        in localPlusDelta `timeDeltaToFrom` btime_b
  -- @t@ may be negative; that's OK.
  modifyServer $ \ser ->
    ser {sactorTime = ageActor (bfid b) (blid b) aid t $ sactorTime ser}

overheadActorTime :: (MonadAtomic m, MonadServer m)
                  => ES.EnumSet LevelId -> FactionId -> ActorId -> m ()
{-# INLINE overheadActorTime #-}
overheadActorTime !arenas !fid !aid = do
  -- Add communication overhead time delta to all non-projectile,
  -- non-dying faction's actors. Effectively, this limits moves of
  -- a faction to 10, regardless of the number of actors
  -- and their speeds.
  -- To discourage distributing actors among active arenas, overhead
  -- applies to all actors on active arenas.
  -- TODO: this and handleActors can be sped up by keeping it per lid
  -- and per fid, in a map from times to list of actors, separately
  -- map for projectiles and dying and a map for others. Complex.
  actorTime <- getsServer $ (EM.! fid) . sactorTime
  s <- getState
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  let f !aid2 !time =
        let body = getActorBody aid2 s
        in if isNothing (btrajectory body)
              && bhp body > 0
              -- If the leader moves, he gets overhead; but not from others.
              && (Just aid2 /= mleader || aid2 == aid)
           then timeShift time (Delta timeClip)
           else time
      g lid levelTime = if lid `ES.member` arenas
                        then EM.mapWithKey f levelTime
                        else levelTime
      actorTimeNew = EM.mapWithKey g actorTime
  modifyServer $ \ser ->
    ser {sactorTime = EM.insert fid actorTimeNew $ sactorTime ser}

-- | Swap the relative move times of two actors (e.g., when switching
-- a UI leader).
swapTime :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
swapTime source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  slvl <- getsState $ getLocalTime (blid sb)
  tlvl <- getsState $ getLocalTime (blid tb)
  btime_sb <- getsServer $ (EM.! source) . (EM.! blid sb) . (EM.! bfid sb) . sactorTime
  btime_tb <- getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
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

-- | Check if the given actor is dominated and update his calm.
-- We don't update calm once per game turn (even though
-- it would make fast actors less overpowered),
-- beucase the effects of close enemies would sometimes manifest only after
-- a couple of player turns (or perhaps never at all, if the player and enemy
-- move away before that moment). A side effect is that under peaceful
-- circumstances, non-max calm causes a consistent Calm regeneration
-- UI indicator to be displayed each turn (not every few turns).
managePerTurn :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
managePerTurn aid = do
  b <- getsState $ getActorBody aid
  unless (bproj b) $ do
    fact <- getsState $ (EM.! bfid b) . sfactionD
    dominated <-
      -- We react one turn after bcalm reaches 0, to let it be
      -- displayed first, to let the player panic in advance
      -- and also to avoid the dramatic domination message
      -- be swamped in other enemy turn messages.
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
      let clearMark = 0
      unless (newCalmDelta == 0) $
        -- Update delta for the current player turn.
        udpateCalm aid newCalmDelta
      unless (bcalmDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillCalm aid clearMark
      unless (bhpDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillHP aid clearMark

udpateCalm :: (MonadAtomic m, MonadServer m) => ActorId -> Int64 -> m ()
udpateCalm target deltaCalm = do
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      calmMax64 = xM $ aMaxCalm ar
  execUpdAtomic $ UpdRefillCalm target deltaCalm
  when (bcalm tb < calmMax64
        && bcalm tb + deltaCalm >= calmMax64
        && bfidImpressed tb /= bfidOriginal tb) $
    execUpdAtomic $
      UpdFidImpressedActor target (bfidImpressed tb) (bfidOriginal tb)

leadLevelSwitch :: (MonadAtomic m, MonadServer m) => m ()
leadLevelSwitch = do
  Kind.COps{cotile} <- getsState scops
  let canSwitch fact = fst (autoDungeonLevel fact)
                       -- a hack to help AI, until AI client can switch levels
                       || case fleaderMode (gplayer fact) of
                            LeaderNull -> False
                            LeaderAI _ -> True
                            LeaderUI _ -> False
      flipFaction fact | not $ canSwitch fact = return ()
      flipFaction fact =
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
              s <- getState
              let ourLvl (lid, lvl) =
                    ( lid
                    , EM.size (lfloor lvl)
                    , -- Drama levels skipped, hence @Regular@.
                      actorRegularIds (== bfid body) lid s )
              ours <- getsState $ map ourLvl . EM.assocs . sdungeon
              -- Non-humans, being born in the dungeon, have a rough idea of
              -- the number of items left on the level and will focus
              -- on levels they started exploring and that have few items
              -- left. This is to to explore them completely, leave them
              -- once and for all and concentrate forces on another level.
              -- In addition, sole stranded actors tend to become leaders
              -- so that they can join the main force ASAP.
              let freqList = [ (k, (lid, a))
                             | (lid, itemN, a : rest) <- ours
                             , not leaderStuck || lid /= blid body
                             , let len = 1 + min 10 (length rest)
                                   k = 1000000 `div` (3 * itemN + len) ]
              unless (null freqList) $ do
                (lid, a) <- rndToAction $ frequency
                                        $ toFreq "leadLevel" freqList
                unless (lid == blid body) $  -- flip levels rather than actors
                  supplantLeader (bfid body) a
  factionD <- getsState sfactionD
  mapM_ flipFaction $ EM.elems factionD
