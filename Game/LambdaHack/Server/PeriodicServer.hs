-- | Server operations performed periodically in the game loop
-- and related operations.
module Game.LambdaHack.Server.PeriodicServer
  ( spawnMonster, addAnyActor, dominateFidSfx
  , advanceTime, managePerTurn, leadLevelSwitch
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe

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
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- TODO: civilians would have 'it' pronoun
-- | Sapwn, possibly, a monster according to the level's actor groups.
-- We assume heroes are never spawned.
spawnMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
spawnMonster lid = do
  -- We check the number of current dungeon dwellers (whether spawned or not)
  -- to decide if more should be spawned.
  f <- getsState $ \s fid -> not $ fcanEscape $ gplayer $ sfactionD s EM.! fid
  spawns <- getsState $ actorRegularList f lid
  totalDepth <- getsState stotalDepth
  -- TODO: eliminate the defeated and victorious faction from lactorFreq;
  -- then fcanEscape and fneverEmpty make sense for spawning factions
  Level{ldepth, lactorCoeff, lactorFreq} <- getLevel lid
  lvlSpawned <- getsServer $ fromMaybe 0 . EM.lookup lid . snumSpawned
  -- Heroes have to endure two lvl-sized waves of spawners upon entering lvl.
  let numSpawnedCoeff = length spawns + lvlSpawned `div` 2
  rc <- rndToAction
        $ monsterGenChance ldepth totalDepth numSpawnedCoeff lactorCoeff
  when rc $ do
    modifyServer $ \ser ->
      ser {snumSpawned = EM.insert lid (lvlSpawned + 1) $ snumSpawned ser}
    time <- getsState $ getLocalTime lid
    maid <- addAnyActor lactorFreq lid time Nothing
    case maid of
      Nothing -> return ()
      Just aid -> do
        b <- getsState $ getActorBody aid
        mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
        when (isNothing mleader) $
          execUpdAtomic $ UpdLeadFaction (bfid b) Nothing (Just (aid, Nothing))

addAnyActor :: (MonadAtomic m, MonadServer m)
            => Freqs ItemKind -> LevelId -> Time -> Maybe Point
            -> m (Maybe ActorId)
addAnyActor actorFreq lid time mpos = do
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- contains the constant properties.
  cops <- getsState scops
  lvl <- getLevel lid
  factionD <- getsState sfactionD
  m4 <- rollItem lid actorFreq
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
      pers <- getsServer sper
      let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                    $ EM.elems $ EM.delete fid pers  -- expensive :(
          mobile = any (`elem` freqNames) ["civilian", "horror"]
      pos <- case mpos of
        Just pos -> return pos
        Nothing -> do
          fact <- getsState $ (EM.! fid) . sfactionD
          rollPos <- getsState $ rollSpawnPos cops allPers mobile lid lvl fact
          rndToAction rollPos
      let container = CTrunk fid lid pos
      trunkId <- registerItem trunkFull itemKnown seed
                              (itemK trunkFull) container False
      addActorIid trunkId trunkFull False fid pos lid id "it" time

rollSpawnPos :: Kind.COps -> ES.EnumSet Point
             -> Bool -> LevelId -> Level -> Faction -> State
             -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible
             mobile lid Level{ltile, lxsize, lysize} fact s = do
  let inhabitants = actorRegularList (isAtWar fact) lid s
      as = actorList (const True) lid s
      isLit = Tile.isLit cotile
      distantSo df p _ =
        all (\b -> df $ chessDist (bpos b) p) inhabitants
      middlePos = Point (lxsize `div` 2) (lysize `div` 2)
      distantMiddle d p _ = chessDist p middlePos < d
      condList | mobile =
        [ \_ t -> not (isLit t) -- no such tiles on some maps
        , distantSo (<= 10)  -- try to harass enemies
        , distantSo (>= 5)   -- but leave room for yourself for ranged combat
        , distantSo (<= 20)  -- but applying pressure is more important
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
  findPosTry (if mobile then 500 else 100) ltile
    ( \p t -> Tile.isWalkable cotile t
              && not (Tile.hasFeature cotile TK.NoActor t)
              && unoccupied as p)
    (condList
     ++ [ \p _ -> not (p `ES.member` visible)  -- surprise and believability
        , distantSo (>= 3)  -- otherwise fast actor approach and hit in one turn
        ])

dominateFidSfx :: (MonadAtomic m, MonadServer m)
               => FactionId -> ActorId -> m Bool
dominateFidSfx fid target = do
  tb <- getsState $ getActorBody target
  -- Actors that don't move freely can't be dominated, for otherwise,
  -- when they are the last survivors, they could get stuck
  -- and the game wouldn't end.
  actorSk <- maxActorSkillsServer target
  let canMove = EM.findWithDefault 0 Ability.AbMove actorSk > 0
                && EM.findWithDefault 0 Ability.AbTrigger actorSk > 0
                && EM.findWithDefault 0 Ability.AbAlter actorSk > 0
  if canMove && not (bproj tb)
    then do
      let execSfx = execSfxAtomic
                    $ SfxEffect (bfidImpressed tb) target IK.Dominate
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
  deduceKilled tb (Just (target, tb))
  -- TODO: some messages after game over below? Compare with dieSer.
  ais <- getsState $ getCarriedAssocs tb
  calmMax <- sumOrganEqpServer IK.EqpSlotAddMaxCalm target
  execUpdAtomic $ UpdLoseActor target tb ais
  let bNew = tb { bfid = fid
                , bfidImpressed = bfid tb
                , bcalm = max 0 $ xM calmMax `div` 2 }
  execUpdAtomic $ UpdSpotActor target bNew ais
  let discoverSeed (iid, cstore) = do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        let c = CActor target cstore
        execUpdAtomic $ UpdDiscoverSeed c iid seed
      aic = getCarriedIidCStore tb
  mapM_ discoverSeed aic
  mleaderOld <- getsState $ gleader . (EM.! fid) . sfactionD
  -- Keep the leader if he is on stairs. We don't want to clog stairs.
  keepLeader <- case mleaderOld of
    Nothing -> return False
    Just (leaderOld, _) -> do
      body <- getsState $ getActorBody leaderOld
      lvl <- getLevel $ blid body
      return $! Tile.isStair cotile $ lvl `at` bpos body
  unless keepLeader $
    -- Focus on the dominated actor, by making him a leader.
    execUpdAtomic $ UpdLeadFaction fid mleaderOld (Just (target, Nothing))

-- | Advance the move time for the given actor
advanceTime :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
advanceTime aid = do
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsServer aid
  let t = ticksPerMeter $ bspeed b activeItems
  execUpdAtomic $ UpdAgeActor aid t

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
    activeItems <- activeItemsServer aid
    fact <- getsState $ (EM.! bfid b) . sfactionD
    dominated <-
      if bcalm b == 0
         && bfidImpressed b /= bfid b
         && fleaderMode (gplayer fact) /= LeaderNull
              -- animals/robots never Calm-dominated
      then dominateFidSfx (bfidImpressed b) aid
      else return False
    unless dominated $ do
      newCalmDelta <- getsState $ regenCalmDelta b activeItems
      let clearMark = 0
      unless (newCalmDelta <= 0) $
        -- Update delta for the current player turn.
        execUpdAtomic $ UpdRefillCalm aid newCalmDelta
      unless (bcalmDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillCalm aid clearMark
      unless (bhpDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillHP aid clearMark

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
          Just (leader, _) -> do
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
                    , -- Drama levels skipped, hence @Regular@.
                      actorRegularAssocsLvl (== bfid body) lvl actorD )
              ours <- getsState $ map ourLvl . EM.assocs . sdungeon
              -- Non-humans, being born in the dungeon, have a rough idea of
              -- the number of items left on the level and will focus
              -- on levels they started exploring and that have few items
              -- left. This is to to explore them completely, leave them
              -- once and for all and concentrate forces on another level.
              -- In addition, sole stranded actors tend to become leaders
              -- so that they can join the main force ASAP.
              let freqList = [ (k, (lid, a))
                             | (lid, itemN, (a, _) : rest) <- ours
                             , not leaderStuck || lid /= blid body
                             , let len = 1 + min 10 (length rest)
                                   k = 1000000 `div` (3 * itemN + len) ]
              unless (null freqList) $ do
                (lid, a) <- rndToAction $ frequency
                                        $ toFreq "leadLevel" freqList
                unless (lid == blid body) $  -- flip levels rather than actors
                  execUpdAtomic
                  $ UpdLeadFaction (bfid body) (gleader fact)
                                               (Just (a, Nothing))
  factionD <- getsState sfactionD
  mapM_ flipFaction $ EM.elems factionD
