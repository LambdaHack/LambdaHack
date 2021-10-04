{-# LANGUAGE TupleSections #-}
-- | Server operations common to many modules.
module Game.LambdaHack.Server.CommonM
  ( revealAll, generalMoveItem, deduceQuits
  , writeSaveAll, verifyCaches, deduceKilled, electLeader, setFreshLeader
  , updatePer, projectFail, addActorFromGroup, registerActor
  , discoverIfMinorEffects, pickWeaponServer, currentSkillsServer, allGroupItems
  , addCondition, removeConditionSingle, addSleep, removeSleepSingle
  , addKillToAnalytics
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , revealItems, revealPerceptionLid, containerMoveItem, quitF, keepArenaFact
  , anyActorsAlive, updatePerFromNew, recomputeCachePer
  , projectBla, addProjectile, addNonProjectile, addActorIid
  , getCacheLucid, getCacheTotal
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import           Data.Ratio

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import qualified Game.LambdaHack.Definition.DefsInternal as DefsInternal
import           Game.LambdaHack.Server.Fov
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.ItemRev
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

revealItems :: MonadServerAtomic m => FactionId -> m ()
revealItems fid = do
  COps{coitem} <- getsState scops
  ServerOptions{sclientOptions} <- getsServer soptions
  discoAspect <- getsState sdiscoAspect
  let keptSecret kind ar = IA.isHumanTrinket kind
                           || IA.checkFlag Ability.MetaGame ar
      discover aid store iid _ = do
        itemKindId <- getsState $ getIidKindIdServer iid
        let arItem = discoAspect EM.! iid
            c = CActor aid store
            itemKind = okind coitem itemKindId
        unless (keptSecret itemKind arItem) $  -- a hack
          execUpdAtomic $ UpdDiscover c iid itemKindId arItem
      f (aid, b) =
        -- CStash is IDed for each actor of each faction, which is fine,
        -- even though it may introduce a slight lag at gameover.
        join $ getsState $ mapActorItems_ (discover aid) b
  -- Don't ID projectiles, their items are not really owned by the party.
  aids <- getsState $ fidActorNotProjGlobalAssocs fid
  mapM_ f aids
  dungeon <- getsState sdungeon
  let minLid = fst $ minimumBy (comparing (ldepth . snd))
                   $ EM.assocs dungeon
      discoverSample iid = do
        itemKindId <- getsState $ getIidKindIdServer iid
        let arItem = discoAspect EM.! iid
            cdummy = CTrunk fid minLid originPoint  -- only @fid@ matters here
            itemKind = okind coitem itemKindId
        execUpdAtomic $ if keptSecret itemKind arItem  -- a hack
                        then UpdSpotItem False iid quantSingle cdummy
                        else UpdDiscover cdummy iid itemKindId arItem
  generationAn <- getsServer sgenerationAn
  getKindId <- getsState $ flip getIidKindIdServer
  let kindsEqual iid iid2 = getKindId iid == getKindId iid2 && iid /= iid2
      nonDupSample em iid 0 = not $ any (kindsEqual iid) $ EM.keys em
      nonDupSample _ _ _ = True
      nonDupGen = EM.map (\em -> EM.filterWithKey (nonDupSample em) em)
                         generationAn
  -- Remove samples that are supplanted by real items.
  -- If there are mutliple UI factions, the second run will be vacuus,
  -- but it's important to do that before the first try to identify things
  -- to prevent spam from identifying samples that are not needed.
  modifyServer $ \ser -> ser {sgenerationAn = nonDupGen}
  when (sexposeActors sclientOptions) $
    -- Few, if any, need ID, but we can't rule out unusual content.
    mapM_ discoverSample $ EM.keys $ nonDupGen EM.! STrunk
  when (sexposeItems sclientOptions) $
    mapM_ discoverSample $ EM.keys $ nonDupGen EM.! SItem
  mapM_ discoverSample $ EM.keys $ nonDupGen EM.! SEmbed
  mapM_ discoverSample $ EM.keys $ nonDupGen EM.! SOrgan
  mapM_ discoverSample $ EM.keys $ nonDupGen EM.! SCondition
  mapM_ discoverSample $ EM.keys $ nonDupGen EM.! SBlast

revealAll :: MonadServerAtomic m => FactionId -> m ()
revealAll fid = do
  revealItems fid
  execUpdAtomic $ UpdMuteMessages fid True
  dungeon <- getsState sdungeon
  -- Perception needs to be sent explicitly, because normal management
  -- assumes an action must happen on a level to invalidate and regenerate
  -- perception on the level (and actors must survive!).
  -- Also, we'd rather hack here and in `verifyCaches` that complicate
  -- the already complex perception creation and caching code.
  mapM_ (revealPerceptionLid fid) $ EM.assocs dungeon
  execUpdAtomic $ UpdMuteMessages fid False

revealPerceptionLid :: MonadServerAtomic m
                    => FactionId -> (LevelId, Level) -> m ()
revealPerceptionLid fid (lid, lvl) = do
  let (x0, y0, x1, y1) = fromArea $ larea lvl
      fullSet = ES.fromDistinctAscList [ Point x y
                                       | y <- [y0 .. y1]
                                       , x <- [x0 .. x1] ]
      perNew = Perception
        { psight = PerVisible fullSet
        , psmell = PerSmelled ES.empty  -- don't obscure
        }
  updatePerFromNew fid lid perNew

-- | Generate the atomic updates that jointly perform a given item move.
generalMoveItem :: MonadStateRead m
                => Bool -> ItemId -> Int -> Container -> Container
                -> m [UpdAtomic]
generalMoveItem _ iid k (CActor aid1 cstore1) c2@(CActor aid2 cstore2)
  | aid1 == aid2 = do
    moveStash <- moveStashIfNeeded c2
    return $! moveStash ++ [UpdMoveItem iid k aid1 cstore1 cstore2]
generalMoveItem verbose iid k c1 c2 = containerMoveItem verbose iid k c1 c2

containerMoveItem :: MonadStateRead m
                  => Bool -> ItemId -> Int -> Container -> Container
                  -> m [UpdAtomic]
containerMoveItem verbose iid k c1 c2 = do
  bag <- getsState $ getContainerBag c1
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (iid, k, c1, c2)
    Just (_, it) -> do
      moveStash <- moveStashIfNeeded c2
      return $ [UpdLoseItem verbose iid (k, take k it) c1]
               ++ moveStash
               ++ [UpdSpotItem verbose iid (k, take k it) c2]

quitF :: MonadServerAtomic m => Status -> FactionId -> m ()
quitF status fid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  let oldSt = gquit fact
  -- Note that it's the _old_ status that we check here.
  case stOutcome <$> oldSt of
    Just Killed -> return ()    -- Do not overwrite in case
    Just Defeated -> return ()  -- many things happen in 1 turn.
    Just Conquer -> return ()
    Just Escape -> return ()
    _ -> do
      let !_A = assert (stOutcome status `notElem` [Camping, Restart]
                        `blame` "Camping and Restart are handled separately"
                        `swith` (stOutcome <$> oldSt, status, fid)) ()
      -- This runs regardless of the _new_ status.
      manalytics <-
        if fhasUI $ gplayer fact then do
          keepAutomated <- getsServer $ skeepAutomated . soptions
          -- Try to remove AI control of the UI faction, to show gameover info.
          when (isAIFact fact && not keepAutomated) $
            execUpdAtomic $ UpdAutoFaction fid False
          revealAll fid
          -- Likely, by this time UI faction is no longer AI-controlled,
          -- so the score will get registered.
          registerScore status fid
          factionAn <- getsServer sfactionAn
          generationAn <- getsServer sgenerationAn
          return $ Just (factionAn, generationAn)
        else return Nothing
      execUpdAtomic $ UpdQuitFaction fid oldSt (Just status) manalytics
      modifyServer $ \ser -> ser {sbreakLoop = True}  -- check game over

-- Send any UpdQuitFaction actions that can be deduced from factions'
-- current state.
deduceQuits :: MonadServerAtomic m => FactionId -> Status -> m ()
deduceQuits fid0 status@Status{stOutcome}
  | stOutcome `elem` [Defeated, Camping, Restart, Conquer] =
    error $ "no quitting to deduce" `showFailure` (fid0, status)
deduceQuits fid0 status = do
  fact0 <- getsState $ (EM.! fid0) . sfactionD
  let factHasUI = fhasUI . gplayer
      quitFaction (stOutcome, (fid, _)) = quitF status{stOutcome} fid
      mapQuitF outfids = do
        let (withUI, withoutUI) =
              partition (factHasUI . snd . snd)
                        ((stOutcome status, (fid0, fact0)) : outfids)
        mapM_ quitFaction (withoutUI ++ withUI)
      inGameOutcome (fid, fact) = do
        let mout | fid == fid0 = Just $ stOutcome status
                 | otherwise = stOutcome <$> gquit fact
        case mout of
          Just Killed -> False
          Just Defeated -> False
          Just Restart -> False  -- effectively, commits suicide
          _ -> True
  factionD <- getsState sfactionD
  let assocsInGame = filter inGameOutcome $ EM.assocs factionD
      assocsKeepArena = filter (keepArenaFact . snd) assocsInGame
      assocsUI = filter (factHasUI . snd) assocsInGame
      nonHorrorAIG = filter (not . isHorrorFact . snd) assocsInGame
      worldPeace =
        all (\(fid1, _) -> all (\(fid2, fact2) -> not $ isFoe fid2 fact2 fid1)
                           nonHorrorAIG)
        nonHorrorAIG
      othersInGame = filter ((/= fid0) . fst) assocsInGame
  if | null assocsUI ->
       -- Only non-UI players left in the game and they all win.
       mapQuitF $ zip (repeat Conquer) othersInGame
     | null assocsKeepArena ->
       -- Only leaderless and spawners remain (the latter may sometimes
       -- have no leader, just as the former), so they win,
       -- or we could get stuck in a state with no active arena
       -- and so no spawns.
       mapQuitF $ zip (repeat Conquer) othersInGame
     | worldPeace ->
       -- Nobody is at war any more, so all win (e.g., horrors, but never mind).
       mapQuitF $ zip (repeat Conquer) othersInGame
     | stOutcome status == Escape -> do
       -- Otherwise, in a game with many warring teams alive,
       -- only complete Victory matters, until enough of them die.
       let (victors, losers) =
             partition (\(fi, _) -> isFriend fid0 fact0 fi) othersInGame
       mapQuitF $ zip (repeat Escape) victors ++ zip (repeat Defeated) losers
     | otherwise -> quitF status fid0

-- | Save game on server and all clients.
writeSaveAll :: MonadServerAtomic m => Bool -> Bool -> m ()
writeSaveAll uiRequested evenForNoConfirmGames = do
  bench <- getsServer $ sbenchmark . sclientOptions . soptions
  noConfirmsGame <- isNoConfirmsGame
  when (uiRequested
        || not bench && (not noConfirmsGame || evenForNoConfirmGames)) $ do
    execUpdAtomic UpdWriteSave
    saveServer
#ifdef WITH_EXPENSIVE_ASSERTIONS
    -- This check is sometimes repeated in @gameExit@, but we don't care about
    -- speed of shutdown and even more so in WITH_EXPENSIVE_ASSERTIONS mode.
    verifyCaches
#endif

verifyCaches :: MonadServer m => m ()
verifyCaches = do
  sperCacheFid <- getsServer sperCacheFid
  sperValidFid <- getsServer sperValidFid
  sactorMaxSkills2 <- getsState sactorMaxSkills
  sfovLucidLid <- getsServer sfovLucidLid
  sfovClearLid <- getsServer sfovClearLid
  sfovLitLid <- getsServer sfovLitLid
  sperFid <- getsServer sperFid
  actorMaxSkills <- getsState maxSkillsInDungeon
  ( fovLitLid, fovClearLid, fovLucidLid
   ,perValidFid, perCacheFid, perFid ) <- getsState perFidInDungeon
  rngs <- getsServer srngs  -- initial display may scroll off terminal memory
  factionD <- getsState sfactionD
  -- Perception off UI faction at game over is illegal (revealed to the player
  -- in 'revealAll'), which is fine, because it's never used.
  -- Don't verify perception in such cases. All the caches from which
  -- legal perception would be created at that point are legal and verified,
  -- which is almost as tight.
  let gameOverUI fact = fhasUI (gplayer fact)
                        && maybe False ((/= Camping) . stOutcome) (gquit fact)
      isGameOverUI = any gameOverUI $ EM.elems factionD
      !_A7 = assert (sfovLitLid == fovLitLid
                     `blame` "wrong accumulated sfovLitLid"
                     `swith` (sfovLitLid, fovLitLid, rngs)) ()
      !_A6 = assert (sfovClearLid == fovClearLid
                     `blame` "wrong accumulated sfovClearLid"
                     `swith` (sfovClearLid, fovClearLid, rngs)) ()
      !_A5 = assert (sactorMaxSkills2 == actorMaxSkills
                     `blame` "wrong accumulated sactorMaxSkills"
                     `swith` (sactorMaxSkills2, actorMaxSkills, rngs)) ()
      !_A4 = assert (sfovLucidLid == fovLucidLid
                     `blame` "wrong accumulated sfovLucidLid"
                     `swith` (sfovLucidLid, fovLucidLid, rngs)) ()
      !_A3 = assert (sperValidFid == perValidFid
                     `blame` "wrong accumulated sperValidFid"
                     `swith` (sperValidFid, perValidFid, rngs)) ()
      !_A2 = assert (sperCacheFid == perCacheFid
                     `blame` "wrong accumulated sperCacheFid"
                     `swith` (sperCacheFid, perCacheFid, rngs)) ()
      !_A1 = assert (isGameOverUI || sperFid == perFid
                     `blame` "wrong accumulated perception"
                     `swith` (sperFid, perFid, rngs)) ()
  return ()

-- | Tell whether a faction that we know is still in game, keeps arena.
-- Keeping arena means, if the faction is still in game,
-- it always has a leader in the dungeon somewhere.
-- So, leaderless factions and spawner factions do not keep an arena,
-- even though the latter usually has a leader for most of the game.
keepArenaFact :: Faction -> Bool
keepArenaFact fact = fleaderMode (gplayer fact) /= Nothing
                     && fneverEmpty (gplayer fact)

-- We assume the actor in the second argument has HP <= 0 or is going to be
-- dominated right now. Even if the actor is to be dominated,
-- @bfid@ of the actor body is still the old faction.
deduceKilled :: MonadServerAtomic m => ActorId -> m ()
deduceKilled aid = do
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  when (fneverEmpty $ gplayer fact) $ do
    actorsAlive <- anyActorsAlive (bfid body) aid
    when (not actorsAlive) $
      deduceQuits (bfid body) $ Status Killed (fromEnum $ blid body) Nothing

anyActorsAlive :: MonadServer m => FactionId -> ActorId -> m Bool
anyActorsAlive fid aid = do
  as <- getsState $ fidActorNotProjGlobalAssocs fid
  -- We test HP here, in case more than one actor goes to 0 HP in the same turn.
  return $! any (\(aid2, b2) -> aid2 /= aid && bhp b2 > 0) as

electLeader :: MonadServerAtomic m => FactionId -> LevelId -> ActorId -> m ()
electLeader fid lid aidToReplace = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (mleader == Just aidToReplace) $ do
    allOurs <- getsState $ fidActorNotProjGlobalAssocs fid  -- not only on level
    let -- Prefer actors on this level and with positive HP and not sleeping.
        -- Exclude @aidToReplace@, even if not dead (e.g., if being dominated).
        (positive, negative) = partition (\(_, b) -> bhp b > 0) allOurs
        (awake, sleeping) = partition (\(_, b) -> bwatch b /= WSleep) positive
    onThisLevel <- getsState $ fidActorRegularAssocs fid lid
    let candidates = filter (\(_, b) -> bwatch b /= WSleep) onThisLevel
                     ++ awake ++ sleeping ++ negative
        mleaderNew =
          listToMaybe $ filter (/= aidToReplace) $ map fst candidates
    execUpdAtomic $ UpdLeadFaction fid mleader mleaderNew

setFreshLeader :: MonadServerAtomic m => FactionId -> ActorId -> m ()
setFreshLeader fid aid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  unless (fleaderMode (gplayer fact) == Nothing) $ do
    -- First update and send Perception so that the new leader
    -- may report his environment.
    b <- getsState $ getActorBody aid
    let !_A = assert (not $ bproj b) ()
    valid <- getsServer $ (EM.! blid b) . (EM.! fid) . sperValidFid
    unless valid $ updatePer fid (blid b)
    execUpdAtomic $ UpdLeadFaction fid (gleader fact) (Just aid)

updatePer :: MonadServerAtomic m => FactionId -> LevelId -> m ()
updatePer fid lid = do
  -- Performed in the State after action, e.g., with a new actor.
  perNew <- recomputeCachePer fid lid
  updatePerFromNew fid lid perNew

updatePerFromNew :: MonadServerAtomic m
                 => FactionId -> LevelId -> Perception -> m ()
updatePerFromNew fid lid perNew = do
  -- Even if nothing needed to be done, perception is now validated.
  modifyServer $ \ser ->
    ser {sperValidFid = EM.adjust (EM.insert lid True) fid $ sperValidFid ser}
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! fid EM.! lid
      inPer = diffPer perNew perOld
      outPer = diffPer perOld perNew
  unless (nullPer outPer && nullPer inPer) $ do
    -- Perception is modified on the server and sent to the client
    -- together with all the revealed info.
    let fper = EM.adjust (EM.insert lid perNew) fid
    modifyServer $ \ser -> ser {sperFid = fper $ sperFid ser}
    execSendPer fid lid outPer inPer perNew

recomputeCachePer :: MonadServer m => FactionId -> LevelId -> m Perception
recomputeCachePer fid lid = do
  total <- getCacheTotal fid lid
  fovLucid <- getCacheLucid lid
  getsState $ perceptionFromPTotal fid lid fovLucid total

-- The missile item is removed from the store only if the projection
-- went into effect (no failure occured).
projectFail :: MonadServerAtomic m
            => ActorId    -- ^ actor causing the projection
            -> ActorId    -- ^ actor projecting the item (is on current level)
            -> Point      -- ^ starting position of the projectile;
                          --   usually, but not always, position of @origin@
            -> Point      -- ^ target position of the projectile
            -> Int        -- ^ digital line parameter
            -> Bool       -- ^ whether to start at the origin's position
            -> ItemId     -- ^ the item to be projected
            -> CStore     -- ^ which store the items comes from
            -> Bool       -- ^ whether the item is a blast
            -> m (Maybe ReqFailure)
projectFail propeller origin oxy tpxy eps center iid cstore blast = do
  COps{corule=RuleContent{rXmax, rYmax}, coTileSpeedup} <- getsState scops
  body <- getsState $ getActorBody origin
  let lid = blid body
  lvl <- getLevel lid
  case bla rXmax rYmax eps oxy tpxy of
    Nothing -> return $ Just ProjectAimOnself
    Just [] -> error $ "projecting from the edge of level"
                       `showFailure` (oxy, tpxy)
    Just (pos : restUnlimited) -> do
      bag <- getsState $ getBodyStoreBag body cstore
      case EM.lookup iid bag of
        Nothing -> return $ Just ProjectOutOfReach
        Just _kit -> do
          itemFull <- getsState $ itemToFull iid
          actorSk <- currentSkillsServer origin
          actorMaxSk <- getsState $ getActorMaxSkills origin
          let skill = Ability.getSk Ability.SkProject actorSk
              forced = blast || bproj body
              calmE = calmEnough body actorMaxSk
              legal = permittedProject forced skill calmE itemFull
              arItem = aspectRecordFull itemFull
          case legal of
            Left reqFail -> return $ Just reqFail
            Right _ -> do
              let lobable = IA.checkFlag Ability.Lobable arItem
                  rest = if lobable
                         then take (chessDist oxy tpxy - 1) restUnlimited
                         else restUnlimited
                  t = lvl `at` pos
              if | not $ Tile.isWalkable coTileSpeedup t ->
                   return $ Just ProjectBlockTerrain
                 | occupiedBigLvl pos lvl ->
                   if blast then do
                     -- Hit the blocking actor by starting the explosion
                     -- particle where the projectile landed, not a step away.
                     -- The same when the spot has the explosive embed,
                     -- regardless if it's walkable (@pos@ is, that's enough).
                     -- No problem even if there's a big actor where
                     -- the projectile starts, though it's wierd it may get
                     -- away unharmed sometimes.
                     projectBla propeller origin oxy (pos:rest)
                                iid cstore blast
                     return Nothing
                   else return $ Just ProjectBlockActor
                 | otherwise -> do
                   -- Make the explosion less regular and weaker at the edges.
                   if blast && center then
                     -- Start in the center, not around, even if the center
                     -- is a non-walkable tile with the exploding embed
                     -- or if a big actor is there.
                     projectBla propeller origin oxy (pos:rest)
                                iid cstore blast
                   else
                     projectBla propeller origin pos rest
                                iid cstore blast
                   return Nothing

projectBla :: MonadServerAtomic m
           => ActorId    -- ^ actor causing the projection
           -> ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ starting point of the projectile
           -> [Point]    -- ^ rest of the trajectory of the projectile
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ which store the items comes from
           -> Bool       -- ^ whether the item is a blast
           -> m ()
projectBla propeller origin pos rest iid cstore blast = do
  body <- getsState $ getActorBody origin
  let lid = blid body
  localTime <- getsState $ getLocalTime lid
  unless blast $ execSfxAtomic $ SfxProject origin iid
  bag <- getsState $ getBodyStoreBag body cstore
  ItemFull{itemKind} <- getsState $ itemToFull iid
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (origin, pos, rest, iid, cstore)
    Just kit@(_, it) -> do
      let delay =
            if IK.iweight itemKind == 0
            then timeTurn  -- big delay at start, e.g., to easily read hologram
            else timeZero  -- avoid running into own projectiles
          btime = absoluteTimeAdd delay localTime
      addProjectile propeller pos rest iid kit lid (bfid body) btime
      let c = CActor origin cstore
      execUpdAtomic $ UpdLoseItem False iid (1, take 1 it) c

addActorFromGroup :: MonadServerAtomic m
                  => GroupName ItemKind -> FactionId -> Point -> LevelId -> Time
                  -> m (Maybe ActorId)
addActorFromGroup actorGroup fid pos lid time = do
  Level{ldepth} <- getLevel lid
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- that contains the fixed properties of all actors of that kind.
  freq <- prepareItemKind 0 ldepth [(actorGroup, 1)]
  m2 <- rollItemAspect freq ldepth
  case m2 of
    NoNewItem -> return Nothing
    NewItem itemKnown itemFull itemQuant -> do
      let itemFullKit = (itemFull, itemQuant)
      Just <$> registerActor False itemKnown itemFullKit fid pos lid time

registerActor :: MonadServerAtomic m
              => Bool -> ItemKnown -> ItemFullKit
              -> FactionId -> Point -> LevelId -> Time
              -> m ActorId
registerActor summoned (ItemKnown kindIx ar _) (itemFullRaw, kit)
              bfid pos lid time = do
  let container = CTrunk bfid lid pos
      jfid = Just bfid
      itemKnown = ItemKnown kindIx ar jfid
      itemFull = itemFullRaw {itemBase = (itemBase itemFullRaw) {jfid}}
  trunkId <- registerItem False (itemFull, kit) itemKnown container
  aid <- addNonProjectile summoned trunkId (itemFull, kit) bfid pos lid time
  fact <- getsState $ (EM.! bfid) . sfactionD
  actorMaxSk <- getsState $ getActorMaxSkills aid
  condAnyFoeAdj <- getsState $ anyFoeAdj aid
  when (canSleep actorMaxSk
        && not condAnyFoeAdj
        && not summoned
        && not (fhasGender (gplayer fact))) $ do  -- heroes never start asleep
    -- A lot of actors will wake up at once anyway, so let most start sleeping.
    let sleepOdds = if prefersSleep actorMaxSk then 19%20 else 2%3
    sleeps <- rndToAction $ chance sleepOdds
    when sleeps $ addSleep aid
  return aid

addProjectile :: MonadServerAtomic m
              => ActorId -> Point -> [Point] -> ItemId -> ItemQuant -> LevelId
              -> FactionId -> Time
              -> m ()
addProjectile propeller pos rest iid (_, it) lid fid time = do
  itemFull <- getsState $ itemToFull iid
  let arItem = aspectRecordFull itemFull
      IK.ThrowMod{IK.throwHP} = IA.aToThrow arItem
      (trajectory, (speed, _)) =
        IA.itemTrajectory arItem (itemKind itemFull) (pos : rest)
      -- Trunk is added to equipment, not to organs, because it's the
      -- projected item, so it's carried, not grown.
      tweakBody b = b { bhp = xM throwHP
                      , btrajectory = Just (trajectory, speed)
                      , beqp = EM.singleton iid (1, take 1 it) }
  aid <- addActorIid iid itemFull True fid pos lid tweakBody
  bp <- getsState $ getActorBody propeller
  -- If propeller is a projectile, it may produce other projectiles, e.g.,
  -- by exploding, so it's not voluntary, so others are to blame.
  -- However, we can't easily see whether a pushed non-projectile actor
  -- produced a projectile due to colliding or voluntarily, so we assign
  -- blame to him.
  originator <- if bproj bp
                then getsServer $ EM.findWithDefault propeller propeller
                                  . strajPushedBy
                else return propeller
  modifyServer $ \ser ->
    ser { strajTime = updateActorTime fid lid aid time $ strajTime ser
        , strajPushedBy = EM.insert aid originator $ strajPushedBy ser }

addNonProjectile :: MonadServerAtomic m
                 => Bool -> ItemId -> ItemFullKit -> FactionId -> Point
                 -> LevelId -> Time
                 -> m ActorId
addNonProjectile summoned trunkId (itemFull, kit) fid pos lid time = do
  let tweakBody b = b { borgan = EM.singleton trunkId kit
                      , bcalm = if summoned
                                then xM 5  -- a tiny buffer before domination
                                else bcalm b }
  aid <- addActorIid trunkId itemFull False fid pos lid tweakBody
  -- We assume actor is never born pushed.
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime fid lid aid time $ sactorTime ser}
  return aid

addActorIid :: MonadServerAtomic m
            => ItemId -> ItemFull -> Bool -> FactionId -> Point -> LevelId
            -> (Actor -> Actor)
            -> m ActorId
addActorIid trunkId ItemFull{itemBase, itemKind, itemDisco=ItemDiscoFull arItem}
            bproj fid pos lid tweakBody = do
  COps{coitem} <- getsState scops
  -- Initial HP and Calm is based only on trunk and ignores organs.
  let trunkMaxHP = max 2 $ IA.getSkill Ability.SkMaxHP arItem
      hp = xM trunkMaxHP `div` 2
      -- Slightly reduced starting Calm to auto-id items that refill Calm
      -- and to let animals do some initial exploration before going to sleep.
      -- Higher reduction would cause confusingly low sight range at game
      -- start and even inability to handle equipment.
      calm = xM (max 1 $ IA.getSkill Ability.SkMaxCalm arItem - 10)
  -- Create actor.
  factionD <- getsState sfactionD
  curChalSer <- getsServer $ scurChalSer . soptions
  let fact = factionD EM.! fid
  bnumberTeam <- case gteamCont fact of
    Just teamContinuity | not bproj -> do
      stcounter <- getsServer stcounter
      let number = EM.findWithDefault 0 teamContinuity stcounter
      modifyServer $ \ser -> ser {stcounter =
        EM.insert teamContinuity (succ number) stcounter}
      return $ Just (number, teamContinuity)
    _ -> return Nothing
  let bnumber = fst <$> bnumberTeam
  -- If difficulty is below standard, HP is added to the UI factions,
  -- otherwise HP is added to their enemies.
  -- If no UI factions, their role is taken by the escapees (for testing).
  let diffBonusCoeff = difficultyCoeff $ cdiff curChalSer
      -- For most projectiles (exceptions are, e.g.,  maxHP boosting rings),
      -- SkMaxHP is zero, which means they drop after one hit regardless
      -- of extra bhp they have due to piercing. That is fine.
      -- If we want armoured missiles, that should not be done via piercing,
      -- but via SkMaxHP of the thrown items. Rings that are piercing
      -- by coincidence are harmless, too. However, piercing should not be
      -- added to missiles via SkMaxHP or equipping them would be beneficial
      -- in a hard to balance way (e.g., one bullet adds 10 SkMaxHP).
      boostFact = not bproj
                  && if diffBonusCoeff > 0
                     then any (fhasUI . gplayer . snd)
                              (filter (\(fi, fa) -> isFriend fi fa fid)
                                      (EM.assocs factionD))
                     else any (fhasUI . gplayer  . snd)
                              (filter (\(fi, fa) -> isFoe fi fa fid)
                                      (EM.assocs factionD))
      finalHP | boostFact = min (xM 899)  -- no more than UI can stand
                                (hp * 2 ^ abs diffBonusCoeff)
              | otherwise = hp
      -- Prevent too high max HP resulting in panic when low HP/max HP ratio.
      maxHP = min (finalHP + xM 100) (2 * finalHP)
      bonusHP = fromEnum (maxHP `div` oneM) - trunkMaxHP
      healthOrgans = [(Just bonusHP, (IK.S_BONUS_HP, COrgan)) | bonusHP /= 0]
      b = actorTemplate trunkId bnumber finalHP calm pos lid fid bproj
      withTrunk =
        b { bweapon = if IA.checkFlag Ability.Meleeable arItem then 1 else 0
          , bweapBenign =
              if IA.checkFlag Ability.Meleeable arItem
                 && IA.checkFlag Ability.Benign arItem then 1 else 0 }
      bodyTweaked = tweakBody withTrunk
  aid <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ aid}
  execUpdAtomic $ UpdCreateActor aid bodyTweaked [(trunkId, itemBase)]
  unless bproj $ do
    steamGearCur <- getsServer steamGearCur
    let gearList = case bnumberTeam of
          Nothing -> []
          Just (number, teamContinuity) ->
            case teamContinuity `EM.lookup` steamGearCur of
              Nothing -> []
              Just im -> IM.findWithDefault [] number im
    -- Create, register and insert all initial actor items, including
    -- the bonus health organs from difficulty setting.
    forM_ (healthOrgans ++ map (Nothing,) (IK.ikit itemKind))
          $ \(mk, (ikGrp, cstore)) -> do
     -- TODO: remove ASAP. This is a hack that prevents AI from stealing
     -- backstories until there is enough of them in Allure.
     -- Instead, pre-generate 20 player heroes to make sure all unique
     -- backstories are available to the player and so that the order
     -- of games played doesn't affect their availability.
     if ikGrp == DefsInternal.GroupName "backstory"
        && isJust bnumberTeam
        && (snd <$> bnumberTeam) /= Just teamExplorer
     then return ()
     else do
      let container = CActor aid cstore
      Level{ldepth} <- getLevel lid
      mIidEtc <- case lookup ikGrp gearList of
        Nothing -> do
          let itemFreq = [(ikGrp, 1)]
          -- Power depth of new items unaffected by number of spawned actors.
          freq <- prepareItemKind 0 ldepth itemFreq
          mIidEtc <- rollAndRegisterItem False ldepth freq container mk
          case (bnumberTeam, mIidEtc) of
            (Just (number, teamContinuity), Just (_, (itemFull2, _))) -> do
              let arItem2 = aspectRecordFull itemFull2
                  inMetaGame = IA.checkFlag Ability.MetaGame arItem2
                  itemKindId2 = itemKindId itemFull2
              when inMetaGame $ do
                let altInner ml = Just $ (ikGrp, itemKindId2) : fromMaybe [] ml
                    alt mim =
                      Just $ IM.alter altInner number $ fromMaybe IM.empty mim
                modifyServer $ \ser ->
                  ser {steamGear = EM.alter alt teamContinuity $ steamGear ser}
            _ -> return ()
          return mIidEtc
        Just itemKindId2 -> do
          let gearListNew = delete (ikGrp, itemKindId2) gearList
              (number, teamContinuity) = fromJust bnumberTeam
              alt mim =
                Just $ IM.insert number gearListNew $ fromMaybe IM.empty mim
          modifyServer $ \ser ->
            ser {steamGearCur = EM.alter alt teamContinuity steamGearCur}
          let itemKind2 = okind coitem itemKindId2
              freq = pure (itemKindId2, itemKind2)
          rollAndRegisterItem False ldepth freq container mk
      case mIidEtc of
        Nothing -> error $ "" `showFailure` (lid, ikGrp, container, mk)
        Just (iid, (itemFull2, _)) ->
          when (cstore /= CGround) $
            -- The items are created owned by actors, so won't be picked up,
            -- so we have to discover them now, if eligible.
            discoverIfMinorEffects container iid (itemKindId itemFull2)
  return aid
addActorIid _ _ _ _ _ _ _ = error "addActorIid: server ignorant about an item"

discoverIfMinorEffects :: MonadServerAtomic m
                       => Container -> ItemId -> ContentId ItemKind -> m ()
discoverIfMinorEffects c iid itemKindId = do
  COps{coitem} <- getsState scops
  discoAspect <- getsState sdiscoAspect
  let arItem = discoAspect EM.! iid
      itemKind = okind coitem itemKindId
   -- Otherwise, discover by use when item's effects get activated later on.
  when (IA.onlyMinorEffects arItem itemKind
        && not (IA.isHumanTrinket itemKind)) $
    execUpdAtomic $ UpdDiscover c iid itemKindId arItem

pickWeaponServer :: MonadServer m => ActorId -> m (Maybe (ItemId, CStore))
pickWeaponServer source = do
  eqpAssocs <- getsState $ kitAssocs source [CEqp]
  bodyAssocs <- getsState $ kitAssocs source [COrgan]
  actorSk <- currentSkillsServer source
  sb <- getsState $ getActorBody source
  let kitAssRaw = eqpAssocs ++ bodyAssocs
      forced = bproj sb
      kitAss | forced = kitAssRaw  -- for projectiles, anything is weapon
             | otherwise =
                 filter (IA.checkFlag Ability.Meleeable
                         . aspectRecordFull . fst . snd) kitAssRaw
  -- Server ignores item effects or it would leak item discovery info.
  -- Hence, weapons with powerful burning or wouding are undervalued.
  -- In particular, it even uses weapons that would heal an opponent.
  -- But server decides only in exceptiona cases, e.g. projectile collision
  -- or melee in place of an impossible displace. Otherwise, client decides.
  strongest <- pickWeaponM False Nothing kitAss actorSk source
  case strongest of
    [] -> return Nothing
    iis@((value1, hasEffect1, timeout1, _, _, _) : _) -> do
      let minIis = takeWhile (\(value, hasEffect, timeout, _, _, _) ->
                                 value == value1
                                 && hasEffect == hasEffect1
                                 && timeout == timeout1)
                             iis
      (_, _, _, _, iid, _) <- rndToAction $ oneOf minIis
      let cstore = if isJust (lookup iid bodyAssocs) then COrgan else CEqp
      return $ Just (iid, cstore)

-- @MonadStateRead@ would be enough, but the logic is sound only on server.
currentSkillsServer :: MonadServer m => ActorId -> m Ability.Skills
currentSkillsServer aid  = do
  body <- getsState $ getActorBody aid
  mleader <- getsState $ gleader . (EM.! bfid body) . sfactionD
  getsState $ actorCurrentSkills mleader aid

getCacheLucid :: MonadServer m => LevelId -> m FovLucid
getCacheLucid lid = do
  fovClearLid <- getsServer sfovClearLid
  fovLitLid <- getsServer sfovLitLid
  fovLucidLid <- getsServer sfovLucidLid
  let getNewLucid = getsState $ \s ->
        lucidFromLevel fovClearLid fovLitLid s lid (sdungeon s EM.! lid)
  case EM.lookup lid fovLucidLid of
    Just (FovValid fovLucid) -> return fovLucid
    _ -> do
      newLucid <- getNewLucid
      modifyServer $ \ser ->
        ser {sfovLucidLid = EM.insert lid (FovValid newLucid)
                            $ sfovLucidLid ser}
      return newLucid

getCacheTotal :: MonadServer m => FactionId -> LevelId -> m CacheBeforeLucid
getCacheTotal fid lid = do
  sperCacheFidOld <- getsServer sperCacheFid
  let perCacheOld = sperCacheFidOld EM.! fid EM.! lid
  case ptotal perCacheOld of
    FovValid total -> return total
    FovInvalid -> do
      actorMaxSkills <- getsState sactorMaxSkills
      fovClearLid <- getsServer sfovClearLid
      getActorB <- getsState $ flip getActorBody
      let perActorNew =
            perActorFromLevel (perActor perCacheOld) getActorB
                              actorMaxSkills (fovClearLid EM.! lid)
          -- We don't check if any actor changed, because almost surely one is.
          -- Exception: when an actor is destroyed, but then union differs, too.
          total = totalFromPerActor perActorNew
          perCache = PerceptionCache { ptotal = FovValid total
                                     , perActor = perActorNew }
          fperCache = EM.adjust (EM.insert lid perCache) fid
      modifyServer $ \ser -> ser {sperCacheFid = fperCache $ sperCacheFid ser}
      return total

allGroupItems :: MonadServerAtomic m
              => CStore -> GroupName ItemKind -> ActorId
              -> m [(ItemId, ItemQuant)]
allGroupItems store grp target = do
  COps{coitem} <- getsState scops
  b <- getsState $ getActorBody target
  assocsCStore <- getsState $ EM.assocs . getBodyStoreBag b store
  getKindId <- getsState $ flip getIidKindIdServer
  let assocsKindId = map (\as@(iid, _) -> (getKindId iid, as)) assocsCStore
      hasGroup (itemKindId, _) =
        maybe False (> 0) $ lookup grp $ IK.ifreq $ okind coitem itemKindId
  return $! map snd $ sortBy (comparing fst) $ filter hasGroup assocsKindId

addCondition :: MonadServerAtomic m
             => Bool -> GroupName ItemKind -> ActorId -> m ()
addCondition verbose name aid = do
  b <- getsState $ getActorBody aid
  Level{ldepth} <- getLevel $ blid b
  let c = CActor aid COrgan
  -- Power depth of new items unaffected by number of spawned actors.
  freq <- prepareItemKind 0 ldepth [(name, 1)]
  mresult <- rollAndRegisterItem verbose ldepth freq c Nothing
  assert (isJust mresult) $ return ()

removeConditionSingle :: MonadServerAtomic m
                      => GroupName ItemKind -> ActorId -> m Int
removeConditionSingle name aid = do
  let c = CActor aid COrgan
  is <- allGroupItems COrgan name aid
  case is of
    [(iid, (nAll, itemTimer))] -> do
      execUpdAtomic $ UpdLoseItem False iid (1, itemTimer) c
      return $ nAll - 1
    _ -> error $ "missing or multiple item" `showFailure` (name, is)

addSleep :: MonadServerAtomic m => ActorId -> m ()
addSleep aid = do
  b <- getsState $ getActorBody aid
  addCondition True IK.S_ASLEEP aid
  execUpdAtomic $ UpdWaitActor aid (bwatch b) WSleep

removeSleepSingle :: MonadServerAtomic m => ActorId -> m ()
removeSleepSingle aid = do
  nAll <- removeConditionSingle IK.S_ASLEEP aid
  when (nAll == 0) $
    execUpdAtomic $ UpdWaitActor aid WWake WWatch

addKillToAnalytics :: MonadServerAtomic m
                   => ActorId -> KillHow -> FactionId -> ItemId -> m ()
addKillToAnalytics aid killHow fid iid = do
  actorD <- getsState sactorD
  case EM.lookup aid actorD of
    Just b ->
      modifyServer $ \ser ->
        ser { sfactionAn = addFactionKill (bfid b) killHow fid iid
                           $ sfactionAn ser
            , sactorAn = addActorKill aid killHow fid iid
                         $ sactorAn ser }
    Nothing -> return ()  -- killer dead, too late to assign blame
