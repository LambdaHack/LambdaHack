-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartM
  ( initPer, reinitGame, gameReset, applyDebug
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , sampleTrunks, sampleItems
  , mapFromFuns, resetFactions, populateDungeon, findEntryPoss
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import           Data.Key (mapWithKeyM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple (swap)
import qualified NLP.Miniutter.English as MU
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour
import           Game.LambdaHack.Server.CommonM
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import           Game.LambdaHack.Server.Fov
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.ItemRev
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

initPer :: MonadServer m => m ()
initPer = do
  ( sfovLitLid, sfovClearLid, sfovLucidLid
   ,sperValidFid, sperCacheFid, sperFid ) <- getsState perFidInDungeon
  modifyServer $ \ser ->
    ser { sfovLitLid, sfovClearLid, sfovLucidLid
        , sperValidFid, sperCacheFid, sperFid }

reinitGame :: MonadServerAtomic m => m ()
reinitGame = do
  COps{coitem} <- getsState scops
  pers <- getsServer sperFid
  ServerOptions{scurChalSer, sknowMap, sshowItemSamples, sclientOptions}
    <- getsServer soptions
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which needs to be updated in clients
  -- at this point to keep them in sync with changes on the server.
  s <- getState
  discoS <- getsState sdiscoKind
  -- Thanks to the following, for any item with not hidden identity,
  -- the client has its kind from the start.
  let discoKindFiltered =
        let f kindId = isNothing $ IK.getMandatoryPresentAsFromKind
                                 $ okind coitem kindId
        in EM.filter f discoS
      defL | sknowMap = s
           | otherwise = localFromGlobal s
      defLocal = updateDiscoKind (const discoKindFiltered) defL
  factionD <- getsState sfactionD
  modifyServer $ \ser -> ser {sclientStates = EM.map (const defLocal) factionD}
  let updRestart fid = UpdRestart fid (pers EM.! fid) defLocal
                                  scurChalSer sclientOptions
  mapWithKeyM_ (\fid _ -> do
    -- Different seed for each client, to make sure behaviour is varied.
    gen1 <- getsServer srandom
    let (clientRandomSeed, gen2) = SM.splitSMGen gen1
    modifyServer $ \ser -> ser {srandom = gen2}
    execUpdAtomic $ updRestart fid clientRandomSeed) factionD
  dungeon <- getsState sdungeon
  let sactorTime = EM.map (const (EM.map (const EM.empty) dungeon)) factionD
      strajTime = EM.map (const (EM.map (const EM.empty) dungeon)) factionD
  modifyServer $ \ser -> ser {sactorTime, strajTime}
  when sshowItemSamples $ do
    genOrig <- getsServer srandom
    uniqueSetOrig <- getsServer suniqueSet
    genOld <- getsServer sgenerationAn
    genSampleTrunks <- sampleTrunks dungeon
    genSampleItems <- sampleItems dungeon
    let sgenerationAn = EM.unions [genSampleTrunks, genSampleItems, genOld]
    modifyServer $ \ser -> ser {sgenerationAn}
    -- Make sure the debug generations don't affect future RNG behaviour.
    -- However, in the long run, AI behaviour is affected anyway,
    -- because the items randomly chosen for AI actions are ordered by their
    -- @ItemId@, which is affected by the sample item generation.
    modifyServer $ \ser -> ser {srandom = genOrig, suniqueSet = uniqueSetOrig}
  populateDungeon
  mapM_ (\fid -> mapM_ (updatePer fid) (EM.keys dungeon))
        (EM.keys factionD)

-- For simplicity only spawnable actors are taken into account, not starting
-- actors of any faction nor summonable actors.
sampleTrunks :: MonadServerAtomic m => Dungeon -> m GenerationAnalytics
sampleTrunks dungeon = do
  COps{cocave, coitem} <- getsState scops
  factionD <- getsState sfactionD
  let getGroups Level{lkind} = map fst $ CK.cactorFreq $ okind cocave lkind
      groups = S.elems $ S.fromList $ concatMap getGroups $ EM.elems dungeon
      addGroupToSet !s0 !grp =
        ofoldlGroup' coitem grp (\s _ ik _ -> ES.insert ik s) s0
      trunkKindIds = ES.elems $ foldl' addGroupToSet ES.empty groups
      minLid = fst $ minimumBy (comparing (ldepth . snd))
                   $ EM.assocs dungeon
      regItem itemKindId = do
        let itemKind = okind coitem itemKindId
            freq = pure (itemKindId, itemKind)
        case possibleActorFactions itemKind factionD of
          [] -> return Nothing
          (fid, _) : _ -> do
            let c = CTrunk fid minLid originPoint
                jfid = Just fid
            m2 <- rollItemAspect freq minLid
            case m2 of
              NoNewItem -> error "sampleTrunks: can't create actor trunk"
              NewItem (ItemKnown kindIx ar _) itemFullRaw itemQuant -> do
                let itemKnown = ItemKnown kindIx ar jfid
                    itemFull =
                      itemFullRaw {itemBase = (itemBase itemFullRaw) {jfid}}
                Just <$> registerItem False (itemFull, itemQuant) itemKnown c
  miids <- mapM regItem trunkKindIds
  return $! EM.singleton STrunk
            $ EM.fromDistinctAscList $ zip (catMaybes miids) $ repeat 0

-- For simplicity, only actors generated on the ground are taken into account.
-- not starting items of any actors nor items that can be create by effects
-- occuring in the game.
sampleItems :: MonadServerAtomic m => Dungeon -> m GenerationAnalytics
sampleItems dungeon = do
  COps{cocave, coitem} <- getsState scops
  let getGroups Level{lkind} = map fst $ CK.citemFreq $ okind cocave lkind
      groups = S.elems $ S.fromList $ concatMap getGroups $ EM.elems dungeon
      addGroupToSet !s0 !grp =
        ofoldlGroup' coitem grp (\s _ ik _ -> ES.insert ik s) s0
      itemKindIds = ES.elems $ foldl' addGroupToSet ES.empty groups
      minLid = fst $ minimumBy (comparing (ldepth . snd))
                   $ EM.assocs dungeon
      regItem itemKindId = do
        let itemKind = okind coitem itemKindId
            freq = pure (itemKindId, itemKind)
            c = CFloor minLid originPoint
        m2 <- rollItemAspect freq minLid
        case m2 of
          NoNewItem -> error "sampleItems: can't create sample item"
          NewItem itemKnown itemFull _ ->
            Just <$> registerItem False (itemFull, (0, [])) itemKnown c
  miids <- mapM regItem itemKindIds
  return $! EM.singleton SItem
            $ EM.fromDistinctAscList $ zip (catMaybes miids) $ repeat 0

mapFromFuns :: Ord b => [a] -> [a -> b] -> M.Map b a
mapFromFuns domain =
  let fromFun f m1 =
        let invAssocs = map (\c -> (f c, c)) domain
            m2 = M.fromList invAssocs
        in m2 `M.union` m1
  in foldr fromFun M.empty

resetFactions :: FactionDict -> ContentId ModeKind -> Int -> Dice.AbsDepth
              -> Roster
              -> Rnd FactionDict
resetFactions factionDold gameModeIdOld curDiffSerOld totalDepth players = do
  let rawCreate (gplayer@Player{..}, initialActors) = do
        let castInitialActors (ln, d, actorGroup) = do
              n <- castDice (Dice.AbsDepth $ abs ln) totalDepth d
              return (ln, n, actorGroup)
        ginitial <- mapM castInitialActors initialActors
        let cmap =
              mapFromFuns Color.legalFgCol
                          [colorToTeamName, colorToPlainName, colorToFancyName]
            colorName = T.toLower $ head $ T.words fname
            prefix = case fleaderMode of
              LeaderNull -> "Loose"
              LeaderAI _ -> "Autonomous"
              LeaderUI _ -> "Controlled"
            gnameNew = prefix <+> if fhasGender
                                  then makePhrase [MU.Ws $ MU.Text fname]
                                  else fname
            gcolor = M.findWithDefault Color.BrWhite colorName cmap
            gvictimsDnew = case find (\fact -> gname fact == gnameNew)
                                $ EM.elems factionDold of
              Nothing -> EM.empty
              Just fact ->
                let sing = IM.singleton curDiffSerOld (gvictims fact)
                    f = IM.unionWith (EM.unionWith (+))
                in EM.insertWith f gameModeIdOld sing $ gvictimsD fact
        let gname = gnameNew
            gdipl = EM.empty  -- fixed below
            gquit = Nothing
            _gleader = Nothing
            gvictims = EM.empty
            gvictimsD = gvictimsDnew
            gstash = Nothing
        return $! Faction{..}
  lUI <- mapM rawCreate $ filter (fhasUI . fst) $ rosterList players
  let !_A = assert (length lUI <= 1
                    `blame` "currently, at most one faction may have a UI"
                    `swith` lUI) ()
  lnoUI <- mapM rawCreate $ filter (not . fhasUI . fst) $ rosterList players
  let lFs = reverse (zip [toEnum (-1), toEnum (-2)..] lnoUI)  -- sorted
            ++ zip [toEnum 1..] lUI
      swapIx l =
        let findPlayerName name = find ((name ==) . fname . gplayer . snd)
            f (name1, name2) =
              case (findPlayerName name1 lFs, findPlayerName name2 lFs) of
                (Just (ix1, _), Just (ix2, _)) -> (ix1, ix2)
                _ -> error $ "unknown faction"
                             `showFailure` ((name1, name2), lFs)
            ixs = map f l
        -- Only symmetry is ensured, everything else is permitted, e.g.,
        -- a faction in alliance with two others that are at war.
        in ixs ++ map swap ixs
      mkDipl diplMode =
        let f (ix1, ix2) =
              let adj fact = fact {gdipl = EM.insert ix2 diplMode (gdipl fact)}
              in EM.adjust adj ix1
        in foldr f
      rawFs = EM.fromDistinctAscList lFs
      -- War overrides alliance, so 'warFs' second.
      allianceFs = mkDipl Alliance rawFs (swapIx (rosterAlly players))
      warFs = mkDipl War allianceFs (swapIx (rosterEnemy players))
  return $! warFs

gameReset :: MonadServer m
          => ServerOptions -> Maybe (GroupName ModeKind)
          -> Maybe SM.SMGen -> m State
gameReset serverOptions mGameMode mrandom = do
  -- Dungeon seed generation has to come first, to ensure item boosting
  -- is determined by the dungeon RNG.
  cops@COps{comode} <- getsState scops
  dungeonSeed <- getSetGen $ sdungeonRng serverOptions `mplus` mrandom
  srandom <- getSetGen $ smainRng serverOptions `mplus` mrandom
  let srngs = RNGs (Just dungeonSeed) (Just srandom)
  when (sdumpInitRngs serverOptions) $ dumpRngs srngs
  scoreTable <- restoreScore cops
  factionDold <- getsState sfactionD
  gameModeIdOld <- getsState sgameModeId
  curChalSer <- getsServer $ scurChalSer . soptions
  let gameMode = fromMaybe INSERT_COIN
                 $ mGameMode `mplus` sgameMode serverOptions
      rnd :: Rnd (FactionDict, FlavourMap, DiscoveryKind, DiscoveryKindRev,
                  DungeonGen.FreshDungeon, ContentId ModeKind)
      rnd = do
        modeKindId <-
          fromMaybe (error $ "Unknown game mode:" `showFailure` gameMode)
          <$> opick comode gameMode (const True)
        let mode = okind comode modeKindId
            automatePS ps = ps {rosterList =
              map (first $ automatePlayer True) $ rosterList ps}
            players = if sautomateAll serverOptions
                      then automatePS $ mroster mode
                      else mroster mode
        sflavour <- dungeonFlavourMap cops
        (discoKind, sdiscoKindRev) <- serverDiscos cops
        freshDng <- DungeonGen.dungeonGen cops serverOptions $ mcaves mode
        factionD <- resetFactions factionDold gameModeIdOld
                                  (cdiff curChalSer)
                                  (DungeonGen.freshTotalDepth freshDng)
                                  players
        return ( factionD, sflavour, discoKind
               , sdiscoKindRev, freshDng, modeKindId )
  let ( factionD, sflavour, discoKind
       ,sdiscoKindRev, DungeonGen.FreshDungeon{..}, modeKindId ) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshTotalDepth
                                factionD cops scoreTable modeKindId discoKind
      defSer = emptyStateServer { srandom
                                , srngs }
  putServer defSer
  modifyServer $ \ser -> ser {sdiscoKindRev, sflavour}
  return $! defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: forall m. MonadServerAtomic m => m ()
populateDungeon = do
  cops@COps{coTileSpeedup} <- getsState scops
  factionD <- getsState sfactionD
  curChalSer <- getsServer $ scurChalSer . soptions
  let nGt0 (_, n, _) = n > 0
      ginitialWolf fact1 = if cwolf curChalSer && fhasUI (gplayer fact1)
                           then case filter nGt0 $ ginitial fact1 of
                             [] -> []
                             (ln, _, grp) : _ -> [(ln, 1, grp)]
                           else ginitial fact1
      -- Players that escape go first to be started over stairs, if possible,
      -- and far from escapes.
      valuePlayer pl = (not $ fcanEscape pl, fname pl)
      -- Sorting, to keep games from similar game modes mutually reproducible.
      needInitialCrew = sortOn (valuePlayer . gplayer . snd)
                        $ filter (not . null . ginitialWolf . snd)
                        $ EM.assocs factionD
      getEntryLevels (_, fact) =
        map (\(ln, _, _) -> toEnum ln) $ ginitialWolf fact
      arenas = ES.elems $ ES.fromList $ concatMap getEntryLevels needInitialCrew
      hasActorsOnArena lid (_, fact) =
        any (\(ln, _, _) -> toEnum ln == lid) $ ginitialWolf fact
      initialActorPositions :: LevelId
                            -> m (LevelId, EM.EnumMap FactionId Point)
      initialActorPositions lid = do
        lvl <- getLevel lid
        let arenaFactions =
              map fst $ filter (hasActorsOnArena lid) needInitialCrew
        entryPoss <- rndToAction
                     $ findEntryPoss cops lid lvl (length arenaFactions)
        when (length entryPoss < length arenaFactions) $ debugPossiblyPrint
          "Server: populateDungeon: failed to find enough distinct faction starting positions; some factions share positions"
        let usedPoss = EM.fromList $ zip arenaFactions $ cycle entryPoss
        return $ (lid, usedPoss)
  factionPositions <- EM.fromDistinctAscList
                      <$> mapM initialActorPositions arenas
  let initialActors :: (FactionId, Faction) -> m ()
      initialActors (fid3, fact3) = do
        let initActors = ginitialWolf fact3
        mapM_ (placeActors fid3) initActors
      placeActors :: FactionId -> (Int, Int, GroupName ItemKind) -> m ()
      placeActors fid3 (ln, n, actorGroup) = do
        let lid = toEnum ln
        lvl <- getLevel lid
        let ppos = factionPositions EM.! lid EM.! fid3
            validTile t = not $ Tile.isNoActor coTileSpeedup t
            -- This takes into account already spawned actors of this
            -- and other factions. If not enough space, some are skipped.
            psFree = nearbyFreePoints cops lvl validTile ppos
            ps = take n psFree
        when (length ps < n) $ debugPossiblyPrint
          "Server: populateDungeon: failed to find enough initial actor positions; some actors are not generated"
        localTime <- getsState $ getLocalTime lid
        forM_ ps $ \p -> do
          rndDelay <- rndToAction $ randomR (1, clipsInTurn - 1)
          let delta = timeDeltaScale (Delta timeClip) rndDelay
              rndTime = timeShift localTime delta
          maid <- addActorFromGroup actorGroup fid3 p lid rndTime
          case maid of
            Nothing -> error $ "can't spawn initial actors"
                               `showFailure` (lid, fid3)
            Just aid -> do
              mleader <- getsState $ gleader . (EM.! fid3) . sfactionD
              -- Sleeping actor may become a leader, but it's quickly corrected.
              when (isNothing mleader) $ setFreshLeader fid3 aid
  placeItemsInDungeon factionPositions
  embedItemsInDungeon
  mapM_ initialActors needInitialCrew

-- | Find starting postions for all factions. Try to make them distant
-- from each other. Place as many of the factions, as possible,
-- over stairs. Place the last faction(s) over escape(s)
-- (we assume they are guardians of the escapes).
-- This implies the inital factions (if any) start far from escapes.
findEntryPoss :: COps -> LevelId -> Level -> Int -> Rnd [Point]
findEntryPoss COps{coTileSpeedup}
              lid lvl@Level{larea, lstair, lescape} k = do
  let (_, xspan, yspan) = spanArea larea
      factionDist = max xspan yspan - 10
      dist !poss !cmin !l _ = all (\ !pos -> chessDist l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind !ps !n = do
        let ds = [ dist ps factionDist
                 , dist ps $ 2 * factionDist `div` 3
                 , dist ps $ factionDist `div` 2
                 , dist ps $ factionDist `div` 3
                 , dist ps $ factionDist `div` 4
                 , dist ps $ factionDist `div` 5
                 ]
        mp <- findPosTry2 500 lvl  -- try really hard, for skirmish fairness
                (\_ !t -> Tile.isWalkable coTileSpeedup t
                          && not (Tile.isNoActor coTileSpeedup t))
                (take 2 ds)  -- don't pick too close @isOftenActor@ locations
                (\_ !t -> Tile.isOftenActor coTileSpeedup t)
                ds
        case mp of
          Just np -> do
            nps <- tryFind (np : ps) (n - 1)
            return $! np : nps
          Nothing -> return []
      -- Only consider deeper stairs to avoid leaderless spawners that stay near
      -- their starting stairs ambushing explorers that enter the level,
      -- unless the staircase has both sets of stairs.
      deeperStairs = (if fromEnum lid > 0 then fst else snd) lstair
  let !_A = assert (k > 0 && factionDist > 0) ()
      onStairs = reverse $ take k deeperStairs
      onEscapes = reverse $ take (k - length onStairs) lescape
      nk = k - length onStairs - length onEscapes
  -- Starting in the middle is too easy.
  found <- tryFind (middlePoint larea : onStairs ++ onEscapes) nk
  return $! onStairs ++ found ++ onEscapes

-- | Apply options that don't need a new game.
applyDebug :: MonadServer m => m ()
applyDebug = do
  ServerOptions{..} <- getsServer soptionsNxt
  modifyServer $ \ser ->
    ser {soptions = (soptions ser) { sniff
                                   , sallClear
                                   , sdbgMsgSer
                                   , snewGameSer
                                   , sassertExplored
                                   , sdumpInitRngs
                                   , sclientOptions }}
