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
import           Game.LambdaHack.Content.FactionKind
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
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

reinitGame :: MonadServerAtomic m => FactionDict -> m ()
reinitGame factionDold = do
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
  -- the client has its kind from the start. The client needs to know this
  -- to have a fast way (faster that looking for @PresentAs@ flag on a list)
  -- of determining whether an item kind is already identified
  -- or needs identification.
  let discoKindFiltered =
        let f kindId = isNothing $ IK.getMandatoryPresentAsFromKind
                                 $ okind coitem kindId
        in EM.filter f discoS
      defL | sknowMap = s
           | otherwise = localFromGlobal s
      defLocal = updateDiscoKind (const discoKindFiltered) defL
  factionD <- getsState sfactionD
  clientStatesOld <- getsServer sclientStates
  modifyServer $ \ser -> ser {sclientStates = EM.map (const defLocal) factionD}
  -- Some item kinds preserve their identity and flavour throughout
  -- the whole metagame, until the savefiles is removed.
  -- These are usually not man-made items, because these can be made
  -- in many flavours so it may be hard to recognize them.
  -- However, the exact properties of even natural items may vary,
  -- so the random aspects of items, stored in @sdiscoAspect@
  -- are not preserved (a lot of other state components would need
  -- to be partially preserved, too, both on server and clients).
  --
  -- This is a terrible temporary hack until Faction becomes content
  -- and we persistently store Faction information on the server.
  let metaHolder factionDict = case find (\(_, fact) ->
                                      fteam (gkind fact) == TeamContinuity 1)
                                    $ EM.assocs factionDict of
        Nothing ->
          -- This is a terrible hack as well. Monsters carry our gear memory
          -- if we are not in the game.
          fst <$> find (\(_, fact) -> fteam (gkind fact) == TeamContinuity 5)
                       (EM.assocs factionDict)
        Just (fid, _) -> Just fid
      mmetaHolderOld = metaHolder factionDold
  case mmetaHolderOld of
    Just metaHolderOld -> do
      let metaDiscoOld =
            let sOld = clientStatesOld EM.! metaHolderOld
                disco = sdiscoKind sOld
                inMetaGame kindId = IK.SetFlag Ability.MetaGame
                                    `elem` IK.iaspects (okind coitem kindId)
            in EM.filter inMetaGame disco
          defDiscoOld = updateDiscoKind (metaDiscoOld `EM.union`) defLocal
          metaHolderNew = fromJust $ metaHolder factionD
      modifyServer $ \ser ->
        ser {sclientStates = EM.insert metaHolderNew defDiscoOld
                             $ sclientStates ser}
    Nothing -> return ()  -- probably no previous games
  -- Hack ends.
  clientStatesNew <- getsServer sclientStates
  let updRestart fid = UpdRestart fid (pers EM.! fid) (clientStatesNew EM.! fid)
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
  Level{ldepth} <- getLevel minLid
  let regItem itemKindId = do
        let itemKind = okind coitem itemKindId
            freq = pure (itemKindId, itemKind)
        case possibleActorFactions itemKind factionD of
          [] -> return Nothing
          (fid, _) : _ -> do
            let c = CTrunk fid minLid originPoint
                jfid = Just fid
            m2 <- rollItemAspect freq ldepth
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
  Level{ldepth} <- getLevel minLid
  let regItem itemKindId = do
        let itemKind = okind coitem itemKindId
            freq = pure (itemKindId, itemKind)
            c = CFloor minLid originPoint
        m2 <- rollItemAspect freq ldepth
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

resetFactions :: ContentData FactionKind -> FactionDict -> ContentId ModeKind
              -> Int -> Dice.AbsDepth -> ModeKind -> Bool
              -> Rnd FactionDict
resetFactions cofact factionDold gameModeIdOld curDiffSerOld totalDepth mode
              automateAll = do
  let rawCreate (fid, (fkGroup, initialActors)) = do
        -- Validation of content guarantess the existence of such faction kind.
        gkindId <- fromJust <$> opick cofact fkGroup (const True)
        let gkind@FactionKind{..} = okind cofact gkindId
            castInitialActors (ln, d, actorGroup) = do
              n <- castDice (Dice.AbsDepth $ abs ln) totalDepth d
              return (ln, n, actorGroup)
        ginitial <- mapM castInitialActors initialActors
        let cmap =
              mapFromFuns Color.legalFgCol
                          [colorToTeamName, colorToPlainName, colorToFancyName]
            colorName = T.toLower $ head $ T.words fname
            prefix = case (fhasPointman, finitUnderAI) of
              (False, False) -> "Uncoordinated"
              (False, True) -> "Loose"
              (True, False) -> "Autonomous"
              (True, True) -> "Controlled"
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
            gdoctrine = finitDoctrine
            gunderAI = finitUnderAI || mattract mode || automateAll
            gdipl = EM.empty  -- fixed below
            gquit = Nothing
            _gleader = Nothing
            gvictims = EM.empty
            gvictimsD = gvictimsDnew
            gstash = Nothing
        return (fid, Faction{..})
  lFs <- mapM rawCreate $ zip [toEnum 1 ..] $ mroster mode
  let mkDipl diplMode =
        let f (ix1, ix2) =
              let adj1 fact = fact {gdipl = EM.insert ix2 diplMode (gdipl fact)}
              in EM.adjust adj1 ix1
        in foldr f
      -- Only symmetry is ensured, everything else is permitted,
      -- e.g., a faction in alliance with two others that are at war.
      pairsFromFaction :: (FactionKind -> [TeamContinuity])
                       -> (FactionId, Faction)
                       -> [(FactionId, FactionId)]
      pairsFromFaction selector (fid, fact) =
        let teams = selector $ gkind fact
            hasTeam team (_, fact2) = team == fteam (gkind fact2)
            pairsFromTeam team = case find (hasTeam team) lFs of
              Just (fid2, _) -> [(fid, fid2), (fid2, fid)]
              Nothing -> []
        in concatMap pairsFromTeam teams
      rawFs = EM.fromList lFs
      -- War overrides alliance, so 'warFs' second. Consequently, if a faction
      -- is allied with a faction that is at war with them, they will be
      -- symmetrically at war.
      allianceFs = mkDipl Alliance rawFs
                   $ concatMap (pairsFromFaction falliedTeams) $ EM.assocs rawFs
      warFs = mkDipl War allianceFs
              $ concatMap (pairsFromFaction fenemyTeams) $ EM.assocs allianceFs
  return $! warFs

gameReset :: MonadServer m
          => ServerOptions -> Maybe (GroupName ModeKind)
          -> Maybe SM.SMGen -> m State
gameReset serverOptions mGameMode mrandom = do
  -- Dungeon seed generation has to come first, to ensure item boosting
  -- is determined by the dungeon RNG.
  cops@COps{cofact, comode} <- getsState scops
  dungeonSeed <- getSetGen $ sdungeonRng serverOptions `mplus` mrandom
  srandom <- getSetGen $ smainRng serverOptions `mplus` mrandom
  let srngs = RNGs (Just dungeonSeed) (Just srandom)
  when (sdumpInitRngs serverOptions) $ dumpRngs srngs
  scoreTable <- restoreScore cops
  factionDold <- getsState sfactionD
  gameModeIdOld <- getsState sgameModeId
  teamGearOld <- getsServer steamGear
  flavourOld <- getsServer sflavour
  discoKindRevOld <- getsServer sdiscoKindRev
  clientStatesOld <- getsServer sclientStates
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
        flavour <- dungeonFlavourMap cops flavourOld
        (discoKind, sdiscoKindRev) <- serverDiscos cops discoKindRevOld
        freshDng <- DungeonGen.dungeonGen cops serverOptions $ mcaves mode
        factionD <- resetFactions cofact factionDold gameModeIdOld
                                  (cdiff curChalSer)
                                  (DungeonGen.freshTotalDepth freshDng)
                                  mode (sautomateAll serverOptions)
        return ( factionD, flavour, discoKind
               , sdiscoKindRev, freshDng, modeKindId )
  let ( factionD, sflavour, discoKind
       ,sdiscoKindRev, DungeonGen.FreshDungeon{..}, modeKindId ) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshTotalDepth
                                factionD cops scoreTable modeKindId discoKind
      defSer = emptyStateServer { srandom
                                , srngs }
  putServer defSer
  modifyServer $ \ser -> ser { steamGear = teamGearOld
                             , steamGearCur = teamGearOld
                             , sclientStates = clientStatesOld  -- reset later
                             , sdiscoKindRev
                             , sflavour }
  return $! defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: forall m. MonadServerAtomic m => m ()
populateDungeon = do
  cops@COps{coTileSpeedup} <- getsState scops
  factionD <- getsState sfactionD
  curChalSer <- getsServer $ scurChalSer . soptions
  let nGt0 (_, n, _) = n > 0
      ginitialWolf fact1 = if cwolf curChalSer && fhasUI (gkind fact1)
                           then case filter nGt0 $ ginitial fact1 of
                             [] -> []
                             (ln, _, grp) : _ -> [(ln, 1, grp)]
                           else ginitial fact1
      -- Keep the same order of factions as in roster.
      needInitialCrew = sortBy (comparing fst)
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
        entryPoss <- rndToAction $ findEntryPoss cops lvl (length arenaFactions)
        when (length entryPoss < length arenaFactions) $ debugPossiblyPrint
          "Server: populateDungeon: failed to find enough distinct faction starting positions; some factions share positions"
        let usedPoss = EM.fromList $ zip arenaFactions $ cycle entryPoss
        return (lid, usedPoss)
  factionPositions <- EM.fromDistinctAscList
                      <$> mapM initialActorPositions arenas
  let initialActors :: (FactionId, Faction) -> m ()
      initialActors (fid3, fact3) =
        mapM_ (placeActors fid3) $ ginitialWolf fact3
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
-- over stairs. Place the first faction(s) over escape(s)
-- (we assume they are guardians of the escapes).
-- This implies the inital factions (if any) start far from escapes.
findEntryPoss :: COps -> Level -> Int -> Rnd [Point]
findEntryPoss COps{cocave, coTileSpeedup}
              lvl@Level{lkind, larea, lstair, lescape}
              kRaw = do
  let lskip = CK.cskip $ okind cocave lkind
      k = kRaw + length lskip  -- if @lskip@ is bogus, will be too large; OK
      (_, xspan, yspan) = spanArea larea
      factionDist = max xspan yspan - 10
      dist !poss !cmin !l _ = all (\ !pos -> chessDist l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind !ps !n = do
        let ds = [ dist ps factionDist
                 , dist ps $ factionDist `div` 2
                 , dist ps $ factionDist `div` 3
                 , dist ps $ max 5 $ factionDist `div` 5
                 , dist ps $ max 2 $ factionDist `div` 10
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
      sameStaircase :: [Point] -> Point -> Bool
      sameStaircase upStairs Point{..} =
        any (\(Point ux uy) -> uy == py && ux + 2 == px) upStairs
      upAndSomeDownStairs =
        fst lstair
        ++ filter (not . sameStaircase (fst lstair)) (snd lstair)
      skipIndexes ixs l = map snd $ filter (\(ix, _) -> ix `notElem` ixs)
                                  $ zip [0..] l
  let !_A = assert (k > 0 && factionDist > 0) ()
      onEscapes = take k lescape
      onStairs = take (k - length onEscapes) upAndSomeDownStairs
      nk = k - length onEscapes - length onStairs
  -- Starting in the middle is too easy.
  found <- tryFind (middlePoint larea : onEscapes ++ onStairs) nk
  return $! skipIndexes lskip $ onEscapes ++ onStairs ++ found

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
