-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartM
  ( initPer, reinitGame, gameReset, applyDebug
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , mapFromFuns, resetFactions, populateDungeon, findEntryPoss
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import           Data.Key (mapWithKeyM_)
import qualified Data.Map.Strict as M
import           Data.Ord
import qualified Data.Text as T
import           Data.Tuple (swap)
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Flavour
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
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
  ServerOptions{scurChalSer, sknowMap, sclientOptions} <- getsServer soptions
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which needs to be updated
  -- at this point to keep clients in sync with server improvements.
  s <- getState
  discoS <- getsState sdiscoKind
  -- Thanks to the following, for any item with not hidden identity,
  -- the client has its kind from the start.
  let discoKindFiltered =
        let f kindId = isNothing $ IK.getHideAs $ okind coitem kindId
        in EM.filter f discoS
      defL | sknowMap = s
           | otherwise = localFromGlobal s
      defLocal = updateDiscoKind (const discoKindFiltered) defL
  factionD <- getsState sfactionD
  modifyServer $ \ser -> ser {sclientStates = EM.map (const defLocal) factionD}
  let updRestart fid = UpdRestart fid (pers EM.! fid) defLocal
                                  scurChalSer sclientOptions
  mapWithKeyM_ (\fid _ -> execUpdAtomic $ updRestart fid) factionD
  dungeon <- getsState sdungeon
  let sactorTime = EM.map (const (EM.map (const EM.empty) dungeon)) factionD
  modifyServer $ \ser -> ser {sactorTime}
  populateDungeon
  mapM_ (\fid -> mapM_ (updatePer fid) (EM.keys dungeon))
        (EM.keys factionD)
  execSfxAtomic SfxSortSlots

mapFromFuns :: (Bounded a, Enum a, Ord b) => [a -> b] -> M.Map b a
mapFromFuns =
  let fromFun f m1 =
        let invAssocs = map (\c -> (f c, c)) [minBound..maxBound]
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
              mapFromFuns [colorToTeamName, colorToPlainName, colorToFancyName]
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
            gsha = EM.empty
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
          -> Maybe R.StdGen -> m State
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
#ifdef USE_BROWSER
  let startingModeGroup = "starting JS"
#else
  let startingModeGroup = "starting"
#endif
      gameMode = fromMaybe startingModeGroup
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
        freshDng <- DungeonGen.dungeonGen cops $ mcaves mode
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
populateDungeon :: MonadServerAtomic m => m ()
populateDungeon = do
  cops@COps{coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  factionD <- getsState sfactionD
  curChalSer <- getsServer $ scurChalSer . soptions
  let ginitialWolf fact1 = if cwolf curChalSer && fhasUI (gplayer fact1)
                           then case ginitial fact1 of
                             [] -> []
                             (ln, _, grp) : _ -> [(ln, 1, grp)]
                           else ginitial fact1
      (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> error $ "empty dungeon" `showFailure` dungeon
      -- Players that escape go first to be started over stairs, if possible.
      valuePlayer pl = (not $ fcanEscape pl, fname pl)
      -- Sorting, to keep games from similar game modes mutually reproducible.
      needInitialCrew = sortBy (comparing $ valuePlayer . gplayer . snd)
                        $ filter (not . null . ginitialWolf . snd)
                        $ EM.assocs factionD
      g (ln, _, _) = max minD . min maxD . toEnum $ ln
      getEntryLevels (_, fact) = map g $ ginitialWolf fact
      arenas = ES.toList $ ES.fromList
               $ concatMap getEntryLevels needInitialCrew
      hasActorsOnArena lid (_, fact) =
        any ((== lid) . g) $ ginitialWolf fact
      initialActorPositions lid = do
        lvl <- getLevel lid
        let arenaFactions = filter (hasActorsOnArena lid) needInitialCrew
            indexff (fid, _) = findIndex ((== fid) . fst) arenaFactions
            representsAlliance ff2@(fid2, fact2) =
              not $ any (\ff3@(fid3, _) ->
                           indexff ff3 < indexff ff2
                           && isFriend fid2 fact2 fid3) arenaFactions
            arenaAlliances = filter representsAlliance arenaFactions
        entryPoss <- rndToAction
                     $ findEntryPoss cops lid lvl (length arenaAlliances)
        let usedPoss = zip3 arenaAlliances entryPoss [0..]
        return $! (lid, usedPoss)
      initialActors (lid, usedPoss) = do
        let arenaFactions = filter (hasActorsOnArena lid) needInitialCrew
            placeAlliance ((fid3, _), ppos, timeOffset) =
              mapM_ (\(fid4, fact4) ->
                      when (isFriend fid4 fact4 fid3) $
                        placeActors lid ((fid4, fact4), ppos, timeOffset))
                    arenaFactions
        mapM_ placeAlliance usedPoss
      placeActors lid ((fid3, fact3), ppos, timeOffset) = do
        localTime <- getsState $ getLocalTime lid
        let clipInTurn = timeTurn `timeFit` timeClip
            nmult = 1 + timeOffset `mod` clipInTurn
            ntime = timeShift localTime (timeDeltaScale (Delta timeClip) nmult)
            validTile t = not $ Tile.isNoActor coTileSpeedup t
            initActors = ginitialWolf fact3
            initGroups = concat [ replicate n actorGroup
                                | ln3@(_, n, actorGroup) <- initActors
                                , g ln3 == lid ]
        psFree <- getsState $ nearbyFreePoints validTile ppos lid
        let ps = zip initGroups psFree
        forM_ ps $ \ (actorGroup, p) -> do
          maid <- addActorFromGroup actorGroup fid3 p lid ntime
          case maid of
            Nothing -> error $ "can't spawn initial actors"
                               `showFailure` (lid, (fid3, fact3))
            Just aid -> do
              mleader <- getsState $ gleader . (EM.! fid3) . sfactionD
              when (isNothing mleader) $ supplantLeader fid3 aid
              return True
  lposs <- mapM initialActorPositions arenas
  let alliancePositions =
        EM.fromList $ map (second $ map $ \(_, l, _) -> l) lposs
  placeItemsInDungeon alliancePositions
  embedItemsInDungeon
  mapM_ initialActors lposs

-- | Find starting postions for all factions. Try to make them distant
-- from each other. Place as many of the factions, as possible,
-- over stairs, starting from the end of the list, including placing the last
-- factions over escapes (we assume they are guardians of the escapes).
-- This implies the inital factions (if any) start far from escapes.
findEntryPoss :: COps -> LevelId -> Level -> Int -> Rnd [Point]
findEntryPoss COps{coTileSpeedup}
              lid Level{ltile, lxsize, lysize, lstair, lescape} k = do
  let factionDist = max lxsize lysize - 10
      dist poss cmin l _ = all (\pos -> chessDist l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        let ds = [ dist ps $ factionDist `div` 2
                 , dist ps $ factionDist `div` 3
                 , dist ps $ factionDist `div` 4
                 , dist ps $ factionDist `div` 6
                 ]
        np <- findPosTry2 1000 ltile  -- try really hard, for skirmish fairness
                (\_ t -> Tile.isWalkable coTileSpeedup t
                         && not (Tile.isNoActor coTileSpeedup t))
                ds
                (\_p t -> Tile.isOftenActor coTileSpeedup t)
                ds
        nps <- tryFind (np : ps) (n - 1)
        return $! np : nps
      -- Only consider deeper stairs to avoid leaderless spawners that lurk near
      -- their starting stairs ambushing explorers that enter the level,
      -- unless the staircase has both sets of stairs.
      deeperStairs = (if fromEnum lid > 0 then fst else snd) lstair
      middlePos = Point (lxsize `div` 2) (lysize `div` 2)
  let !_A = assert (k > 0 && factionDist > 0) ()
      onStairs = reverse $ take k $ lescape ++ deeperStairs
      nk = k - length onStairs
  -- Starting in the middle is too easy.
  found <- tryFind (middlePos : onStairs) nk
  return $! found ++ onStairs

-- | Apply options that don't need a new game.
applyDebug :: MonadServer m => m ()
applyDebug = do
  ServerOptions{..} <- getsServer soptionsNxt
  modifyServer $ \ser ->
    ser {soptions = (soptions ser) { sniff
                                   , sallClear
                                   , sdbgMsgSer
                                   , snewGameSer
                                   , sdumpInitRngs
                                   , sclientOptions }}
