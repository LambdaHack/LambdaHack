-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartM
  ( gameReset, reinitGame, updatePer, initPer, recruitActors, applyDebug
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import Data.Key (mapWithKeyM_)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.HighScore as HighScore
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
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

initPer :: MonadServer m => m ()
initPer = do
  discoAspect <- getsServer sdiscoAspect
  ( sactorAspect, sfovLitLid, sfovClearLid, sfovLucidLid
   ,sperValidFid, sperCacheFid, sperFid )
    <- getsState $ perFidInDungeon discoAspect
  modifyServer $ \ser ->
    ser { sactorAspect, sfovLitLid, sfovClearLid, sfovLucidLid
        , sperValidFid, sperCacheFid, sperFid }

reinitGame :: (MonadAtomic m, MonadServer m) => m ()
reinitGame = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  pers <- getsServer sperFid
  DebugModeSer{scurDiffSer, sknowMap, sdebugCli} <- getsServer sdebugSer
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which needs to be updated
  -- at this point to keep clients in sync with server improvements.
  s <- getState
  let defLocal | sknowMap = s
               | otherwise = localFromGlobal s
  discoS <- getsServer sdiscoKind
  let sdiscoKind =
        let f KindMean{kmKind} = IK.Identified `elem` IK.ifeature (okind kmKind)
        in EM.filter f discoS
      updRestart fid = UpdRestart fid sdiscoKind (pers EM.! fid) defLocal
                                  scurDiffSer sdebugCli
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execUpdAtomic $ updRestart fid) factionD
  dungeon <- getsState sdungeon
  let sactorTime = EM.map (const (EM.map (const EM.empty) dungeon)) factionD
  modifyServer $ \ser -> ser {sactorTime}
  populateDungeon
  mapM_ (\fid -> mapM_ (\lid -> updatePer fid lid) (EM.keys dungeon))
        (EM.keys factionD)
  execSfxAtomic $ SfxMsgAll "SortSlots"  -- hack

updatePer :: (MonadAtomic m, MonadServer m) => FactionId -> LevelId -> m ()
{-# INLINE updatePer #-}
updatePer fid lid = do
  modifyServer $ \ser ->
    ser {sperValidFid = EM.adjust (EM.insert lid True) fid $ sperValidFid ser}
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! fid EM.! lid
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  -- Performed in the State after action, e.g., with a new actor.
  perNew <- recomputeCachePer fid lid
  let inPer = diffPer perNew perOld
      outPer = diffPer perOld perNew
  unless (nullPer outPer && nullPer inPer) $ do
    unless knowEvents $  -- inconsistencies would quickly manifest
      execSendPer fid lid outPer inPer perNew

mapFromFuns :: (Bounded a, Enum a, Ord b) => [a -> b] -> M.Map b a
mapFromFuns =
  let fromFun f m1 =
        let invAssocs = map (\c -> (f c, c)) [minBound..maxBound]
            m2 = M.fromList invAssocs
        in m2 `M.union` m1
  in foldr fromFun M.empty

resetFactions :: FactionDict -> Kind.Id ModeKind -> Int -> AbsDepth -> Roster
              -> Rnd FactionDict
resetFactions factionDold gameModeIdOld curDiffSerOld totalDepth players = do
  let rawCreate Player{..} = do
        let castInitialActor (ln, d, actorGroup) = do
              n <- castDice (AbsDepth $ abs ln) totalDepth d
              return (ln, n, actorGroup)
        initialActors <- mapM castInitialActor finitialActors
        let gplayer = Player{finitialActors = initialActors, ..}
            cmap = mapFromFuns
                     [colorToTeamName, colorToPlainName, colorToFancyName]
            nameoc = T.toLower $ head $ T.words fname
            prefix = case fleaderMode of
              LeaderNull -> "Loose"
              LeaderAI _ -> "Autonomous"
              LeaderUI _ -> "Controlled"
            (gcolor, gnameNew) = case M.lookup nameoc cmap of
              Nothing -> (Color.BrWhite, prefix <+> fname)
              Just c -> (c, prefix <+> fname <+> "Team")
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
            gleader = Nothing
            gvictims = EM.empty
            gvictimsD = gvictimsDnew
            gsha = EM.empty
        return $! Faction{..}
  lUI <- mapM rawCreate $ filter fhasUI $ rosterList players
  let !_A = assert (length lUI <= 1
                    `blame` "currently, at most one faction may have a UI"
                    `twith` lUI) ()
  lnoUI <- mapM rawCreate $ filter (not . fhasUI) $ rosterList players
  let lFs = reverse (zip [toEnum (-1), toEnum (-2)..] lnoUI)  -- sorted
            ++ zip [toEnum 1..] lUI
      swapIx l =
        let findPlayerName name = find ((name ==) . fname . gplayer . snd)
            f (name1, name2) =
              case (findPlayerName name1 lFs, findPlayerName name2 lFs) of
                (Just (ix1, _), Just (ix2, _)) -> (ix1, ix2)
                _ -> assert `failure` "unknown faction"
                            `twith` ((name1, name2), lFs)
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
          => Kind.COps -> DebugModeSer -> Maybe (GroupName ModeKind)
          -> Maybe R.StdGen -> m State
gameReset cops@Kind.COps{comode=Kind.Ops{opick, okind}}
          sdebug mGameMode mrandom = do
  dungeonSeed <- getSetGen $ sdungeonRng sdebug `mplus` mrandom
  srandom <- getSetGen $ smainRng sdebug `mplus` mrandom
  let srngs = RNGs (Just dungeonSeed) (Just srandom)
  when (sdumpInitRngs sdebug) $ dumpRngs srngs
  scoreTable <- if sfrontendNull $ sdebugCli sdebug then
                  return HighScore.empty
                else
                  restoreScore cops
  sheroNames <- getsServer sheroNames  -- copy over from previous game
  factionDold <- getsState sfactionD
  gameModeIdOld <- getsState sgameModeId
  DebugModeSer{scurDiffSer} <- getsServer sdebugSer
#ifdef USE_BROWSER
  let startingModeGroup = "starting JS"
#else
  let startingModeGroup = "starting"
#endif
      gameMode = fromMaybe startingModeGroup
                 $ mGameMode `mplus` sgameMode sdebug
      rnd :: Rnd (FactionDict, FlavourMap, DiscoveryKind, DiscoveryKindRev,
                  DungeonGen.FreshDungeon, Kind.Id ModeKind)
      rnd = do
        modeKindId <- fromMaybe (assert `failure` gameMode)
                      <$> opick gameMode (const True)
        let mode = okind modeKindId
            automatePS ps = ps {rosterList =
                                  map (automatePlayer True) $ rosterList ps}
            players = if sautomateAll sdebug
                      then automatePS $ mroster mode
                      else mroster mode
        sflavour <- dungeonFlavourMap cops
        (sdiscoKind, sdiscoKindRev) <- serverDiscos cops
        freshDng <- DungeonGen.dungeonGen cops $ mcaves mode
        factionD <- resetFactions factionDold gameModeIdOld scurDiffSer
                                  (DungeonGen.freshTotalDepth freshDng)
                                  players
        return ( factionD, sflavour, sdiscoKind
               , sdiscoKindRev, freshDng, modeKindId )
  let ( factionD, sflavour, sdiscoKind
       ,sdiscoKindRev, DungeonGen.FreshDungeon{..}, modeKindId ) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshTotalDepth
                                factionD cops scoreTable modeKindId
      defSer = emptyStateServer { sheroNames
                                , srandom
                                , srngs }
  putServer defSer
  modifyServer $ \ser -> ser {sdiscoKind, sdiscoKindRev, sflavour}
  return $! defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: (MonadAtomic m, MonadServer m) => m ()
populateDungeon = do
  cops@Kind.COps{coTileSpeedup} <- getsState scops
  placeItemsInDungeon
  embedItemsInDungeon
  dungeon <- getsState sdungeon
  factionD <- getsState sfactionD
  sheroNames <- getsServer sheroNames
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "empty dungeon" `twith` dungeon
      -- Players that escape go first to be started over stairs, if possible.
      valuePlayer pl = (not $ fcanEscape pl, fname pl)
      -- Sorting, to keep games from similar game modes mutually reproducible.
      needInitialCrew = sortBy (comparing $ valuePlayer . gplayer . snd)
                        $ filter (not . null . finitialActors . gplayer . snd)
                        $ EM.assocs factionD
      g (ln, _, _) = max minD . min maxD . toEnum $ ln
      getEntryLevels (_, fact) = map g $ finitialActors $ gplayer fact
      arenas = ES.toList $ ES.fromList
               $ concatMap getEntryLevels needInitialCrew
      hasActorsOnArena lid (_, fact) =
        any ((== lid) . g) $ finitialActors $ gplayer fact
      initialActors lid = do
        lvl <- getLevel lid
        let arenaFactions = filter (hasActorsOnArena lid) needInitialCrew
            indexff (fid, _) = findIndex ((== fid) . fst) arenaFactions
            representsAlliance ff2@(_, fact2) =
              not $ any (\ff3@(fid3, _) ->
                           indexff ff3 < indexff ff2
                           && isAllied fact2 fid3) arenaFactions
            arenaAlliances = filter representsAlliance arenaFactions
            placeAlliance ((fid3, _), ppos, timeOffset) =
              mapM_ (\(fid4, fact4) ->
                      when (isAllied fact4 fid3 || fid4 == fid3) $
                        placeActors lid ((fid4, fact4), ppos, timeOffset))
                    arenaFactions
        entryPoss <- rndToAction
                     $ findEntryPoss cops lid lvl (length arenaAlliances)
        mapM_ placeAlliance $ zip3 arenaAlliances entryPoss [0..]
      placeActors lid ((fid3, fact3), ppos, timeOffset) = do
        localTime <- getsState $ getLocalTime lid
        let clipInTurn = timeTurn `timeFit` timeClip
            nmult = 1 + timeOffset `mod` clipInTurn
            ntime = timeShift localTime (timeDeltaScale (Delta timeClip) nmult)
            validTile t = not $ Tile.isNoActor coTileSpeedup t
            initActors = finitialActors $ gplayer fact3
            initGroups = concat [ replicate n actorGroup
                                | ln3@(_, n, actorGroup) <- initActors
                                , g ln3 == lid ]
        psFree <- getsState $ nearbyFreePoints validTile ppos lid
        let ps = zip3 initGroups [0..] psFree
        forM_ ps $ \ (actorGroup, n, p) -> do
          go <-
            if not $ fhasNumbers $ gplayer fact3
            then recruitActors actorGroup [p] lid ntime fid3
            else do
              let hNames = EM.findWithDefault [] fid3 sheroNames
              maid <- addHero actorGroup fid3 p lid hNames (Just n) ntime
              case maid of
                Nothing -> return False
                Just aid -> do
                  mleader <- getsState $ gleader . (EM.! fid3) . sfactionD
                  when (isNothing mleader) $ supplantLeader fid3 aid
                  return True
          unless go $ assert `failure` "can't spawn initial actors"
                             `twith` (lid, (fid3, fact3))
  mapM_ initialActors arenas

-- | Spawn actors of any specified faction, friendly or not.
-- To be used for initial dungeon population and for the summon effect.
recruitActors :: (MonadAtomic m, MonadServer m)
              => GroupName ItemKind -> [Point] -> LevelId -> Time -> FactionId
              -> m Bool
recruitActors actorGroup ps lid time fid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  laid <- forM ps $ \ p ->
    if fhasNumbers $ gplayer fact
    then addHero actorGroup fid p lid [] Nothing time
    else addMonster actorGroup fid p lid time
  case catMaybes laid of
    [] -> return False
    aid : _ -> do
      mleader <- getsState $ gleader . (EM.! fid) . sfactionD  -- just changed
      when (isNothing mleader) $ supplantLeader fid aid
      return True

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => GroupName ItemKind -> FactionId -> Point -> LevelId -> Time
           -> m (Maybe ActorId)
addMonster groupName bfid ppos lid time = do
  fact <- getsState $ (EM.! bfid) . sfactionD
  pronoun <- if fhasGender $ gplayer fact
             then rndToAction $ oneOf ["he", "she"]
             else return "it"
  addActor groupName bfid ppos lid id pronoun time

-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => GroupName ItemKind -> FactionId -> Point -> LevelId
        -> [(Int, (Text, Text))]-> Maybe Int -> Time
        -> m (Maybe ActorId)
addHero actorGroup bfid ppos lid heroNames mNumber time = do
  Faction{gcolor, gplayer} <- getsState $ (EM.! bfid) . sfactionD
  mhs <- mapM (getsState . tryFindHeroK bfid) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe (fromMaybe 100 freeHeroK) mNumber
      bsymbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      nameFromNumber 0 = ("Captain", "he")
      nameFromNumber k | k `mod` 7 == 0 = ("Heroine" <+> tshow k, "she")
      nameFromNumber k = ("Hero" <+> tshow k, "he")
      (bname, pronoun) | gcolor == Color.BrWhite =
        fromMaybe (nameFromNumber n) $ lookup n heroNames
                       | otherwise =
        let (nameN, pronounN) = nameFromNumber n
        in (fname gplayer <+> nameN, pronounN)
      tweakBody b = b {bsymbol, bname, bcolor = gcolor}
  addActor actorGroup bfid ppos lid tweakBody pronoun time

-- | Find starting postions for all factions. Try to make them distant
-- from each other. Place as many of the factions, as possible,
-- over stairs, starting from the end of the list, including placing the last
-- factions over escapes (we assume they are guardians of the escapes).
-- This implies the inital factions (if any) start far from escapes.
findEntryPoss :: Kind.COps -> LevelId -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{coTileSpeedup}
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
      -- Prefer deeper stairs to avoid spawners ambushing explorers.
      (deeperStairs, shallowerStairs) =
        (if fromEnum lid > 0 then id else swap) lstair
      stairPoss = if length deeperStairs > length shallowerStairs
                  then deeperStairs
                  else shallowerStairs
      middlePos = Point (lxsize `div` 2) (lysize `div` 2)
  let !_A = assert (k > 0 && factionDist > 0) ()
      onStairs = reverse $ take k $ lescape ++ stairPoss
      nk = k - length onStairs
  -- Starting in the middle is too easy.
  found <- tryFind ([middlePos] ++ onStairs) nk
  return $! found ++ onStairs

-- | Apply debug options that don't need a new game.
applyDebug :: MonadServer m => m ()
applyDebug = do
  DebugModeSer{..} <- getsServer sdebugNxt
  modifyServer $ \ser ->
    ser {sdebugSer = (sdebugSer ser) { sniffIn
                                     , sniffOut
                                     , sallClear
                                     , sdbgMsgSer
                                     , snewGameSer
                                     , sdumpInitRngs
                                     , sdebugCli }}
