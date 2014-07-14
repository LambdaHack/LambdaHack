-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartServer
  ( gameReset, reinitGame, initPer, recruitActors, applyDebug, initDebug
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.CommonServer
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

initPer :: MonadServer m => m ()
initPer = do
  fovMode <- getsServer $ sfovMode . sdebugSer
  ser <- getServer
  pers <- getsState $ \s -> dungeonPerception (fromMaybe Digital fovMode) s ser
  modifyServer $ \ser1 -> ser1 {sper = pers}

reinitGame :: (MonadAtomic m, MonadServer m) => m ()
reinitGame = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  pers <- getsServer sper
  knowMap <- getsServer $ sknowMap . sdebugSer
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which needs to be updated
  -- at this point to keep clients in sync with server improvements.
  s <- getState
  let defLocal | knowMap = s
               | otherwise = localFromGlobal s
  discoS <- getsServer sdisco
  let sdisco = let f ik = Effect.Identified `elem` ifeature (okind ik)
               in EM.filter f discoS
  sdebugCli <- getsServer $ sdebugCli . sdebugSer
  modeName <- getsServer $ sgameMode . sdebugSer
  broadcastUpdAtomic
    $ \fid -> UpdRestart fid sdisco (pers EM.! fid) defLocal sdebugCli modeName
  populateDungeon

mapFromFuns :: (Bounded a, Enum a, Ord b) => [a -> b] -> M.Map b a
mapFromFuns =
  let fromFun f m1 =
        let invAssocs = map (\c -> (f c, c)) [minBound..maxBound]
            m2 = M.fromList invAssocs
        in m2 `M.union` m1
  in foldr fromFun M.empty

lowercase :: Text -> Text
lowercase = T.pack . map Char.toLower . T.unpack

createFactions :: Kind.COps -> Players -> Rnd FactionDict
createFactions Kind.COps{cofaction=Kind.Ops{opick}} players = do
  let rawCreate gplayer@Player{..} = do
        let cmap = mapFromFuns
                     [colorToTeamName, colorToPlainName, colorToFancyName]
            nameoc = lowercase playerName
            prefix | playerAI = "Autonomous"
                   | otherwise = "Controlled"
            (gcolor, gname) = case M.lookup nameoc cmap of
              Nothing -> (Color.BrWhite, prefix <+> playerName)
              Just c -> (c, prefix <+> playerName <+> "Team")
        gkind <- fmap (fromMaybe $ assert `failure` playerFaction)
                 $ opick playerFaction (const True)
        let gdipl = EM.empty  -- fixed below
            gquit = Nothing
            gleader = Nothing
            gvictims = EM.empty
            gsha = EM.empty
        return $! Faction{..}
  lUI <- mapM rawCreate $ filter playerUI $ playersList players
  lnoUI <- mapM rawCreate $ filter (not . playerUI) $ playersList players
  let lFs = reverse (zip [toEnum (-1), toEnum (-2)..] lnoUI)  -- sorted
            ++ zip [toEnum 1..] lUI
      swapIx l =
        let findPlayerName name = find ((name ==) . playerName . gplayer . snd)
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
      allianceFs = mkDipl Alliance rawFs (swapIx (playersAlly players))
      warFs = mkDipl War allianceFs (swapIx (playersEnemy players))
  return $! warFs

gameReset :: MonadServer m
          => Kind.COps -> DebugModeSer -> Maybe R.StdGen -> m State
gameReset cops@Kind.COps{coitem, comode=Kind.Ops{opick, okind}}
          sdebug mrandom = do
  dungeonSeed <- getSetGen $ sdungeonRng sdebug `mplus` mrandom
  srandom <- getSetGen $ smainRng sdebug `mplus` mrandom
  scoreTable <- if sfrontendNull $ sdebugCli sdebug then
                  return HighScore.empty
                else
                  restoreScore cops
  sstart <- getsServer sstart  -- copy over from previous game
  sallTime <- getsServer sallTime  -- copy over from previous game
  sheroNames <- getsServer sheroNames  -- copy over from previous game
  let smode = sgameMode sdebug
      rnd :: Rnd (FactionDict, FlavourMap, Discovery, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        modeKind <- fmap (fromMaybe $ assert `failure` smode)
                    $ opick smode (const True)
        let mode = okind modeKind
            automate p = p {playerAI = True}
            automatePS ps = ps {playersList = map automate $ playersList ps}
            players = if sautomateAll sdebug
                      then automatePS $ mplayers mode
                      else mplayers mode
        faction <- createFactions cops players
        sflavour <- dungeonFlavourMap coitem
        (sdisco, sdiscoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops $ mcaves mode
        return (faction, sflavour, sdisco, sdiscoRev, freshDng)
  let (faction, sflavour, sdisco, sdiscoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshTotalDepth
                                faction cops scoreTable
      defSer = emptyStateServer { sstart, sallTime, sheroNames, srandom
                                , srngs = RNGs (Just dungeonSeed)
                                               (Just srandom) }
  putServer defSer
  when (sbenchmark sdebug) resetGameStart
  modifyServer $ \ser -> ser {sdisco, sdiscoRev, sflavour}
  when (sdumpInitRngs sdebug) $ dumpRngs
  return $! defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: (MonadAtomic m, MonadServer m) => m ()
populateDungeon = do
  cops@Kind.COps{cotile} <- getsState scops
  placeItemsInDungeon
  dungeon <- getsState sdungeon
  factionD <- getsState sfactionD
  sheroNames <- getsServer sheroNames
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "empty dungeon" `twith` dungeon
      needInitialCrew = filter ((> 0 ) . playerInitial . gplayer . snd)
                        $ EM.assocs factionD
      getEntryLevel (_, fact) =
        max minD $ min maxD $ toEnum $ playerEntry $ gplayer fact
      arenas = ES.toList $ ES.fromList $ map getEntryLevel needInitialCrew
      initialActors lid = do
        lvl <- getLevel lid
        let arenaFactions = filter ((== lid) . getEntryLevel) needInitialCrew
            representsAlliance (fid2, fact2) =
              not $ any (\(fid3, _) -> fid3 < fid2
                                       && isAllied fact2 fid3) arenaFactions
            arenaAlliances = filter representsAlliance arenaFactions
            placeAlliance ((fid3, _), ppos) =
              mapM_ (\(fid4, fact4) ->
                      if isAllied fact4 fid3 || fid4 == fid3
                      then placeActors lid ((fid4, fact4), ppos)
                      else return ()) arenaFactions
        entryPoss <- rndToAction
                     $ findEntryPoss cops lvl (length arenaAlliances)
        mapM_ placeAlliance $ zip arenaAlliances entryPoss
      placeActors lid ((fid3, fact3), ppos) = do
        time <- getsState $ getLocalTime lid
        let nmult = 1 + fromEnum fid3 `mod` 4  -- always positive
            ntime = timeShift time (timeDeltaScale (Delta timeClip) nmult)
            validTile t = not $ Tile.hasFeature cotile F.NoActor t
        psFree <- getsState $ nearbyFreePoints validTile ppos lid
        let ps = take (playerInitial $ gplayer fact3) $ zip [0..] psFree
        forM_ ps $ \ (n, p) -> do
          go <-
            if not $ isHeroFact fact3
            then recruitActors [p] lid ntime fid3
            else do
              let hNames = fromMaybe [] $ EM.lookup fid3 sheroNames
              maid <- addHero fid3 p lid hNames (Just n) ntime
              case maid of
                Nothing -> return False
                Just aid -> do
                  mleader <- getsState $ gleader . (EM.! fid3) . sfactionD
                  when (isNothing mleader) $
                    execUpdAtomic $ UpdLeadFaction fid3 Nothing (Just aid)
                  return True
          unless go $ assert `failure` "can't spawn initial actors"
                             `twith` (lid, (fid3, fact3))
  mapM_ initialActors arenas

-- | Spawn actors of any specified faction, friendly or not.
-- To be used for initial dungeon population and for the summon effect.
recruitActors :: (MonadAtomic m, MonadServer m)
              => [Point] -> LevelId -> Time -> FactionId
              -> m Bool
recruitActors ps lid time fid = assert (not $ null ps) $ do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let spawnName = fname $ okind $ gkind fact
  laid <- forM ps $ \ p ->
    if isHeroFact fact
    then addHero fid p lid [] Nothing time
    else addMonster spawnName fid p lid time
  case catMaybes laid of
    [] -> return False
    aid : _ -> do
      mleader <- getsState $ gleader . (EM.! fid) . sfactionD  -- just changed
      when (isNothing mleader) $
        execUpdAtomic $ UpdLeadFaction fid Nothing (Just aid)
      return True

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => Text -> FactionId -> Point -> LevelId -> Time
           -> m (Maybe ActorId)
addMonster groupName bfid ppos lid time = do
  cops <- getsState scops
  fact <- getsState $ (EM.! bfid) . sfactionD
  pronoun <- if isCivilianFact cops fact
             then rndToAction $ oneOf ["he", "she"]
             else return "it"
  addActor groupName bfid ppos lid id pronoun time

-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => FactionId -> Point -> LevelId -> [(Int, (Text, Text))]
        -> Maybe Int -> Time
        -> m (Maybe ActorId)
addHero bfid ppos lid heroNames mNumber time = do
  Kind.COps{cofaction=Kind.Ops{okind=okind}} <- getsState scops
  Faction{gcolor, gplayer, gkind} <- getsState $ (EM.! bfid) . sfactionD
  let fName = fname $ okind gkind
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfid n) [0..9]
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
        in (playerName gplayer <+> nameN, pronounN)
      tweakBody b = b {bsymbol, bname, bcolor = gcolor}
  addActor fName bfid ppos lid tweakBody pronoun time

-- | Find starting postions for all factions. Try to make them distant
-- from each other. If only one faction, also move it away from any stairs.
findEntryPoss :: Kind.COps -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lysize, lstair} k = do
  let factionDist = max lxsize lysize - 5
      dist poss cmin l _ = all (\pos -> chessDist l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        np <- findPosTry 1000 ltile  -- try really hard, for skirmish fairness
                (\_ t -> Tile.isWalkable cotile t
                         && (not $ Tile.hasFeature cotile F.NoActor t))
                [ dist ps $ factionDist `div` 2
                , dist ps $ factionDist `div` 3
                , const (Tile.hasFeature cotile F.OftenActor)
                , dist ps $ factionDist `div` 3
                , dist ps $ factionDist `div` 4
                , dist ps $ factionDist `div` 5
                , dist ps $ factionDist `div` 7
                , dist ps $ factionDist `div` 10
                ]
        nps <- tryFind (np : ps) (n - 1)
        return $! np : nps
      stairPoss = fst lstair ++ snd lstair
      middlePos = Point (lxsize `div` 2) (lysize `div` 2)
  assert (k > 0 && factionDist > 0) skip
  case k of
    1 -> tryFind stairPoss k
    2 -> -- Make sure the first faction's pos is not chosen in the middle.
         tryFind [middlePos] k
    _ | k > 2 -> tryFind [] k
    _ -> assert `failure` k

initDebug :: MonadStateRead m => Kind.COps -> DebugModeSer -> m DebugModeSer
initDebug Kind.COps{corule} sdebugSer = do
  let stdRuleset = Kind.stdRuleset corule
  return $!
    (\dbg -> dbg {sfovMode =
        sfovMode dbg `mplus` Just (rfovMode stdRuleset)}) .
    (\dbg -> dbg {ssavePrefixSer =
        ssavePrefixSer dbg `mplus` Just (rsavePrefix stdRuleset)})
    $ sdebugSer

-- | Apply debug options that don't need a new game.
applyDebug :: MonadServer m => m ()
applyDebug = do
  DebugModeSer{..} <- getsServer sdebugNxt
  modifyServer $ \ser ->
    ser {sdebugSer = (sdebugSer ser) { sniffIn
                                     , sniffOut
                                     , sallClear
                                     , sfovMode
                                     , sstopAfter
                                     , sdbgMsgSer
                                     , snewGameSer
                                     , sdumpInitRngs
                                     , sdebugCli }}
