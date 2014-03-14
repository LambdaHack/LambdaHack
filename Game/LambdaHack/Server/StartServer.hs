-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartServer
  ( gameReset, reinitGame, initPer, applyDebug, initDebug
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.CommonServer
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.State

initPer :: MonadServer m => m ()
initPer = do
  cops <- getsState scops
  fovMode <- getsServer $ sfovMode . sdebugSer
  pers <- getsState $ dungeonPerception cops (fromMaybe (Digital 12) fovMode)
  modifyServer $ \ser1 -> ser1 {sper = pers}

reinitGame :: (MonadAtomic m, MonadServer m) => m ()
reinitGame = do
  Kind.COps{coitem=Kind.Ops{okind}, corule} <- getsState scops
  pers <- getsServer sper
  knowMap <- getsServer $ sknowMap . sdebugSer
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which really needs to be updated
  -- at this point to keep clients in sync with server improvements.
  fromGlobal <- getsState localFromGlobal
  s <- getState
  let defLoc | knowMap = s
             | otherwise = fromGlobal
  discoS <- getsServer sdisco
  let misteriousSymbols = ritemProject $ Kind.stdRuleset corule
      sdisco = let f ik = isymbol (okind ik) `notElem` misteriousSymbols
               in EM.filter f discoS
  sdebugCli <- getsServer $ sdebugCli . sdebugSer
  modeName <- getsServer $ sgameMode . sdebugSer
  broadcastUpdAtomic
    $ \fid -> UpdRestart fid sdisco (pers EM.! fid) defLoc sdebugCli modeName
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
            prefix | playerAiLeader = "Autonomous"
                   | otherwise = "Human"
            (gcolor, gname) = case M.lookup nameoc cmap of
              Nothing -> (Color.BrWhite, prefix <+> playerName)
              Just c -> (c, prefix <+> playerName <+> "Team")
        gkind <- fmap (fromMaybe $ assert `failure` playerFaction)
                 $ opick playerFaction (const True)
        let gdipl = EM.empty  -- fixed below
            gquit = Nothing
            gleader = Nothing
            gvictims = EM.empty
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
  scoreTable <- if sfrontendNo $ sdebugCli sdebug then
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
            automate p = p {playerAiLeader = True}
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
      defState = defStateGlobal freshDungeon freshDepth faction cops scoreTable
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
  let initialItems lid (Level{ltile, litemNum, lxsize, lysize}) =
        replicateM litemNum $ do
          Level{lfloor} <- getLevel lid
          pos <- rndToAction $ findPosTry 1000 ltile
                                 -- try really hard, for skirmish fairness
                   (const (Tile.hasFeature cotile F.CanItem))
                   [ \p _ -> all (flip EM.notMember lfloor)
                             $ vicinity lxsize lysize p
                   , \p _ -> EM.notMember p lfloor
                   ]
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapWithKeyM_ initialItems dungeon
  factionD <- getsState sfactionD
  sheroNames <- getsServer sheroNames
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "empty dungeon" `twith` dungeon
      needInitialCrew = filter ((> 0 ) . playerInitial . gplayer . snd)
                        $ EM.assocs factionD
      getEntryLevel (_, fact) =
        max minD $ min maxD $ playerEntry $ gplayer fact
      arenas = ES.toList $ ES.fromList $ map getEntryLevel needInitialCrew
      initialActors lid = do
        lvl <- getLevel lid
        let arenaFactions = filter ((== lid) . getEntryLevel) needInitialCrew
        entryPoss <- rndToAction
                     $ findEntryPoss cops lvl (length arenaFactions)
        mapM_ (arenaActors lid) $ zip arenaFactions entryPoss
      arenaActors _ ((_, Faction{gplayer = Player{playerInitial = 0}}), _) =
        return ()
      arenaActors lid ((side, fact), ppos) = do
        time <- getsState $ getLocalTime lid
        let nmult = fromEnum side `mod` 5  -- always positive
            ntime = timeAdd time (timeScale timeClip nmult)
            validTile t = Tile.hasFeature cotile F.CanActor t
        psFree <- getsState $ nearbyFreePoints cotile validTile ppos lid
        let ps = take (playerInitial $ gplayer fact) $ zip [0..] psFree
        forM_ ps $ \ (n, p) ->
          if not $ isHeroFact cops fact
          then spawnMonsters [p] lid ntime side
          else do
            let hNames = fromMaybe [] $ EM.lookup side sheroNames
            aid <- addHero side p lid hNames (Just n) ntime
            mleader <- getsState
                       $ gleader . (EM.! side) . sfactionD  -- just changed
            when (isNothing mleader) $
              execUpdAtomic $ UpdLeadFaction side Nothing (Just aid)
  mapM_ initialActors arenas

-- | Find starting postions for all factions. Try to make them distant
-- from each other. If only one faction, also move it away from any stairs.
findEntryPoss :: Kind.COps -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lysize, lstair} k = do
  let factionDist = max lxsize lysize - 5
      dist poss cmin l _ = all (\pos -> chessDist l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        np <- findPosTry 1000 ltile  -- try really hard, for skirmish fairness
                (const (Tile.hasFeature cotile F.CanActor))
                [ dist ps $ factionDist `div` 2
                , dist ps $ factionDist `div` 3
                , dist ps $ factionDist `div` 4
                , dist ps $ factionDist `div` 8
                , dist ps $ factionDist `div` 16
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

initDebug :: MonadReadState m => Kind.COps -> DebugModeSer -> m DebugModeSer
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
