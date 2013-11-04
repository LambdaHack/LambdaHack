{-# LANGUAGE OverloadedStrings #-}
-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartAction
  ( applyDebug, gameReset, reinitGame, initPer
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.Action hiding (sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.Config
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ServerSem
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Assert

-- | Apply debug options that don't need a new game.
applyDebug :: MonadServer m => DebugModeSer -> m ()
applyDebug sdebugNxt =
  modifyServer $ \ser ->
    ser {sdebugSer = (sdebugSer ser) { sniffIn = sniffIn sdebugNxt
                                     , sniffOut = sniffOut sdebugNxt
                                     , sallClear = sallClear sdebugNxt
                                     , stryFov = stryFov sdebugNxt
                                     , sdbgMsgSer = sdbgMsgSer sdebugNxt }}

initPer :: MonadServer m => m ()
initPer = do
  cops <- getsState scops
  configFov <- fovMode
  pers <- getsState $ dungeonPerception cops configFov
  modifyServer $ \ser1 -> ser1 {sper = pers}

reinitGame :: (MonadAtomic m, MonadServer m) => m ()
reinitGame = do
  Kind.COps{ coitem=Kind.Ops{okind}, corule } <- getsState scops
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
  modeName <- getsServer smode
  broadcastCmdAtomic
    $ \fid -> RestartA fid sdisco (pers EM.! fid) defLoc sdebugCli modeName
  populateDungeon

mapFromInvFuns :: (Bounded a, Enum a, Ord b) => [a -> b] -> M.Map b a
mapFromInvFuns =
  let fromFun f m1 =
        let invAssocs = map (\c -> (f c, c)) [minBound..maxBound]
            m2 = M.fromList invAssocs
        in m2 `M.union` m1
  in foldr fromFun M.empty

lowercase :: Text -> Text
lowercase = T.pack . map Char.toLower . T.unpack

createFactions :: Kind.COps -> Players -> Rnd FactionDict
createFactions Kind.COps{cofact=Kind.Ops{opick}} players = do
  let rawCreate gplayer@Player{..} = do
        let cmap = mapFromInvFuns
                     [colorToTeamName, colorToPlainName, colorToFancyName]
            nameoc = lowercase playerName
            prefix | playerHuman = "Human"
                   | otherwise = "Autonomous"
            (gcolor, gname) = case M.lookup nameoc cmap of
              Nothing -> (Color.BrWhite, prefix <+> playerName)
              Just c -> (c, prefix <+> playerName <+> "Team")
        gkind <- opick playerFaction (const True)
        let gdipl = EM.empty  -- fixed below
            gquit = Nothing
            gleader = Nothing
        return Faction{..}
  lUI <- mapM rawCreate $ filter playerUI $ playersList players
  lnoUI <- mapM rawCreate $ filter (not . playerUI) $ playersList players
  let lFs = reverse (zip [toEnum (-1), toEnum (-2)..] lnoUI)  -- sorted
            ++ zip [toEnum 1..] lUI
      swapIx l =
        let ixs =
              let f (name1, name2) =
                    [ (ix1, ix2) | (ix1, fact1) <- lFs
                                 , playerName (gplayer fact1) == name1
                                 , (ix2, fact2) <- lFs
                                 , playerName (gplayer fact2) == name2]
              in concatMap f l
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
  return warFs

gameReset :: MonadServer m => Kind.COps -> m State
gameReset cops@Kind.COps{coitem, comode=Kind.Ops{opick, okind}, corule} = do
  -- Rules config reloaded at each new game start.
  -- Taking the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  (sconfig, dungeonSeed, srandom) <- mkConfigRules corule
  smode <- getsServer smode
  scoreTable <- restoreScore sconfig
  let rnd :: Rnd (FactionDict, FlavourMap, Discovery, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        modeKind <- opick smode (const True)
        let mode = okind modeKind
        faction <- createFactions cops $ mplayers mode
        sflavour <- dungeonFlavourMap coitem
        (sdisco, sdiscoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops $ mcaves mode
        return (faction, sflavour, sdisco, sdiscoRev, freshDng)
  let (faction, sflavour, sdisco, sdiscoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshDepth faction cops scoreTable
      defSer = emptyStateServer
                 {sdisco, sdiscoRev, sflavour, srandom, smode, sconfig}
  sdebugNxt <- getsServer sdebugNxt
  putServer defSer {sdebugNxt, sdebugSer = sdebugNxt}
  return defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: (MonadAtomic m, MonadServer m) => m ()
populateDungeon = do
  cops@Kind.COps{cotile} <- getsState scops
  let initialItems lid (Level{ltile, litemNum}) =
        replicateM litemNum $ do
          pos <- rndToAction
                 $ findPos ltile (const (Tile.hasFeature cotile F.CanItem))
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapWithKeyM_ initialItems dungeon
  factionD <- getsState sfactionD
  Config{configHeroNames} <- getsServer sconfig
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` dungeon
      needInitialCrew = EM.assocs factionD
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
      arenaActors lid ((side, fact@Faction{gplayer}), ppos) = do
        time <- getsState $ getLocalTime lid
        let nmult = fromEnum side `mod` 5  -- always positive
            ntime = timeAdd time (timeScale timeClip nmult)
        psFree <-
          getsState $ nearbyFreePoints
                        cotile (Tile.hasFeature cotile F.CanActor) ppos lid
        let ps = take (playerInitial gplayer) $ zip [0..] psFree
        forM_ ps $ \ (n, p) ->
          if isSpawnFact cops fact
          then spawnMonsters [p] lid ((== side) . fst) ntime "spawn"
          else do
            aid <- addHero side p lid configHeroNames (Just n) ntime
            mleader <- getsState $ gleader . (EM.! side) . sfactionD
            when (isNothing mleader) $
              execCmdAtomic $ LeadFactionA side Nothing (Just aid)
  mapM_ initialActors arenas

-- | Find starting postions for all factions. Try to make them distant
-- from each other. If only one faction, also move it away from any stairs.
findEntryPoss :: Kind.COps -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lysize, lstair} k =
  let factionDist = max lxsize lysize - 5
      dist poss cmin l _ =
        all (\pos -> chessDist lxsize l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        np <- findPosTry 40 ltile
                [ dist ps factionDist
                , dist ps $ 2 * factionDist `div` 3
                , dist ps $ factionDist `div` 2
                , dist ps $ factionDist `div` 3
                , dist ps $ factionDist `div` 4
                , dist ps $ factionDist `div` 6
                , const (Tile.hasFeature cotile F.CanActor)
                ]
        nps <- tryFind (np : ps) (n - 1)
        return $ np : nps
      stairPoss | k == 1 = [fst lstair, snd lstair]
                | otherwise = []
  in tryFind stairPoss k
