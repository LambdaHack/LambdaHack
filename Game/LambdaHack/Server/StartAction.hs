{-# LANGUAGE OverloadedStrings #-}
-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartAction
  ( applyDebug, gameReset, reinitGame, initPer
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Tuple (swap)

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
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
                                     , stryFov = stryFov sdebugNxt }}

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
  broadcastCmdAtomic
    $ \fid -> RestartA fid sdisco (pers EM.! fid) defLoc sdebugCli
  populateDungeon

createFactions :: Kind.COps -> Players -> Rnd FactionDict
createFactions Kind.COps{ cofact=Kind.Ops{opick, okind}
                        , costrat=Kind.Ops{opick=sopick} } players = do
  let rawCreate isHuman (gname, fType) gcolor = do
        gkind <- opick fType (const True)
        let fk = okind gkind
            gdipl = EM.empty  -- fixed below
            gquit = Nothing
        gAiLeader <-
          if isHuman
          then return Nothing
          else fmap Just $ sopick (fAiLeader fk) (const True)
        gAiMember <- fmap Just $ sopick (fAiMember fk) (const True)
        let gleader = Nothing
        return Faction{..}
      actorColors = cycle Color.brightCol
      humanColors = [Color.BrWhite] ++ actorColors
      computerColors = drop (length (playersHuman players) - 1) actorColors
  lHuman <-
    zipWithM (rawCreate True) (playersHuman players) humanColors
  lComputer <-
    zipWithM (rawCreate False) (playersComputer players) computerColors
  let lFs = reverse (zip [toEnum (-1), toEnum (-2)..] lComputer)  -- sorted
            ++ zip [toEnum 1..] lHuman
      swapIx l =
        let ixs =
              let f (name1, name2) =
                    [ (ix1, ix2) | (ix1, fact1) <- lFs, gname fact1 == name1
                                 , (ix2, fact2) <- lFs, gname fact2 == name2]
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

gameReset :: MonadServer m => Kind.COps -> Text -> m State
gameReset cops@Kind.COps{coitem, corule} t = do
  -- Rules config reloaded at each new game start.
  -- Taking the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  (sconfig, dungeonSeed, srandom) <- mkConfigRules corule
  scoreTable <- restoreScore sconfig
  let rnd :: Rnd (FactionDict, FlavourMap, Discovery, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        let scenario = case M.lookup t $ configScenario sconfig of
              Just pl -> pl
              Nothing -> assert `failure` "no scenario configuration:" <+> t
            dng = scenarioDungeon scenario
            caves = case M.lookup dng $ configCaves sconfig of
              Just cv -> cv
              Nothing -> assert `failure` "no caves configuration:" <+> dng
            plr = scenarioPlayers scenario
            players = case M.lookup plr $ configPlayers sconfig of
              Just pl -> pl
              Nothing -> assert `failure` "no players configuration:" <+> plr
        faction <- createFactions cops players
        sflavour <- dungeonFlavourMap coitem
        (sdisco, sdiscoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops caves
        return (faction, sflavour, sdisco, sdiscoRev, freshDng)
  let (faction, sflavour, sdisco, sdiscoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshDepth faction cops scoreTable
      defSer = emptyStateServer {sdisco, sdiscoRev, sflavour, srandom, sconfig}
  sdebugNxt <- getsServer sdebugNxt
  putServer defSer {sdebugNxt, sdebugSer = sdebugNxt}
  return defState

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: (MonadAtomic m, MonadServer m) => m ()
populateDungeon = do
  cops@Kind.COps{ cotile
                , cofact=Kind.Ops{okind} } <- getsState scops
  let initialItems lid (Level{ltile, litemNum}) =
        replicateM litemNum $ do
          pos <- rndToAction
                 $ findPos ltile (const (Tile.hasFeature cotile F.Boring))
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapWithKeyM_ initialItems dungeon
  factionD <- getsState sfactionD
  config <- getsServer sconfig
  let heroNames = configHeroNames config : repeat []
      notSpawning (_, fact) = not $ isSpawningFact cops fact
      needInitialCrew = filter notSpawning $ EM.assocs factionD
      getEntryLevel (_, fact) = fentry $ okind $ gkind fact
      arenas = ES.toList $ ES.fromList $ map getEntryLevel needInitialCrew
      initialHeroes lid = do
        lvl <- getsLevel lid id
        let arenaFactions = filter ((== lid) . getEntryLevel) needInitialCrew
        entryPoss <- rndToAction
                     $ findEntryPoss cops lvl (length arenaFactions)
        mapM_ (arenaHeroes lid) $ zip3 arenaFactions entryPoss heroNames
      arenaHeroes lid ((side, _), ppos, heroName) = do
        psFree <-
          getsState
          $ nearbyFreePoints cotile (Tile.hasFeature cotile F.Boring) ppos lid
        let ps = take (1 + configExtraHeroes config) $ zip [0..] psFree
        laid <- forM ps $ \ (n, p) ->
          addHero side p lid heroName (Just n)
        mleader <- getsState $ gleader . (EM.! side) . sfactionD
        when (mleader == Nothing) $
          execCmdAtomic $ LeadFactionA side Nothing (Just $ head laid)
  mapM_ initialHeroes arenas

-- | Find starting postions for all factions. Try to make them distant
-- from each other and from any stairs.
findEntryPoss :: Kind.COps -> Level -> Int -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lstair} k =
  let cminStairDist = chessDist lxsize (fst lstair) (snd lstair)
      dist poss cmin l _ =
        all (\pos -> chessDist lxsize l pos > cmin) poss
      tryFind _ 0 = return []
      tryFind ps n = do
        np <- findPosTry 20 ltile  -- 20 only, for unpredictability
                [ dist ps $ 2 * cminStairDist
                , dist ps cminStairDist
                , dist ps $ cminStairDist `div` 2
                , dist ps $ cminStairDist `div` 4
                , const (Tile.hasFeature cotile F.Boring)
                ]
        nps <- tryFind (np : ps) (n - 1)
        return $ np : nps
      stairPoss = [fst lstair, snd lstair]
  in tryFind stairPoss k
