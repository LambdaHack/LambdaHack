{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Operations for starting and restarting the game.
module Game.LambdaHack.Server.StartAction
  ( initConn, gameReset, reinitGame, initPer
  ) where

import Control.Arrow (second)
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action hiding (sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.Config
import qualified Game.LambdaHack.Server.DungeonGen as DungeonGen
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile

-- | Init connections, clients, debug and perception.
initConn :: (MonadAction m, MonadServerConn m)
         => DebugModeSer
         -> (FactionId -> Conn CmdClientUI -> IO ())
         -> (FactionId -> Conn CmdClientAI -> IO ())
         -> m ()
initConn sdebugNxt executorUI executorAI = do
  -- Set up connections.
  connServer
  -- Launch clients.
  launchClients executorUI executorAI
  -- Apply debug options that don't need a new game.
  modifyServer $ \ser ->
    ser {sdebugSer = (sdebugSer ser) { sniffIn = sniffIn sdebugNxt
                                     , sniffOut = sniffOut sdebugNxt
                                     , sallClear = sallClear sdebugNxt
                                     , stryFov = stryFov sdebugNxt }}
  -- Set up COps according to new debug.
  modifyState $ updateCOps $ speedupCOps (sallClear sdebugNxt)
  initPer

initPer :: MonadServer m => m ()
initPer = do
  cops <- getsState scops
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  let tryFov = stryFov $ sdebugSer ser
      fovMode = fromMaybe (configFovMode config) tryFov
      pers = dungeonPerception cops fovMode glo
  modifyServer $ \ser1 -> ser1 {sper = pers}

reinitGame :: MonadServer m => WriterT [Atomic] m ()
reinitGame = do
  Kind.COps{ coitem=Kind.Ops{okind}, corule } <- getsState scops
  pers <- getsServer sper
  knowMap <- getsServer $ sknowMap . sdebugSer
  -- This state is quite small, fit for transmition to the client.
  -- The biggest part is content, which really needs to be updated
  -- at this point to keep clients in sync with server improvements.
  fromGlobal <- getsState localFromGlobal
  glo <- getState
  let defLoc | knowMap = glo
             | otherwise = fromGlobal
  discoS <- getsServer sdisco
  let misteriousSymbols = ritemProject $ Kind.stdRuleset corule
      sdisco = let f ik = isymbol (okind ik) `notElem` misteriousSymbols
               in EM.filter f discoS
  broadcastCmdAtomic $ \fid -> RestartA fid sdisco (pers EM.! fid) defLoc
  populateDungeon
  broadcastSfxAtomic $ \fid -> FadeinD fid False

createFactions :: Kind.COps -> Config -> Rnd FactionDict
createFactions Kind.COps{ cofact=Kind.Ops{opick, okind}
                        , costrat=Kind.Ops{opick=sopick} } config = do
  let g isHuman (gname, fType) = do
        gkind <- opick fType (const True)
        let fk = okind gkind
            genemy = []  -- fixed below
            gally = []  -- fixed below
            gquit = Nothing
        gAiLeader <-
          if isHuman
          then return Nothing
          else fmap Just $ sopick (fAiLeader fk) (const True)
        gAiMember <- fmap Just $ sopick (fAiMember fk) (const True)
        let gleader = Nothing
        return Faction{..}
  lHuman <- mapM (g True) (configHuman config)
  lComputer <- mapM (g False) (configComputer config)
  let rawFs = zip [toEnum 1..] $ lHuman ++ lComputer
      isOfType fType fact =
        let fk = okind $ gkind fact
        in case lookup fType $ ffreq fk of
          Just n | n > 0 -> True
          _ -> False
      enemyAlly fact =
        let f fType = filter (isOfType fType . snd) rawFs
            fk = okind $ gkind fact
            setEnemy = ES.fromList $ map fst $ concatMap f $ fenemy fk
            setAlly  = ES.fromList $ map fst $ concatMap f $ fally fk
            genemy = ES.toList setEnemy
            gally = ES.toList $ setAlly ES.\\ setEnemy
        in fact {genemy, gally}
  return $! EM.fromDistinctAscList $ map (second enemyAlly) rawFs

gameReset :: (MonadAction m, MonadServer m) => Kind.COps -> m ()
gameReset cops@Kind.COps{coitem, corule} = do
  -- Rules config reloaded at each new game start.
  -- Taking the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  (sconfig, dungeonSeed, srandom) <- mkConfigRules corule
  let rnd :: Rnd (FactionDict, FlavourMap, Discovery, DiscoRev,
                  DungeonGen.FreshDungeon)
      rnd = do
        faction <- createFactions cops sconfig
        sflavour <- dungeonFlavourMap coitem
        (sdisco, sdiscoRev) <- serverDiscos coitem
        freshDng <- DungeonGen.dungeonGen cops sconfig
        return (faction, sflavour, sdisco, sdiscoRev, freshDng)
  let (faction, sflavour, sdisco, sdiscoRev, DungeonGen.FreshDungeon{..}) =
        St.evalState rnd dungeonSeed
      defState = defStateGlobal freshDungeon freshDepth faction cops
      defSer = emptyStateServer {sdisco, sdiscoRev, sflavour, srandom, sconfig}
  putState defState
  sdebugNxt <- getsServer sdebugNxt
  putServer defSer {sdebugNxt, sdebugSer = sdebugNxt}

-- Spawn initial actors. Clients should notice this, to set their leaders.
populateDungeon :: MonadServer m => WriterT [Atomic] m ()
populateDungeon = do
  cops@Kind.COps{ cotile
                , cofact=Kind.Ops{okind} } <- getsState scops
  let initialItems (lid, Level{ltile, litemNum}) =
        replicateM litemNum $ do
          pos <- rndToAction
                 $ findPos ltile (const (Tile.hasFeature cotile F.Boring))
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapM_ initialItems $ EM.assocs dungeon
  faction <- getsState sfaction
  config <- getsServer sconfig
  let heroNames = configHeroNames config : repeat []
      notSpawning (_, fact) = not $ isSpawningFact cops fact
      needInitialCrew = filter notSpawning $ EM.assocs faction
      getEntryLevel (_, fact) = fentry $ okind $ gkind fact
      arenas = ES.toList $ ES.fromList $ map getEntryLevel needInitialCrew
      initialHeroes arena = do
        lvl <- getsLevel arena id
        let arenaFactions = filter ((== arena) . getEntryLevel) needInitialCrew
        entryPoss <- rndToAction
                     $ findEntryPoss cops lvl (length arenaFactions)
        mapM_ (arenaHeroes arena) $ zip3 arenaFactions entryPoss heroNames
      arenaHeroes arena ((side, _), ppos, heroName) = do
        psFree <- getsState $ nearbyFreePoints cotile ppos arena
        let ps = take (1 + configExtraHeroes config) $ zip [1..] psFree
        laid <- forM ps $ \ (n, p) ->
          addHero side p arena heroName (Just n)
        mleader <- getsState $ gleader . (EM.! side) . sfaction
        when (mleader == Nothing) $
          tellCmdAtomic $ LeadFactionA side Nothing (Just $ head laid)
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
                , const (Tile.hasFeature cotile F.Walkable)
                ]
        nps <- tryFind (np : ps) (n - 1)
        return $ np : nps
      stairPoss = [fst lstair, snd lstair]
  in tryFind stairPoss k
