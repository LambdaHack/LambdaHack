{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServerRO( getServer, getsServer )
  , MonadServer( putServer, modifyServer )
  , MonadServerChan
  , executorSer, tryRestore, connServer, launchClients
  , waitForChildren, speedupCOps
    -- * Accessor to the Perception Reader
  , askPerceptionSer
    -- * Turn init operations
  , withPerception, remember
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg, mkConfigRules, handleScores
  , switchGlobalSelectedSide
  , sendUpdateUI, sendQueryUI
  , sendUpdateCli, sendQueryCli
  , broadcastUI, broadcastPosUI, funBroadcastUI
  , broadcastCli, broadcastPosCli, funBroadcastCli
  , withAI
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader.Class
import Data.Dynamic
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import System.Time
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (finally)
import qualified System.Random as R

import Game.LambdaHack.Action
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Server.Action.ActionClass (MonadServerRO(..), MonadServer(..), MonadServerChan(..))
import Game.LambdaHack.Server.Action.ActionType (executorSer)
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Server.Action.HighScore (register)
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Tile as Tile

-- | Update the cached perception for the selected level, for all factions,
-- for the given computation. The assumption is the level, and only the level,
-- has changed since the previous perception calculation.
withPerception :: MonadServerRO m => m a -> m a
withPerception m = do
  cops <- getsState scops
  configFovMode <- getsServer (configFovMode . sconfig)
  sdebugSer <- getsServer sdebugSer
  lvl <- getsState getArena
  arena <- getsState sarena
  let tryFov = stryFov sdebugSer
      fovMode = fromMaybe configFovMode tryFov
      per side = levelPerception cops fovMode side lvl
  local (EM.mapWithKey (\side lp -> EM.insert arena (per side) lp)) m

getPerFid :: MonadServerRO m => FactionId -> m Perception
getPerFid fid = do
  arena <- getsState sarena
  pers <- ask
  let fper = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup arena fper
  return $! per

-- | Get the current perception of the server.
askPerceptionSer :: MonadServerRO m => m Perception
askPerceptionSer = do
  side <- getsState sside
  getPerFid side

-- | Update all factions' memory of the current level.
--
-- This has to be strict wrt map operations or we leak one perception
-- per turn. This has to lazy wrt the perception sets or we compute them
-- for factions that do not move, perceive or not even reside on the level.
-- When clients and server communicate via network the communication
-- has to be explicitely lazy and multiple updates have to be collapsed
-- when sending is forced by the server asking a client to perceive
-- something or to act.
remember :: MonadServerChan m => m ()
remember = do
  arena <- getsState sarena
  lvl <- getsState getArena
  faction <- getsState sfaction
  itemD <- getsState sitem
  pers <- ask
  -- TODO: leaky! secret lvl sent
  let broadcast = funBroadcastCli (\fid ->
        RememberPerCli arena (pers EM.! fid EM.! arena) lvl itemD faction)
  broadcast
  withAI broadcast

saveGameSer :: MonadServerChan m => m ()
saveGameSer = do
--  broadcastCli [] $ GameSaveBkpCli False
--  withAI $ broadcastCli [] $ GameSaveBkpCli True
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameSer config glo ser

-- | Save a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadServerChan m => m ()
saveGameBkp = do
  broadcastCli [] $ GameSaveBkpCli False
  withAI $ broadcastCli [] $ GameSaveBkpCli True
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameBkpSer config glo ser

-- | Dumps the current game rules configuration to a file.
dumpCfg :: MonadServer m => FilePath -> m ()
dumpCfg fn = do
  config <- getsServer sconfig
  liftIO $ ConfigIO.dump config fn

-- TODO: show this for all humans and only humans.
-- | Handle current score and display it with the high scores.
-- Aborts if display of the scores was interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
handleScores :: MonadServerChan m => Bool -> Status -> Int -> m ()
handleScores write status total =
  when (total /= 0) $ do
    config <- getsServer sconfig
    time <- getsState getTime
    curDate <- liftIO getClockTime
    slides <-
      liftIO $ register config write total time curDate status
    side <- getsState sside
    go <- sendQueryUI side $ ShowSlidesCli slides
    when (not go) abort

switchGlobalSelectedSide :: MonadServer m => FactionId -> m ()
switchGlobalSelectedSide =
  modifyState . switchGlobalSelectedSideOnlyForGlobalState

withAI :: MonadServerChan m => m a -> m a
withAI m = do
  d <- getDict
  modifyDict $ EM.map $ \(_chanCli, chanAI) -> (chanAI, undefined)
  a <- m
  putDict d
  return a

isFactionAware :: MonadServerChan m => [Point] -> FactionId -> m Bool
isFactionAware poss fid = do
  per <- getPerFid fid
  let inter = ES.fromList poss `ES.intersection` totalVisible per
  return $! null poss || not (ES.null inter)

connSendUpdateCli :: MonadServerChan m => CmdUpdateCli -> ConnCli -> m ()
connSendUpdateCli cmd ConnCli {toClient} =
  liftIO $ writeChan toClient $ Left $ CmdUpdateCli cmd

sendUpdateCli :: MonadServerChan m => FactionId -> CmdUpdateCli -> m ()
sendUpdateCli fid cmd = do
  conn <- getsDict (fst . (EM.! fid))
  maybe (return ()) (connSendUpdateCli cmd) conn

connSendQueryCli :: (Typeable a, MonadServerChan m)
                 => CmdQueryCli a -> ConnCli
                 -> m a
connSendQueryCli cmd ConnCli{toClient, toServer} = do
  liftIO $ writeChan toClient $ Left $ CmdQueryCli cmd
  a <- liftIO $ readChan toServer
  return $ fromDyn a (assert `failure` (cmd, a))

sendQueryCli :: (Typeable a, MonadServerChan m)
             => FactionId -> CmdQueryCli a
             -> m a
sendQueryCli fid cmd = do
  conn <- getsDict (fst . (EM.! fid))
  maybe (assert `failure` (fid, cmd)) (connSendQueryCli cmd) conn

broadcastCli :: MonadServerChan m
             => [FactionId -> m Bool] -> CmdUpdateCli
             -> m ()
broadcastCli ps cmd = do
  faction <- getsState sfaction
  let p fid = do
        bs <- sequence $ map (\f -> f fid) ps
        return $! and bs
  ks <- filterM p $ EM.keys faction
  mapM_ (flip sendUpdateCli cmd) ks

broadcastPosCli :: MonadServerChan m => [Point] -> CmdUpdateCli -> m ()
broadcastPosCli poss = broadcastCli [isFactionAware poss]

funBroadcastCli :: MonadServerChan m => (FactionId -> CmdUpdateCli) -> m ()
funBroadcastCli cmd = do
  faction <- getsState sfaction
  let f fid = sendUpdateCli fid (cmd fid)
  mapM_ f $ EM.keys faction

connSendUpdateUI :: MonadServerChan m => CmdUpdateUI -> ConnCli -> m ()
connSendUpdateUI cmd ConnCli{toClient} =
  liftIO $ writeChan toClient $ Right $ CmdUpdateUI cmd

sendUpdateUI :: MonadServerChan m => FactionId -> CmdUpdateUI -> m ()
sendUpdateUI fid cmd = do
  conn <- getsDict (fst . (EM.! fid))
  maybe (return ()) (connSendUpdateUI cmd) conn

connSendQueryUI :: (Typeable a, MonadServerChan m)
                => CmdQueryUI a -> ConnCli
                -> m a
connSendQueryUI cmd ConnCli{toClient, toServer} = do
  liftIO $ writeChan toClient $ Right $ CmdQueryUI cmd
  a <- liftIO $ readChan toServer
  return $ fromDyn a (assert `failure` (cmd, a))

sendQueryUI :: (Typeable a, MonadServerChan m)
            => FactionId -> CmdQueryUI a
            -> m a
sendQueryUI fid cmd = do
  conn <- getsDict (fst . (EM.! fid))
  maybe (assert `failure` (fid, cmd)) (connSendQueryUI cmd) conn

broadcastUI :: MonadServerChan m
            => [FactionId -> m Bool] -> CmdUpdateUI
            -> m ()
broadcastUI ps cmd = do
  faction <- getsState sfaction
  let p fid = do
        bs <- sequence $ map (\f -> f fid) ps
        return $! and bs
  ks <- filterM p $ EM.keys faction
  mapM_ (flip sendUpdateUI cmd) ks

broadcastPosUI :: MonadServerChan m => [Point] -> CmdUpdateUI -> m ()
broadcastPosUI poss = broadcastUI [isFactionAware poss]

funBroadcastUI :: MonadServerChan m => (FactionId -> CmdUpdateUI) -> m ()
funBroadcastUI cmd = do
  faction <- getsState sfaction
  let f fid = sendUpdateUI fid (cmd fid)
  mapM_ f $ EM.keys faction

-- | Create a server config file. Warning: when it's use, the game state
-- may still be undefined, hence the content ops are given as an argument.
mkConfigRules :: MonadServer m
              => Kind.Ops RuleKind -> m (Config, R.StdGen, R.StdGen)
mkConfigRules = liftIO . ConfigIO.mkConfigRules

tryRestore :: MonadServer m
           => Kind.COps -> m (Either (State, StateServer, Msg) Msg)
tryRestore Kind.COps{corule} = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  -- A throw-away copy of rules config, to be used until the old
  -- version of the config can be read from the savefile.
  (sconfig, _, _) <- mkConfigRules corule
  liftIO $ Save.restoreGameSer sconfig pathsDataFile title

-- | Prepare connections based on factions.
connServer :: MonadServerChan m =>m ()
connServer = do
  faction <- getsState sfaction
  -- Prepare connections based on factions.
  let mkConnCli = do
        toClient <- newChan
        toServer <- newChan
        return $ Just $ ConnCli{..}
      addChan (fid, fact) = do
        chanCli <- if isHumanFact fact
                   then mkConnCli
                   else return Nothing
        chanAI <- mkConnCli
        return (fid, (chanCli, chanAI))
  chanAssocs <- liftIO $ mapM addChan $ EM.assocs faction
  putDict $ EM.fromAscList chanAssocs

-- | Connect to clients by starting them in spawned threads that read
-- and write directly to the channels.
launchClients :: MonadServerChan m
              => (FactionId -> ConnCli -> Bool -> IO ())
              -> m ()
launchClients executorC = do
  let forkClient (fid, (chanCli, chanAI)) = do
        let forkAI = case chanAI of
              -- TODO: for a screensaver, try True
              Just ch -> void $ forkChild $ executorC fid ch False
              Nothing -> return ()
        case chanCli of
          Just ch -> do
            void $ forkChild $ executorC fid ch True
            forkAI
          Nothing ->
            forkAI
  d <- getDict
  liftIO $ mapM_ forkClient $ EM.assocs d

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.6.0.0/Control-Concurrent.html
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkIO (io `finally` putMVar mvar ())
-- 7.6  forkFinally io (\_ -> putMVar mvar ())

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code. Also, evaluate content
-- to check consistency.
speedupCOps :: Kind.COps -> Kind.COps
speedupCOps !copsSlow@Kind.COps{cotile=tile} =
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
  in copsSlow {Kind.cotile}
