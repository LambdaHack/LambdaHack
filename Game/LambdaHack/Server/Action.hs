{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Server.Action
  ( -- * Action monads
    MonadServer( getServer, getsServer, putServer, modifyServer )
  , MonadServerChan
  , executorSer, tryRestore, connServer, launchClients
  , waitForChildren, speedupCOps
    -- * Communication
  , sendUpdateUI, sendQueryUI, sendUpdateCli, sendQueryCli
  , broadcastUI, funBroadcast
    -- * Assorted primitives
  , saveGameSer, saveGameBkp, dumpCfg, mkConfigRules, handleScores
  , rndToAction, resetFidPerception, getPerFid
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import System.Time
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (finally)
import qualified System.Random as R
import qualified Control.Monad.State as St
import Control.Concurrent.STM
import Control.Concurrent

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Server.Action.ActionClass (MonadServer(..), MonadServerChan(..))
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
import Game.LambdaHack.Random
import Game.LambdaHack.Level
import Game.LambdaHack.Time
import Game.LambdaHack.CmdSer
import Game.LambdaHack.CmdAtomic

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m ()
resetFidPerception fid arena = do
  cops <- getsState scops
  configFovMode <- getsServer (configFovMode . sconfig)
  sdebugSer <- getsServer sdebugSer
  lvl <- getsLevel arena id
  s <- getState
  let tryFov = stryFov sdebugSer
      fovMode = fromMaybe configFovMode tryFov
      per = levelPerception cops s fovMode fid arena lvl
      upd = EM.adjust (EM.adjust (const per) arena) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}

getPerFid :: MonadServer m => FactionId -> LevelId -> m Perception
getPerFid fid arena = do
  pers <- getsServer sper
  let fper = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` (arena, fid)) $ EM.lookup arena fper
  return $! per

saveGameSer :: MonadServer m => m ()
saveGameSer = do
  glo <- getState
  ser <- getServer
  config <- getsServer sconfig
  liftIO $ Save.saveGameSer config glo ser

-- | Save a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadServer m => m ()
saveGameBkp = do
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
handleScores :: (MonadActionAbort m, MonadServerChan m)
             => FactionId -> Bool -> Status -> Int -> m ()
handleScores _fid write status total =
  when (total /= 0) $ do
    config <- getsServer sconfig
-- TODO: sum over all levels?    time <- getsState getTime
    curDate <- liftIO getClockTime
    _slides <-
      liftIO $ register config write total timeZero curDate status
    go <- error "handleScores" -- sendQueryUI fid $ ShowSlidesUI slides
    when (not go) abort

connSendUpdateCli :: MonadServerChan m => CmdCli -> ConnCli CmdCli -> m ()
connSendUpdateCli cmd ConnCli{toClient} =
 liftIO $ atomically $ writeTQueue toClient cmd

sendUpdateCli :: MonadServerChan m => FactionId -> CmdCli -> m ()
sendUpdateCli fid cmd = do
  conn <- getsDict $ snd . (EM.! fid)
  maybe skip (connSendUpdateCli cmd) conn

connSendQueryCli :: MonadServerChan m => ActorId -> ConnCli CmdCli -> m CmdSer
connSendQueryCli aid conn@ConnCli{toClient, toServer} = do
  cmds <- liftIO $ atomically $ readTQueue toServer
  case cmds of
    [] -> do
      liftIO $ atomically $ writeTQueue toClient $ CmdHandleAICli aid
      connSendQueryCli aid conn
    cmd : rest -> do
      liftIO $ atomically $ unGetTQueue toServer rest
      return cmd

sendQueryCli :: MonadServerChan m => FactionId -> ActorId -> m CmdSer
sendQueryCli fid aid = do
  conn <- getsDict (snd . (EM.! fid))
  maybe (assert `failure` (fid, aid)) (connSendQueryCli aid) conn

funBroadcast :: MonadServerChan m => (FactionId -> CmdAtomic) -> m ()
funBroadcast fcmd = do
  faction <- getsState sfaction
  let f fid = do
        sendUpdateUI fid $ CmdAtomicUI $ fcmd fid
        sendUpdateCli fid $ CmdAtomicCli $ fcmd fid
  mapM_ f $ EM.keys faction

connSendUpdateUI :: MonadServerChan m => CmdUI -> ConnCli CmdUI -> m ()
connSendUpdateUI cmd ConnCli{toClient} =
  liftIO $ atomically $ writeTQueue toClient cmd

sendUpdateUI :: MonadServerChan m => FactionId -> CmdUI -> m ()
sendUpdateUI fid cmd = do
  conn <- getsDict (fst . (EM.! fid))
  maybe skip (connSendUpdateUI cmd) conn

connSendQueryUI :: MonadServerChan m => ActorId -> ConnCli CmdUI -> m CmdSer
connSendQueryUI aid conn@ConnCli{toClient, toServer} = do
  cmds <- liftIO $ atomically $ readTQueue toServer
  case cmds of
    [] -> do
      liftIO $ atomically $ writeTQueue toClient $ CmdHandleHumanUI aid
      connSendQueryUI aid conn
    cmd : rest -> do
      liftIO $ atomically $ unGetTQueue toServer rest
      return cmd

sendQueryUI :: MonadServerChan m => FactionId -> ActorId -> m CmdSer
sendQueryUI fid aid = do
  conn <- getsDict (fst . (EM.! fid))
  maybe (assert `failure` (fid, aid)) (connSendQueryUI aid) conn

broadcastUI :: MonadServerChan m => CmdUI -> m ()
broadcastUI cmd = do
  faction <- getsState sfaction
  mapM_ (flip sendUpdateUI cmd) $ EM.keys faction

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
connServer :: MonadServerChan m => m ()
connServer = do
  faction <- getsState sfaction
  -- Prepare connections based on factions.
  let mkConnCli :: IO (Maybe (ConnCli c))
      mkConnCli = do
        toClient <- newTQueueIO
        toServer <- newTQueueIO
        return $ Just $ ConnCli{..}
      addChan (fid, fact) = do
        chanUI <- if isHumanFact fact
                  then mkConnCli
                  else return Nothing
        chanCli <- mkConnCli
        return (fid, (chanUI, chanCli))
  chanAssocs <- liftIO $ mapM addChan $ EM.assocs faction
  putDict $ EM.fromDistinctAscList chanAssocs

-- | Connect to clients by starting them in spawned threads that read
-- and write directly to the channels.
launchClients :: MonadServerChan m
              => (FactionId -> ConnCli CmdUI -> IO ())
              -> (FactionId -> ConnCli CmdCli -> IO ())
              -> m ()
launchClients executorHuman executorComputer = do
  let forkClient (fid, (chanUI, chanCli)) = do
        let forkAI = case chanCli of
              -- TODO: for a screensaver, try True
              Just ch -> void $ forkChild $ executorComputer fid ch
              Nothing -> return ()
        case chanUI of
          Just ch -> do
            void $ forkChild $ executorHuman fid ch
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

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer $ \ser -> ser {srandom = ng}
  return a
