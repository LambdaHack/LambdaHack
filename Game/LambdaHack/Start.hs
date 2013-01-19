{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Spwaning client threads and starting up the server.
module Game.LambdaHack.Start
  ( startFrontend
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe

import Game.LambdaHack.Action
import qualified Game.LambdaHack.Client.Action.ConfigIO as Client.ConfigIO
import Game.LambdaHack.Client
import Game.LambdaHack.Client.Action.Frontend
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Perception
import Game.LambdaHack.Server.Action.ActionClass (ConnDict)
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.Server
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile

-- | Wire together content, the definitions of game commands,
-- config and a high-level startup function
-- to form the starting game session. Evaluate to check for errors,
-- in particular verify content consistency.
-- Then create the starting game config from the default config file
-- and initialize the engine with the starting session.
startFrontend :: (MonadActionAbort m, MonadActionAbort n)
              => Kind.COps
              -> (m () -> Pers -> State -> StateServer -> ConnDict -> IO ())
              -> (n () -> Maybe FrontendSession -> Maybe Binding -> ConfigUI
                  -> State -> StateClient -> ConnClient -> IO ())
              -> m () -> n () -> IO ()
startFrontend !copsSlow@Kind.COps{corule, cotile=tile}
              executorS executorC
              loopSer loopCli = do
  -- Compute and insert auxiliary optimized components into game content,
  -- to be used in time-critical sections of the code.
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
      cops = copsSlow {Kind.cotile}
  -- UI config reloaded at each client start.
  sconfigUI <- Client.ConfigIO.mkConfigUI corule
  -- A throw-away copy of rules config reloaded at client start, too,
  -- until an old version of the config can be read from the savefile.
  (sconfig, _, _) <- ConfigIO.mkConfigRules corule
  let !sbinding = stdBinding sconfigUI
      font = configFont sconfigUI
      -- In addition to handling the turn, if the game ends or exits,
      -- handle the history and backup savefile.
      handleServer = do
        loopSer
--        d <- getDict
--        -- Save history often, at each game exit, in case of crashes.
--        liftIO $ Save.rmBkpSaveHistory sconfig sconfigUI d
      loop sfs = start executorS executorC
                       sfs cops sbinding sconfig sconfigUI
                       handleServer loopCli
  startup font loop

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: (MonadActionAbort m, MonadActionAbort n)
      => (m () -> Pers -> State -> StateServer -> ConnDict -> IO ())
      -> (n () -> Maybe FrontendSession -> Maybe Binding -> ConfigUI
          -> State -> StateClient -> ConnClient -> IO ())
      -> FrontendSession -> Kind.COps -> Binding -> Config -> ConfigUI
      -> m () -> n () -> IO ()
start executorS executorC sfs cops@Kind.COps{corule}
      sbinding sconfig sconfigUI handleServer loopClient = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  -- TODO: rewrite; this is a bit wrong
  (gloR, serR, funR) <- gameReset cops
  restored <- Save.restoreGameSer sconfig sconfigUI pathsDataFile title
  (glo, ser, _msg) <- case restored of
    Right msg -> do  -- Starting a new game.
      return (gloR, serR, msg)
    Left (gloL, serL, msg) -> do  -- Running a restored game.
      let gloCops = updateCOps (const cops) gloL
      return (gloCops, serL, msg)
  -- Prepare data for the server.
  let tryFov = stryFov $ sdebugSer ser
      fovMode = fromMaybe (configFovMode sconfig) tryFov
      pers = dungeonPerception cops fovMode glo
      faction = sfaction glo
      mkConnClient = do
        toClient <- newChan
        toServer <- newChan
        return $ ConnClient {toClient, toServer}
      addChan (fid, fact) = do
        chan <- mkConnClient
        let isHuman = isHumanFact fact
        -- For computer players we don't spawn a separate AI client.
        -- In this way computer players are allowed to cheat:
        -- their non-leader actors know leader plans and act accordingly,
        -- while human non-leader actors are controlled by an AI ignorant
        -- of human plans.
        mchan <- if isHuman
                 then fmap Just mkConnClient
                 else return Nothing
        return (fid, (chan, mchan))
  chanAssocs <- mapM addChan $ IM.toList faction
  let d = IM.fromAscList chanAssocs
  -- Prepare data for clients.
  defHist <- defHistory
  let clientAssocs =
        map (\(fid, chans) ->
              let (sper, loc) = funR fid
              -- TODO: rewrite; this is a bit wrong
              in (fid, chans, defStateClient defHist sper, loc))
        chanAssocs
  -- Launch clients.
  let forkClient (fid, (chan, mchan), cli, loc) = do
        if isHumanFaction loc fid
          then void $ forkIO $ executorC
                 loopClient (Just sfs) (Just sbinding) sconfigUI loc cli chan
          else void $ forkIO $ executorC
                 loopClient Nothing Nothing sconfigUI loc cli chan
        case mchan of
          Nothing -> return ()
          Just ch ->
            -- The AI client does not know it's not the main client.
            void $ forkIO
              $ executorC loopClient Nothing Nothing sconfigUI loc cli ch
  mapM_ forkClient clientAssocs
  -- Launch server.
  executorS handleServer pers glo ser d
