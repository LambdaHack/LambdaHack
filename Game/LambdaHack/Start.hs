{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Spwaning client threads and starting up the server.
module Game.LambdaHack.Start
  ( loopFront, speedupCOps
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Server
import Game.LambdaHack.Server.Action.ActionClass (ConnDict)
import qualified Game.LambdaHack.Server.Action.ConfigIO as ConfigIO
import qualified Game.LambdaHack.Server.Action.Save as Save
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code. Also, evaluate content
-- to check consistency.
speedupCOps :: Kind.COps -> Kind.COps
speedupCOps !copsSlow@Kind.COps{cotile=tile} =
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
  in copsSlow {Kind.cotile}

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
loopFront :: Kind.COps
          -> (Pers -> State -> StateServer -> ConnDict -> IO ())
          -> (FactionId -> ConnClient -> IO ())
          -> (FactionId -> ConnClient -> IO ())
          -> IO ()
loopFront cops@Kind.COps{corule} executorS executorHuman executorComputer = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  -- A throw-away copy of rules config reloaded at client start, too,
  -- until an old version of the config can be read from the savefile.
  (sconfig, _, _) <- ConfigIO.mkConfigRules corule
  -- TODO: rewrite; this is a bit wrong
  restored <- Save.restoreGameSer sconfig pathsDataFile title
  (glo, ser, _msg) <- case restored of
    Right msg -> do  -- Starting a new game.
      (gloR, serR, _) <- gameReset cops
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
  -- Launch clients.
  let forkClient (fid, (chan, mchan)) = do
        if isHumanFaction glo fid
          then void $ forkIO $ executorHuman fid chan
          else void $ forkIO $ executorComputer fid chan
        case mchan of
          Nothing -> return ()
          Just ch ->
            -- The AI client does not know it's not the main client.
            void $ forkIO $ executorComputer fid ch
  mapM_ forkClient chanAssocs
  -- Launch server.
  executorS pers glo ser d
