{-# LANGUAGE FlexibleContexts #-}
-- | The main code file of LambdaHack. Here the knot of engine
-- code pieces and the LambdaHack-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import System.Environment (getArgs)

import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.StrategyKind
import qualified Content.TileKind
import Game.LambdaHack.Action
import Game.LambdaHack.Client
import Game.LambdaHack.Client.Action
import Game.LambdaHack.CmdCli
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Server
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.State

-- | Fire up the frontend with the engine fueled by content.
-- The action monad types to be used are determined by the 'executorSer'
-- and 'executorCli' calls. If other functions are used in their place
-- the types are different and so the whole pattern of computation
-- is different. Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
main :: IO ()
main = do
  args <- getArgs
  let usage =
        [ "Configure server debug options here, gamplay in config.rules.ini."
        , "  --knowMap reveal map for all clients in the next game"
        , "  --knowEvents show all events in the next game (needs --knowMap)"
        , "  --sniffIn display all incoming commands on console "
        , "  --sniffOut display all outgoing commands on console "
        , "  --allClear let all map tiles be translucent"
        , "  --tryFov m set a Field of View mode, where m can be"
        , "    Digital r, r > 0"
        , "    Permissive"
        , "    Shadow"
        , "    Blind"
        ]
      parseArgs [] = defDebugModeSer
      parseArgs ("--knowMap" : rest) =
        (parseArgs rest) {sknowMap = True}
      parseArgs ("--knowEvents" : rest) =
        (parseArgs rest) {sknowEvents = True}
      parseArgs ("--sniffIn" : rest) =
        (parseArgs rest) {sniffIn = True}
      parseArgs ("--sniffOut" : rest) =
        (parseArgs rest) {sniffOut = True}
      parseArgs ("--allClear" : rest) =
        (parseArgs rest) {sallClear = True}
      parseArgs ("--tryFov" : "Digital" : r : rest) | (read r :: Int) > 0 =
        (parseArgs rest) {stryFov = Just $ Digital $ read r}
      parseArgs ("--tryFov" : fovMode : rest) =
        (parseArgs rest) {stryFov = Just $ read fovMode}
      parseArgs _ = error $ unlines usage
      !sdebugNxt = parseArgs args
  let copsSlow = Kind.COps
        { coactor = Kind.createOps Content.ActorKind.cdefs
        , cocave  = Kind.createOps Content.CaveKind.cdefs
        , cofact  = Kind.createOps Content.FactionKind.cdefs
        , coitem  = Kind.createOps Content.ItemKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , costrat = Kind.createOps Content.StrategyKind.cdefs
        , cotile  = Kind.createOps Content.TileKind.cdefs
        }
      cops = speedupCOps False copsSlow
      loopClientUI :: ( MonadActionAbort m, MonadAction m
                      , MonadClientUI m, MonadClientConn CmdClientUI m )
                   => m ()
      loopClientUI = loopUI cmdClientUISem
      loopClientAI :: ( MonadAction m
                      , MonadClient m, MonadClientConn CmdClientAI m )
                   => m ()
      loopClientAI = loopAI cmdClientAISem
      exeClientUI = executorCli loopClientUI
      exeClientAI = executorCli loopClientAI
      loopServer = loopSer sdebugNxt cmdSerSem
      exeServer executorUI executorAI =
        executorSer (loopServer executorUI executorAI cops)
  exeFrontend cops exeClientUI exeClientAI exeServer
  waitForChildren
