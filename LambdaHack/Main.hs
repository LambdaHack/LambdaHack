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
  -- Debug arguments for the server. Gamplay is configured via config.rules.
  args <- getArgs
  let usage =
        [ "Only debug options here. "
          ++ "Gamplay is configured via config.rules.ini."
        , "  -knowMap reveals map for all clients in the next game"
        , "  -knowEvents makes all events visible to clients in the next game"
        , "  -tryFov m sets a Field of View mode, where m can be"
        , "    Digital r, r > 0"
        , "    Permissive"
        , "    Shadow"
        , "    Blind"
        ]
      parseArgs [] = defDebugModeSer
      parseArgs ("-knowMap" : rest) =
        (parseArgs rest) {sknowMap = True}
      parseArgs ("-knowEvents" : rest) =
        (parseArgs rest) {sknowEvents = True}
      parseArgs ("-tryFov" : "Digital" : r : rest) | (read r :: Int) > 0 =
        (parseArgs rest) {stryFov = Just $ Digital $ read r}
      parseArgs ("-tryFov" : fovMode : rest) =
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
      cops = speedupCOps copsSlow
      loopHuman :: (MonadActionAbort m, MonadAction m
                   , MonadClientUI m, MonadClientChan m)
                => m ()
      loopHuman = loopUI cmdCliSem cmdUISem
      loopComputer :: (MonadAction m, MonadClientChan m) => m ()
      loopComputer = loopCli cmdCliSem
      exeClient False = executorCli loopHuman
      exeClient True = executorCli loopComputer
      loopServer = loopSer sdebugNxt cmdSerSem
      exeServer executorC = executorSer (loopServer executorC cops)
  exeFrontend cops exeClient exeServer
  waitForChildren
