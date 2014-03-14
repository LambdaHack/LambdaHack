-- | The main source code file of LambdaHack. Here the knot of engine
-- code pieces and the LambdaHack-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.Client
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.SampleImplementation.SampleImplementationMonadClient
import Game.LambdaHack.SampleImplementation.SampleImplementationMonadServer
import Game.LambdaHack.Server

-- | Tie the LambdaHack engine clients and server code
-- with the LambdaHack-specific content defintions and run the game.
main :: IO ()
main =
  let copsSlow = Kind.COps
        { coactor   = Kind.createOps Content.ActorKind.cdefs
        , cocave    = Kind.createOps Content.CaveKind.cdefs
        , cofaction = Kind.createOps Content.FactionKind.cdefs
        , coitem    = Kind.createOps Content.ItemKind.cdefs
        , comode    = Kind.createOps Content.ModeKind.cdefs
        , coplace   = Kind.createOps Content.PlaceKind.cdefs
        , corule    = Kind.createOps Content.RuleKind.cdefs
        , cotile    = Kind.createOps Content.TileKind.cdefs
        }
  in mainSer copsSlow executorSer $ exeFrontend executorCli executorCli
