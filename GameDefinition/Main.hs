-- | The main source code file of LambdaHack, the game. Here the knot of engine
-- code pieces and the LambdaHack-specific content definitions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Client.UI.Content.KeyKind as Content.KeyKind
import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.Client (exeFrontend)
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.SampleImplementation.SampleImplementationMonadClient (executorCli)
import Game.LambdaHack.SampleImplementation.SampleImplementationMonadServer (executorSer)
import Game.LambdaHack.Server (mainSer)

-- | Tie the LambdaHack engine client, server and frontend code
-- with the LambdaHack-specific content definitions, and run the game.
main :: IO ()
main =
  let -- Server content operations, created from server content definitions.
      copsServer = Kind.COps
        { coactor   = Kind.createOps Content.ActorKind.cdefs
        , cocave    = Kind.createOps Content.CaveKind.cdefs
        , cofaction = Kind.createOps Content.FactionKind.cdefs
        , coitem    = Kind.createOps Content.ItemKind.cdefs
        , comode    = Kind.createOps Content.ModeKind.cdefs
        , coplace   = Kind.createOps Content.PlaceKind.cdefs
        , corule    = Kind.createOps Content.RuleKind.cdefs
        , cotile    = Kind.createOps Content.TileKind.cdefs
        }
      -- Client content operations.
      copsClient = Content.KeyKind.standardKeys
      -- A single frontend is currently started by the server,
      -- instead of each client starting it's own.
      startupFrontend = exeFrontend executorCli executorCli copsClient
  in mainSer copsServer executorSer startupFrontend
