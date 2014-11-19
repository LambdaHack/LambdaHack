-- | Here the knot of engine code pieces and the game-specific
-- content definitions is tied, resulting in an executable game.
module TieKnot ( tieKnot ) where

import qualified Client.UI.Content.KeyKind as Content.KeyKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.ItemKindActor
import qualified Content.ItemKindOrgan
import qualified Content.ItemKindShrapnel
import qualified Content.ItemKindTemporary
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.Client (exeFrontend)
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.SampleImplementation.SampleMonadClient (executorCli)
import Game.LambdaHack.SampleImplementation.SampleMonadServer (executorSer)
import Game.LambdaHack.Server (mainSer)

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
tieKnot :: [String] -> IO ()
tieKnot args =
  let -- Common content operations, created from content definitions.
      copsServer = Kind.COps
        { cocave     = Kind.createOps Content.CaveKind.cdefs
        , coitem     = Kind.createOps Content.ItemKind.cdefs
        , coactor    = Kind.createOps Content.ItemKindActor.cdefs
        , coorgan    = Kind.createOps Content.ItemKindOrgan.cdefs
        , coshrapnel = Kind.createOps Content.ItemKindShrapnel.cdefs
        , cotmp      = Kind.createOps Content.ItemKindTemporary.cdefs
        , comode     = Kind.createOps Content.ModeKind.cdefs
        , coplace    = Kind.createOps Content.PlaceKind.cdefs
        , corule     = Kind.createOps Content.RuleKind.cdefs
        , cotile     = Kind.createOps Content.TileKind.cdefs
        }
      -- Client content operations.
      copsClient = Content.KeyKind.standardKeys
      -- A single frontend is currently started by the server,
      -- instead of each client starting it's own.
      startupFrontend = exeFrontend executorCli executorCli copsClient
  in mainSer args copsServer executorSer startupFrontend
