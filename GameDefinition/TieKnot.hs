-- | Here the knot of engine code pieces and the game-specific
-- content definitions is tied, resulting in an executable game.
module TieKnot
  ( tieKnot
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Client.UI.Content.KeyKind as Content.KeyKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.SampleImplementation.SampleMonadClientAsThread (executorCliAsThread)
import Game.LambdaHack.SampleImplementation.SampleMonadServer (executorSer)

import Game.LambdaHack.Client
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.SessionUI
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Server
import Game.LambdaHack.Server.State

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
--
-- The action monad types to be used are determined by the 'executorSer'
-- and 'executorCli' calls. If other functions are used in their place
-- the types are different and so the whole pattern of computation
-- is different. Which of the frontends is run inside the UI client
-- depends on the flags supplied when compiling the engine library.
tieKnot :: [String] -> IO ()
tieKnot args = do
  -- Options for the next game taken from the commandline.
  sdebugNxt@DebugModeSer{sallClear, sdebugCli} <- debugArgs args
  let -- Common content operations, created from content definitions.
      -- Evaluated fully to discover errors ASAP and free memory.
      cotile = Kind.createOps Content.TileKind.cdefs
      !cops = Kind.COps
        { cocave  = Kind.createOps Content.CaveKind.cdefs
        , coitem  = Kind.createOps Content.ItemKind.cdefs
        , comode  = Kind.createOps Content.ModeKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , cotile
        , coClear = sallClear
        , coTileSpeedup = Tile.speedup sallClear cotile
        }
      -- Client content operations containing default keypresses
      -- and command descriptions.
      !copsClient = Content.KeyKind.standardKeys
  -- Parsed UI client configuration file.
  -- It is reloaded at each game executable start.
  sconfig <- mkConfig cops sdebugCli
  -- Options for the clients modified with the configuration file.
  -- The client debug inside server debug only holds the client commandline
  -- options and is never updated with config options, etc.
  let sdebugMode = applyConfigToDebug cops sconfig sdebugCli
      -- Partially applied main loops of the clients.
      exeClientAI = executorCliAsThread cops ()
                    $ loopAI sdebugMode
      exeClientUI = executorCliAsThread cops (emptySessionUI sconfig)
                    $ loopUI copsClient sconfig sdebugMode
  -- Wire together game content, the main loops of game clients
  -- and the game server loop.
  executorSer cops $ loopSer sdebugNxt exeClientUI exeClientAI
