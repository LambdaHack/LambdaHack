-- | Here the knot of engine code pieces and the game-specific
-- content definitions is tied, resulting in an executable game.
module TieKnot ( tieKnot ) where

import qualified Client.UI.Content.KeyKind as Content.KeyKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.Client
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.SampleImplementation.SampleMonadClient (executorCli)
import Game.LambdaHack.SampleImplementation.SampleMonadServer (executorSer)
import Game.LambdaHack.Server

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
tieKnot :: [String] -> IO ()
tieKnot args = do
  let -- Common content operations, created from content definitions.
      -- Evaluated fully to discover errors ASAP and free memory.
      !copsSlow = Kind.COps
        { cocave  = Kind.createOps Content.CaveKind.cdefs
        , coitem  = Kind.createOps Content.ItemKind.cdefs
        , comode  = Kind.createOps Content.ModeKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , cotile  = Kind.createOps Content.TileKind.cdefs
        }
      !copsShared = speedupCOps False copsSlow
      -- Client content operations.
      copsClient = Content.KeyKind.standardKeys
  sdebugNxt <- debugArgs args
  -- Fire up the frontend with the engine fueled by content.
  -- The action monad types to be used are determined by the 'exeSer'
  -- and 'executorCli' calls. If other functions are used in their place
  -- the types are different and so the whole pattern of computation
  -- is different. Which of the frontends is run depends on the flags supplied
  -- when compiling the engine library.
  let exeServer executorUI executorAI =
        executorSer $ loopSer copsShared sdebugNxt executorUI executorAI
  -- Currently a single frontend is started by the server,
  -- instead of each client starting it's own.
  srtFrontend (executorCli . loopUI)
              (executorCli . loopAI)
              copsClient copsShared (sdebugCli sdebugNxt) exeServer
