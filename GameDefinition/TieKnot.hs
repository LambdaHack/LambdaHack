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
import Game.LambdaHack.SampleImplementation.SampleMonadClient (executorCli)
import Game.LambdaHack.SampleImplementation.SampleMonadServer (executorSer)

import Game.LambdaHack.Client
import Game.LambdaHack.Client.State hiding (sdebugCli)
import Game.LambdaHack.Client.UI.Config
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
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
      !cops = speedupCOps False copsSlow
      -- Client content operations.
      copsClient = Content.KeyKind.standardKeys
  sdebugNxt <- debugArgs args
  -- The action monad types to be used are determined by the 'exeSer'
  -- and 'executorCli' calls. If other functions are used in their place
  -- the types are different and so the whole pattern of computation
  -- is different. Which of the frontends is run depends on the flags supplied
  -- when compiling the engine library.
  -- Wire together game content, the main loops of game clients,
  -- the main game loop assigned to this frontend,
  -- UI config and the definitions of game commands.
  -- UI config reloaded at each client start.
  sconfig <- mkConfig cops
  let debugCli = sdebugCli sdebugNxt
      sdebugMode = applyConfigToDebug sconfig debugCli cops
  defaultHist <- defaultHistory $ configHistoryMax sconfig
  let cli = defStateClient defaultHist emptyReport
      s = updateCOps (const cops) emptyState
      exeClientAI fid = (executorCli . loopAI) sdebugMode s (cli fid True)
      exeClientUI fid = (\xconfig xdebugCli ->
                           executorCli $ loopUI copsClient xconfig xdebugCli)
                             sconfig sdebugMode s (cli fid False)
  executorSer $ loopSer cops sdebugNxt exeClientUI exeClientAI
