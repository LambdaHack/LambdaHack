-- | The main code file of LambdaHack. Here the knot of engine
-- code pieces and the LambdaHack-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Game.LambdaHack.Display as Display
import qualified Game.LambdaHack.Kind as Kind
import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import qualified Game.LambdaHack.Start as Start
import Game.LambdaHack.Command
import Game.LambdaHack.Display

import qualified ConfigDefault

-- | Gather together the content and verify its consistency.
cops :: Kind.COps
cops = Kind.COps
  { coactor = Kind.createOps Content.ActorKind.cdefs
  , cocave  = Kind.createOps Content.CaveKind.cdefs
  , coitem  = Kind.createOps Content.ItemKind.cdefs
  , coplace = Kind.createOps Content.PlaceKind.cdefs
  , corule  = Kind.createOps Content.RuleKind.cdefs
  , cotile  = Kind.createOps Content.TileKind.cdefs
  }

-- | Wire together the content, the default config file and the definitions
-- of the game commands. Each of these parts is autonomously modifiable.
start :: FrontendSession -> IO ()
start = Start.start cops ConfigDefault.configDefault cmdSemantics cmdDescription

-- | Start the frontend with the game rules. Which of the frontends is run
-- depends on the flags supplied when compiling the engine library.
main :: IO ()
main = Display.startup start
