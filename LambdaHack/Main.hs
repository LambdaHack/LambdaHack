-- | The main code file of LambdaHack. Here the knot of engine
-- code pieces and the LambdaHack-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.StrategyKind
import qualified Content.TileKind
import Game.LambdaHack.Client
import Game.LambdaHack.Client.Action
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.LoopAction

-- | Fire up the frontend with the engine fueled by content.
-- The @Action@ type to be used is decided by the second argument
-- to @startFrontend@. It neededn't be @ActionType.Action@.
-- Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
main :: IO ()
main =
  let cops = Kind.COps
        { coactor = Kind.createOps Content.ActorKind.cdefs
        , cocave  = Kind.createOps Content.CaveKind.cdefs
        , cofact  = Kind.createOps Content.FactionKind.cdefs
        , coitem  = Kind.createOps Content.ItemKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , costrat = Kind.createOps Content.StrategyKind.cdefs
        , cotile  = Kind.createOps Content.TileKind.cdefs
        }
  in startFrontend executorSer executorCli
                   cops loopServer (loopClient2 cmdUpdateCli cmdQueryCli)
