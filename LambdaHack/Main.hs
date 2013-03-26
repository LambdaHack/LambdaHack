{-# OPTIONS -fno-warn-orphans #-}
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
import Game.LambdaHack.Action (MonadAtomic (..))
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.AtomicSem
import Game.LambdaHack.Client
import Game.LambdaHack.Client.Action.ActionType
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Server
import Game.LambdaHack.Server.Action.ActionType
import Game.LambdaHack.Server.AtomicSemSer

instance MonadAtomic ActionSer where
  execAtomic = atomicSendSem

instance MonadAtomic (ActionCli c) where
  execAtomic (CmdAtomic cmd) = cmdAtomicSem cmd
  execAtomic (SfxAtomic _) = return ()

main :: IO ()
main =
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
  in mainSer copsSlow executorSer $ exeFrontend executorCli executorCli
