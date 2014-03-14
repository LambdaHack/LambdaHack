{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Game.LambdaHack.Client.MonadClient.SampleImplementationMonadClient
import Game.LambdaHack.Common.Action (MonadAtomic (..))
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.AtomicSem
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Server
import Game.LambdaHack.Server.MonadServer.SampleImplementationMonadServer
import Game.LambdaHack.Server.AtomicSemSer

-- | The game-state semantics of atomic game commands
-- as computed on the server.
instance MonadAtomic ActionSer where
  execAtomic = atomicSendSem

-- | The game-state semantics of atomic game commands
-- as computed on clients. Special effects (@SfxAtomic@) don't modify state.
instance MonadAtomic (ActionCli c d) where
  execAtomic (CmdAtomic cmd) = cmdAtomicSem cmd
  execAtomic (SfxAtomic _) = return ()

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
