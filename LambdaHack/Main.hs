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

import qualified ConfigDefault

cops :: Kind.COps
cops = Kind.COps
  { coactor = Kind.createOps Content.ActorKind.cdefs
  , cocave  = Kind.createOps Content.CaveKind.cdefs
  , coitem  = Kind.createOps Content.ItemKind.cdefs
  , coroom  = Kind.createOps Content.PlaceKind.cdefs
  , corule  = Kind.createOps Content.RuleKind.cdefs
  , cotile  = Kind.createOps Content.TileKind.cdefs
  }

main :: IO ()
main =
  Display.startup $
    Start.start cops ConfigDefault.configDefault cmdSemantics cmdDescription
