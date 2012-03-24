-- | Game rules and assorted data for LambdaHack.
module Content.RuleKind ( cdefs ) where

-- Cabal
import qualified Paths_LambdaHack as Self (getDataFileName, version)

import Game.LambdaHack.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Content as Content

cdefs :: Content.CDefs RuleKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = ruvalidate
  , content =
      [standard]
  }
standard :: RuleKind

standard = RuleKind
  { rsymbol        = 's'
  , rname          = "standard LambdaHack ruleset"
  , rfreq          = [("standard", 100)]
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
    -- Apart of checking the target tile, we forbid diagonal movement
    -- to and from doors.
  , raccessible    = \ lxsize sloc src tloc tgt ->
      F.Walkable `elem` tfeature tgt
      && not ((F.Closable `elem` tfeature src ||
               F.Closable `elem` tfeature tgt)
              && diagonal lxsize (towards lxsize sloc tloc))
  , rtitle         = "LambdaHack"
  , rpathsDataFile = Self.getDataFileName
  , rpathsVersion  = Self.version
  , ritemMelee     = ")"
  , ritemProject   = "!?|/"
  }
