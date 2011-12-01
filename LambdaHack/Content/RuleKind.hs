module Content.RuleKind ( cdefs ) where

import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Content.Content as Content

cdefs :: Content.CDefs RuleKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , content =
      [standard]
  }
standard :: RuleKind

standard = RuleKind
  { rsymbol           = 's'
  , rname             = "Standard game ruleset"
  , rfreq             = 100
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
    -- TODO: in the future check flying for chasms, swimming for water, etc.
  , raccessible       = \ _sloc _src _tloc tgt -> F.Walkable `elem` tfeature tgt
  }
