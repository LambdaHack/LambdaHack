module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..)
  ) where

import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Loc

data RuleKind = RuleKind
  { rsymbol           :: Char
  , rname             :: String
  , rfreq             :: Int
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
  , raccessible       :: Loc -> TileKind -> Loc -> TileKind -> Bool
  }
