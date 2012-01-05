module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), ruvalidate
  ) where

import Data.Version

import Game.LambdaHack.Geometry
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Loc

-- TODO: very few rules are configurable yet, extend as needed.
-- | As long as the rules immutable, there is no type Rule, just RuleKInd.
-- However, in the future, when the rules can get changed during gameplay
-- based on data mining of player behaviour, there we may add such type
-- and then RuleKind will just remain a starting template.
data RuleKind = RuleKind
  { rsymbol           :: Char
  , rname             :: String
  , rfreq             :: [(String, Int)]  -- ^ frequency within groups
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
  , raccessible       :: X -> Loc -> TileKind -> Loc -> TileKind -> Bool
  , rtitle            :: String
  , rpathsDataFile    :: FilePath -> IO FilePath
  , rpathsVersion     :: Version
  }

instance Show RuleKind where
  show _ = "A game ruleset specification."

ruvalidate :: [RuleKind] -> [RuleKind]
ruvalidate _ = [] -- TODO
