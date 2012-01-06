module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), ruvalidate
  ) where

import Data.Version

import Game.LambdaHack.Geometry
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Loc
import Game.LambdaHack.Content.Content

-- TODO: very few rules are configurable yet, extend as needed.
-- | As long as the rules immutable, there is no type Rule, just RuleKInd.
-- However, in the future, if the rules can get changed during gameplay
-- based on data mining of player behaviour, there we may add such type
-- and then RuleKind will just remain a starting template.
data RuleKind = RuleKind
  { rsymbol           :: Char     -- ^ a symbol
  , rname             :: String   -- ^ short description
  , rfreq             :: Freqs    -- ^ frequency within groups
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
  , raccessible       :: X -> Loc -> TileKind -> Loc -> TileKind -> Bool
  , rtitle            :: String   -- ^ the title of the game
  , rpathsDataFile    :: FilePath -> IO FilePath  -- ^ the path to data files
  , rpathsVersion     :: Version  -- ^ the version of the game
  }

instance Show RuleKind where
  show _ = "The game ruleset specification."

-- | No specific possible problems for the content of this kind, so far.
ruvalidate :: [RuleKind] -> [RuleKind]
ruvalidate _ = []
