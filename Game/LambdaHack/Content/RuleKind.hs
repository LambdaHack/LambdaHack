-- | The type of game rule sets and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), ruvalidate
  ) where

import Data.Version

import Game.LambdaHack.PointXY
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Loc
import Game.LambdaHack.Content.Content

-- TODO: very few rules are configurable yet, extend as needed.

-- | The type of game rule sets and assorted game data.
--
-- As long as the rules are immutable througout the game, there is
-- no type @Rule@ to hold the changing parapaters, just @RuleKind@.
-- However, in the future, if the rules can get changed during gameplay
-- based on data mining of player behaviour, we may add such a type
-- and then @RuleKind@ will just remain a starting template, analogously
-- as for the other content.
data RuleKind = RuleKind
  { rsymbol           :: Char     -- ^ a symbol
  , rname             :: String   -- ^ short description
  , rfreq             :: Freqs    -- ^ frequency within groups
    -- Predicate that tells whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
  , raccessible       :: X -> Loc -> TileKind -> Loc -> TileKind -> Bool
  , rtitle            :: String   -- ^ the title of the game
  , rpathsDataFile    :: FilePath -> IO FilePath  -- ^ the path to data files
  , rpathsVersion     :: Version  -- ^ the version of the game
  }

-- | A dummy instance of the 'Show' class, to satisfy general requirments
-- about content. We won't have many rule sets and they contain functions,
-- so defining a proper instance is not practical.
instance Show RuleKind where
  show _ = "The game ruleset specification."

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
ruvalidate :: [RuleKind] -> [RuleKind]
ruvalidate _ = []
