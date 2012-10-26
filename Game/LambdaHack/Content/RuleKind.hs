-- | The type of game rule sets and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), ruvalidate
  ) where

import Data.Version

import Game.LambdaHack.PointXY
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Point
import Game.LambdaHack.Misc

-- TODO: very few rules are configurable yet, extend as needed.

-- | The type of game rule sets and assorted game data.
--
-- For now the rules are immutable througout the game, so there is
-- no type @Rule@ to hold any changing parameters, just @RuleKind@
-- for the fixed set.
-- However, in the future, if the rules can get changed during gameplay
-- based on data mining of player behaviour, we may add such a type
-- and then @RuleKind@ will become just a starting template, analogously
-- as for the other content.
--
-- The @raccessible@ field holds a predicate that tells
-- whether one location is accessible from another.
-- Precondition: the two locations are next to each other.
data RuleKind = RuleKind
  { rsymbol           :: Char     -- ^ a symbol
  , rname             :: String   -- ^ short description
  , rfreq             :: Freqs    -- ^ frequency within groups
  , raccessible       :: X -> Point -> TileKind -> Point -> TileKind -> Bool
  , rtitle            :: String   -- ^ the title of the game
  , rpathsDataFile    :: FilePath -> IO FilePath  -- ^ the path to data files
  , rpathsVersion     :: Version  -- ^ the version of the game
  , ritemMelee        :: [Char]   -- ^ symbols of melee weapons
  , ritemProject      :: [Char]   -- ^ symbols of items AI can project
  , rmainMenuArt      :: String   -- ^ the ASCII art for the Main Menu
  }

-- | A dummy instance of the 'Show' class, to satisfy general requirments
-- about content. We won't have many rule sets and they contain functions,
-- so defining a proper instance is not practical.
instance Show RuleKind where
  show _ = "The game ruleset specification."

-- | Validates the ASCII art format (TODO).
ruvalidate :: [RuleKind] -> [RuleKind]
ruvalidate _ = []
