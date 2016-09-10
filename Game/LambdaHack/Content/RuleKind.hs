-- | The type of game rule sets and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), validateSingleRuleKind, validateAllRuleKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Version

import Game.LambdaHack.Common.Misc

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
data RuleKind = RuleKind
  { rsymbol         :: !Char      -- ^ a symbol
  , rname           :: !Text      -- ^ short description
  , rfreq           :: !(Freqs RuleKind)  -- ^ frequency within groups
  , rtitle          :: !Text      -- ^ the title of the game
  , raddress        :: !Text      -- ^ the homepage of the game
  , rpathsDataFile  :: FilePath -> IO FilePath
                                  -- ^ the path to data files
  , rpathsVersion   :: !Version   -- ^ the version of the game
  , rcfgUIName      :: !FilePath  -- ^ base name of the UI config file
  , rcfgUIDefault   :: !String    -- ^ the default UI settings config file
  , rmainMenuArt    :: !Text      -- ^ the ASCII art for the Main Menu
  , rfirstDeathEnds :: !Bool      -- ^ whether first non-spawner actor death
                                  --   ends the game
  , rwriteSaveClips :: !Int       -- ^ game is saved that often
  , rleadLevelClips :: !Int       -- ^ server switches leader level that often
  , rscoresFile     :: !FilePath  -- ^ name of the scores file
  , rnearby         :: !Int       -- ^ what distance between actors is 'nearby'
  }

-- | A dummy instance of the 'Show' class, to satisfy general requirments
-- about content. We won't have many rule sets and they contain functions,
-- so defining a proper instance is not practical.
instance Show RuleKind where
  show _ = "The game ruleset specification."

-- | Catch invalid rule kind definitions.
-- In particular, this validates the ASCII art format (TODO).
validateSingleRuleKind :: RuleKind -> [Text]
validateSingleRuleKind _ = []

-- | Since we have only one rule kind, the set of rule kinds is always valid.
validateAllRuleKind :: [RuleKind] -> [Text]
validateAllRuleKind _ = []
