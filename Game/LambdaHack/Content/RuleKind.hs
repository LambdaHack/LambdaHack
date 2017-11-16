-- | The type of game rule sets and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), validateSingleRuleKind, validateAllRuleKind
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Version

import Game.LambdaHack.Common.Misc

-- | The type of game rule sets and assorted game data.
--
-- In principle, it'se possible to have many rule sets
-- and switch between them during a game session or even a single game.
data RuleKind = RuleKind
  { rsymbol         :: Char      -- ^ a symbol
  , rname           :: Text      -- ^ short description
  , rfreq           :: Freqs RuleKind  -- ^ frequency within groups
  , rtitle          :: Text      -- ^ title of the game (not lib)
  , rfontDir        :: FilePath  -- ^ font directory for the game (not lib)
  , rexeVersion     :: Version   -- ^ version of the game
  , rcfgUIName      :: FilePath  -- ^ name of the UI config file
  , rcfgUIDefault   :: String    -- ^ the default UI settings config file
  , rmainMenuArt    :: Text      -- ^ the ASCII art for the Main Menu
  , rfirstDeathEnds :: Bool      -- ^ whether first non-spawner actor death
                                  --   ends the game
  , rwriteSaveClips :: Int       -- ^ game is saved that often
  , rleadLevelClips :: Int       -- ^ server switches leader level that often
  , rscoresFile     :: FilePath  -- ^ name of the scores file
  , rnearby         :: Int       -- ^ what distance between actors is 'nearby'
  }

-- | A dummy instance of the 'Show' class, to satisfy general requirments
-- about content. We won't don't expect to ever print out whole rule sets.
instance Show RuleKind where
  show _ = "The game ruleset specification."

-- | Catch invalid rule kind definitions.
validateSingleRuleKind :: RuleKind -> [Text]
validateSingleRuleKind _ = []

-- | Since we have only one rule kind, the set of rule kinds is always valid.
validateAllRuleKind :: [RuleKind] -> [Text]
validateAllRuleKind _ = []
