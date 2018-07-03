-- | The type of game rules and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleContent(..), emptyRuleContent, makeData
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Version

-- | The type of game rules and assorted game data.
data RuleContent = RuleContent
  { rtitle          :: Text      -- ^ title of the game (not lib)
  , rfontDir        :: FilePath  -- ^ font directory for the game (not lib)
  , rexeVersion     :: Version   -- ^ version of the game
  , rcfgUIName      :: FilePath  -- ^ name of the UI config file
  , rcfgUIDefault   :: String    -- ^ the default UI settings config file
  , rfirstDeathEnds :: Bool      -- ^ whether first non-spawner actor death
                                 --   ends the game
  , rwriteSaveClips :: Int       -- ^ game is saved that often (not on browser)
  , rleadLevelClips :: Int       -- ^ server switches leader level that often
  , rscoresFile     :: FilePath  -- ^ name of the scores file
  , rnearby         :: Int       -- ^ what distance between actors is 'nearby'
  }

emptyRuleContent :: RuleContent
emptyRuleContent = RuleContent
  { rtitle = ""
  , rfontDir = ""
  , rexeVersion = makeVersion []
  , rcfgUIName = ""
  , rcfgUIDefault = ""
  , rfirstDeathEnds = False
  , rwriteSaveClips = 0
  , rleadLevelClips = 0
  , rscoresFile = ""
  , rnearby = 0
  }

-- | Catch invalid rule kind definitions.
validateSingle :: RuleContent -> [Text]
validateSingle _ = []

makeData :: RuleContent -> RuleContent
makeData rc =
  let singleOffenders = validateSingle rc
  in assert (null singleOffenders
             `blame` "Rule Content not valid"
             `swith` singleOffenders) $
     rc
