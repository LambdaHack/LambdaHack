-- | The type of game rules and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleContent(..), emptyRuleContent, makeData
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Ini as Ini
import qualified Data.Ini.Types as Ini
import           Data.Version

import Game.LambdaHack.Definition.Defs

-- | The type of game rules and assorted game data.
data RuleContent = RuleContent
  { rtitle            :: String    -- ^ title of the game (not lib)
  , rXmax             :: X         -- ^ maximum level width; for now,
                                   --   keep equal to ScreenContent.rwidth
  , rYmax             :: Y         -- ^ maximum level height; for now,
                                   --   keep equal to ScreenContent.rheight - 3
  , rexeVersion       :: Version   -- ^ version of the game
  , rcfgUIName        :: FilePath  -- ^ name of the UI config file
  , rcfgUIDefault     :: (String, Ini.Config)
                                   -- ^ the default UI settings config file
  , rwriteSaveClips   :: Int       -- ^ game saved that often (not on browser)
  , rleadLevelClips   :: Int       -- ^ server switches leader level that often
  , rscoresFile       :: FilePath  -- ^ name of the scores file
  , rnearby           :: Int       -- ^ what is a close distance between actors
  , rstairWordCarried :: [Text]    -- ^ words that can't be dropped from stair
                                   --   name as it goes through levels
  , rsymbolProjectile :: Char
  }

emptyRuleContent :: RuleContent
emptyRuleContent = RuleContent
  { rtitle = ""
  , rXmax = 0
  , rYmax = 0
  , rexeVersion = makeVersion []
  , rcfgUIName = ""
  , rcfgUIDefault = ("", Ini.emptyConfig)
  , rwriteSaveClips = 0
  , rleadLevelClips = 0
  , rscoresFile = ""
  , rnearby = 0
  , rstairWordCarried = []
  , rsymbolProjectile = '0'
  }

-- | Catch invalid rule kind definitions.
validateSingle :: RuleContent -> [Text]
validateSingle _ = []

makeData :: RuleContent -> RuleContent
makeData rc =
  let singleOffenders = validateSingle rc
  in assert (null singleOffenders
             `blame` "Rule Content not valid"
             `swith` singleOffenders)
     rc
