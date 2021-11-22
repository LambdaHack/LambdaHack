-- | The type of game rules and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleContent(..), emptyRuleContent, makeData
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , emptyRuleContentRaw, validateSingle
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Ini as Ini
import qualified Data.Ini.Types as Ini
import           Data.Version

import Game.LambdaHack.Content.ItemKind
  (ItemSymbolsUsedInEngine, emptyItemSymbolsUsedInEngine)
import Game.LambdaHack.Definition.Defs

-- | The type of game rules and assorted game data.
data RuleContent = RuleContent
  { rtitle            :: String    -- ^ title of the game (not lib)
  , rWidthMax         :: X         -- ^ maximum level width
  , rHeightMax        :: Y         -- ^ maximum level height
  , rexeVersion       :: Version   -- ^ version of the game
  , rcfgUIName        :: FilePath  -- ^ name of the UI config file
  , rcfgUIDefault     :: (Text, Ini.Config)
                                   -- ^ the default UI settings config file
  , rwriteSaveClips   :: Int       -- ^ game saved that often (not on browser)
  , rleadLevelClips   :: Int       -- ^ server switches leader level that often
  , rscoresFileName   :: FilePath  -- ^ name of the scores file
  , rnearby           :: Int       -- ^ what is a close distance between actors
  , rstairWordCarried :: [Text]    -- ^ words that can't be dropped from stair
                                   --   name as it goes through levels
  , ritemSymbols      :: ItemSymbolsUsedInEngine
                                   -- ^ item symbols treated specially in engine
  }

emptyRuleContentRaw :: RuleContent
emptyRuleContentRaw = RuleContent
  { rtitle = ""
  , rWidthMax = 2
  , rHeightMax = 2
  , rexeVersion = makeVersion []
  , rcfgUIName = ""
  , rcfgUIDefault = ("", Ini.emptyConfig)
  , rwriteSaveClips = 0
  , rleadLevelClips = 0
  , rscoresFileName = ""
  , rnearby = 0
  , rstairWordCarried = []
  , ritemSymbols = emptyItemSymbolsUsedInEngine
  }

emptyRuleContent :: RuleContent
emptyRuleContent = assert (null $ validateSingle emptyRuleContentRaw)
                          emptyRuleContentRaw

-- | Catch invalid rule kind definitions.
validateSingle :: RuleContent -> [Text]
validateSingle RuleContent{..} =
  [ "rWidthMax < 2" | rWidthMax < 2 ]  -- hero, opponent, stairs, wiggle room
  ++ [ "rHeightMax < 2" | rHeightMax < 2 ]  -- or 4 tiles of sentinel wall

makeData :: RuleContent -> RuleContent
makeData rc =
  let singleOffenders = validateSingle rc
  in assert (null singleOffenders
             `blame` "Rule Content not valid"
             `swith` singleOffenders)
     rc
