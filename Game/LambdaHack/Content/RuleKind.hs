-- | The type of game rule sets and assorted game data.
module Game.LambdaHack.Content.RuleKind
  ( RuleKind(..), FovMode(..), validateSingleRuleKind, validateAllRuleKind
  ) where

import Data.Binary
import Data.Text (Text)
import Data.Version

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point

-- TODO: very few rules are configurable yet, extend as needed.
-- TODO: in the future, in @raccessible@ check flying for chasms,
-- swimming for water, etc.
-- TODO: tweak other code to allow games with only cardinal direction moves

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
-- The @raccessible@ field holds extra conditions that have to be met
-- for a tile to be accessible, on top of being an open tile
-- (or openable, in some contexts). The @raccessibleDoor@ field
-- contains yet additional conditions concerning tiles that are doors,
-- whether open or closed.
-- Precondition: the two positions are next to each other.
-- We assume the predicate is symmetric.
data RuleKind = RuleKind
  { rsymbol         :: !Char      -- ^ a symbol
  , rname           :: !Text      -- ^ short description
  , rfreq           :: !(Freqs RuleKind)  -- ^ frequency within groups
  , raccessible     :: !(Maybe (Point -> Point -> Bool))  -- ^ see above
  , raccessibleDoor :: !(Maybe (Point -> Point -> Bool))  -- ^ see above
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
  , rfovMode        :: !FovMode   -- ^ FOV calculation mode
  , rwriteSaveClips :: !Int       -- ^ game is saved that often
  , rleadLevelClips :: !Int       -- ^ server switches leader level that often
  , rscoresFile     :: !FilePath  -- ^ name of the scores file
  , rnearby         :: !Int       -- ^ what distance between actors is 'nearby'
  }

-- | Field Of View scanning mode.
data FovMode =
    Shadow      -- ^ restrictive shadow casting (not symmetric!)
  | Permissive  -- ^ permissive FOV
  | Digital     -- ^ digital FOV
  deriving (Show, Read)

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

instance Binary FovMode where
  put Shadow      = putWord8 0
  put Permissive  = putWord8 1
  put Digital     = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> return Shadow
      1 -> return Permissive
      2 -> return Digital
      _ -> fail "no parse (FovMode)"
