{-# LANGUAGE TemplateHaskell #-}
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO (readFile)

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.RuleKind

cdefs :: ContentDef RuleKind
cdefs = ContentDef
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validateSingle = validateSingleRuleKind
  , validateAll = validateAllRuleKind
  , content = contentFromList
      [standard]
  }

standard :: RuleKind
standard = RuleKind
  { rsymbol = 's'
  , rname = "standard LambdaHack ruleset"
  , rfreq = [("standard", 100)]
  , rtitle = "LambdaHack"
  , rexeVersion = Self.version
  -- The strings containing the default configuration file
  -- included from config.ui.default.
  , rcfgUIName = "config.ui" <.> "ini"
  , rcfgUIDefault = $(do
      let path = "GameDefinition" </> "config.ui" <.> "default"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      lift x)
  -- ASCII art for the Main Menu. Only pure 7-bit ASCII characters are
  -- allowed. The picture should be exactly 24 rows by 80 columns,
  -- plus an extra frame (of any characters) that is ignored.
  -- For a different screen size, the picture is centered and the outermost
  -- rows and columns cloned. When displayed in the Main Menu screen,
  -- it's overwritten with the game version string and keybinding strings.
  -- The game version string begins and ends with a space and is placed
  -- in the very bottom right corner. The keybindings overwrite places
  -- marked with 25 left curly brace signs '{' in a row. The sign is forbidden
  -- everywhere else. A specific number of such places with 25 left braces
  -- are required, at most one per row, and all are overwritten
  -- with text that is flushed left and padded with spaces.
  -- The Main Menu is displayed dull white on black.
  , rmainMenuArt = $(do
      let path = "GameDefinition/MainMenu.ascii"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      lift x)
  , rfirstDeathEnds = False
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "scores"
  , rnearby = 20
  }
