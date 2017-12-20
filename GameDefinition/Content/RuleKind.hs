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
import qualified Paths_LambdaHack as Self (getDataFileName, version)

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
  , rfontDir = $(do
      x <- qRunIO (Self.getDataFileName "GameDefinition/fonts")
      lift x)
  , rexeVersion = Self.version
  -- The strings containing the default configuration file
  -- included from config.ui.default.
  , rcfgUIName = "config.ui" <.> "ini"
  , rcfgUIDefault = $(do
      let path = "GameDefinition" </> "config.ui" <.> "default"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      lift x)
  -- ASCII art for the Main Menu. Only pure 7-bit ASCII characters are allowed.
  -- When displayed in the Main Menu screen, the picture is overwritten
  -- with game and engine version strings and keybindings.
  -- The keybindings overwrite places marked with left curly brace signs.
  -- This sign is forbidden anywhere else in the picture.
  -- The picture and the whole Main Menu is displayed dull white on black.
  --
  -- The picture should be exactly 60 rows by 110 columns,
  -- but only the middle rectangle of 24 rows by 80 columns is partially
  -- overwritten with UI information and the curly brace signs are allowed
  -- only there. So, the rectangle is 15 characters distant from the left
  -- and 18 from top. For screen sizes larger than 60 by 100,
  -- the picture is centered and padded with spaces, so it makes sense
  -- for some or all of the picture borders to be spaces, as well.
  -- If the screen is smaller than 60 by 100, borders of the picture
  -- are cut off. Minimal screes size is 24 by 80 and the picture
  -- should look well at this size, as well.
  , rmainMenuArt = $(do
      let path = "GameDefinition/MainMenu.ascii"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      lift x)
  , rintroScreen = $(do
      let path = "GameDefinition/PLAYING.md"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      let paragraphs :: [String] -> [String] -> [[String]]
          paragraphs [] rows = [reverse rows]
          paragraphs (l : ls) rows = if null l
                                     then reverse rows : paragraphs ls []
                                     else paragraphs ls (l : rows)
          intro = case paragraphs (lines x) [] of
            _title : _blurb : par1 : par2 : _rest -> par1 ++ [""] ++ par2
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rfirstDeathEnds = False
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "LambdaHack.scores"
  , rnearby = 20
  }
