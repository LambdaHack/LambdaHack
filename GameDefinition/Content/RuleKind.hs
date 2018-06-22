{-# LANGUAGE TemplateHaskell #-}
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO (readFile)

-- Cabal
import qualified Paths_LambdaHack as Self (getDataFileName, version)

import Game.LambdaHack.Content.RuleKind

content :: [RuleKind]
content = [standard]

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
  -- ASCII art for the main menu. Only pure 7-bit ASCII characters are allowed.
  -- When displayed in the main menu screen, the picture is overwritten
  -- with game and engine version strings and keybindings.
  -- The keybindings overwrite places marked with left curly brace signs.
  -- This sign is forbidden anywhere else in the picture.
  -- The picture and the whole main menu is displayed dull white on black.
  -- The glyphs, or at least the character cells, are perfect squares.
  -- The picture should be exactly 45 rows by 80 columns.
  -- For larger screen sizes, the picture is centered and padded with spaces,
  -- so it makes sense for some or all of the picture borders to be spaces.
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
            _title : _blurb : par1 : par2 : _rest ->
              ["", ""] ++ par1 ++ [""] ++ par2 ++ ["", ""]
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rfirstDeathEnds = False
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "LambdaHack.scores"
  , rnearby = 20
  }
