{-# LANGUAGE TemplateHaskell #-}
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( standardRules
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO (readFile)

-- Cabal
import qualified Paths_LambdaHack as Self (getDataFileName, version)

import Game.LambdaHack.Content.RuleKind

standardRules :: RuleContent
standardRules = RuleContent
  { rtitle = "LambdaHack"
  , rXmax = 80
  , rYmax = 21
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
  , rfirstDeathEnds = False
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "LambdaHack.scores"
  , rnearby = 20
  , rstairWordCarried = ["staircase"]  -- only one, so inert
  , rsymbolProjectile = '|'
  }
