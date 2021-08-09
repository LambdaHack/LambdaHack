{-# LANGUAGE TemplateHaskell #-}
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( standardRules
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Ini.Reader as Ini
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax
import           System.FilePath
import           System.IO
  (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, utf8)

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Game.LambdaHack.Content.RuleKind

standardRules :: RuleContent
standardRules = RuleContent
  { rtitle = "LambdaHack"
  , rXmax = 80
  , rYmax = 21
  , rexeVersion = Self.version
  -- The strings containing the default configuration file
  -- included from config.ui.default.
  , rcfgUIName = "config.ui" <.> "ini"
  , rcfgUIDefault = $(do
      let path = "GameDefinition" </> "config.ui" <.> "default"
      qAddDependentFile path
      s <- qRunIO $ do
        inputHandle <- openFile path ReadMode
        hSetEncoding inputHandle utf8
        hGetContents inputHandle
      let cfgUIDefault =
            either (error . ("Ini.parse of default config" `showFailure`)) id
            $ Ini.parse s
      lift (s, cfgUIDefault))
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "LambdaHack.scores"
  , rnearby = 20
  , rstairWordCarried = ["staircase"]  -- only one, so inert
  , rsymbolProjectile = '|'
  , rsymbolLight      = '('
  , rsymbolTool       = '('
  , rsymbolSpecial    = '*'  -- don't overuse; it clashes with projectiles
  , rsymbolGold       = '$'  -- also gems
  , rsymbolNecklace   = '"'
  , rsymbolRing       = '='
  , rsymbolPotion     = '!'  -- also concoction, bottle, jar, vial
  , rsymbolFlask      = '!'
  , rsymbolScroll     = '?'  -- also book, note, tablet, card
  , rsymbolTorsoArmor = '['
  , rsymbolMiscArmor  = '['
  , rsymbolClothes    = '['
  , rsymbolShield     = ']'
  , rsymbolPolearm    = ')'
  , rsymbolEdged      = ')'
  , rsymbolHafted     = ')'
  , rsymbolWand       = '/'  -- also magical rod, pistol, instrument
  , rsymbolFood       = ','  -- also body part;
                             -- distinct from floor: not middle dot
  }
