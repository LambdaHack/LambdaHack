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

import Game.LambdaHack.Content.ItemKind (ItemSymbolsUsedInEngine (..))
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Definition.DefsInternal

standardRules :: RuleContent
standardRules = RuleContent
  { rtitle = "LambdaHack"
  , rWidthMax = 80
  , rHeightMax = 21
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
  , rscoresFileName = "LambdaHack.scores"
  , rnearby = 20
  , rstairWordCarried = ["staircase"]  -- only one, so inert
  , ritemSymbols = ItemSymbolsUsedInEngine
      { rsymbolProjectile = toContentSymbol '|'
      , rsymbolLight      = toContentSymbol '('
      , rsymbolTool       = toContentSymbol '('
      , rsymbolSpecial    = toContentSymbol '*'
                              -- don't overuse; it clashes with projectiles
      , rsymbolGold       = toContentSymbol '$'
                              -- also gems
      , rsymbolNecklace   = toContentSymbol '"'
      , rsymbolRing       = toContentSymbol '='
      , rsymbolPotion     = toContentSymbol '!'
                              -- also concoction, bottle, jar, vial
      , rsymbolFlask      = toContentSymbol '!'
      , rsymbolScroll     = toContentSymbol '?'
                              -- also book, note, tablet, card
      , rsymbolTorsoArmor = toContentSymbol '['
      , rsymbolMiscArmor  = toContentSymbol '['
      , rsymbolClothes    = toContentSymbol '['
      , rsymbolShield     = toContentSymbol ']'
      , rsymbolPolearm    = toContentSymbol ')'
      , rsymbolEdged      = toContentSymbol ')'
      , rsymbolHafted     = toContentSymbol ')'
      , rsymbolWand       = toContentSymbol '/'
                              -- also magical rod, pistol, instrument
      , rsymbolFood       = toContentSymbol ','
          -- also body part; distinct enough from floor, which is middle dot
      }
  }
