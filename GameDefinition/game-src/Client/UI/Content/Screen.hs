{-# LANGUAGE TemplateHaskell #-}
-- | The default screen layout and features definition.
module Client.UI.Content.Screen
  ( standardLayoutAndFeatures
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.FileEmbed (embedDir)
import           Language.Haskell.TH.Syntax
import           System.IO

import Game.LambdaHack.Client.UI.Content.Screen

-- | Description of default screen layout and features.
standardLayoutAndFeatures :: ScreenContent
standardLayoutAndFeatures = ScreenContent
  { rwidth = 80
  , rheight = 24
  , rwrap = 80  -- not enough height to fit longer messages if wrapped early
  , rwebAddress = "http://lambdahack.github.io"
  , rintroScreen = $(do
      let path = "GameDefinition/PLAYING.md"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      let paragraphs :: [String] -> [String] -> [[String]]
          paragraphs [] rows = [reverse rows]
          paragraphs (l : ls) rows = if null l
                                     then reverse rows : paragraphs ls []
                                     else paragraphs ls (l : rows)
          intro = case paragraphs (lines x) [] of
            _title : _blurb : par1 : par2 : _rest ->
              par1 ++ [""] ++ par2
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rapplyVerbMap = EM.fromList [('!', "quaff"), (',', "eat"), ('?', "read")]
  , rFontFiles =
-- Checking USE_SDL would be more accurate, but would require complicating
-- .cabal file and slightly larger vty executable is not a problem.
#ifdef USE_JSFILE
      []
#else
      $(embedDir "GameDefinition/fonts")
#endif
  }
