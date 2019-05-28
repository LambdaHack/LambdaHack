{-# LANGUAGE TemplateHaskell #-}
-- | The default screen layout and features definition.
module Client.UI.Content.Screen
  ( standardLayoutAndFeatures
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Language.Haskell.TH.Syntax
import           System.IO

import Game.LambdaHack.Client.UI.Content.Screen

-- | Description of default screen layout and features.
standardLayoutAndFeatures :: ScreenContent
standardLayoutAndFeatures = ScreenContent
  { rwidth = 80
  , rheight = 24
  , rmainMenuArt =
      [ "         LambdaHack "
      , ""
      , "    <lambdahack.github.io>" ]
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
              ["", ""] ++ par1 ++ [""] ++ par2 ++ ["", ""]
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rmoveKeysScreen = $(do
      let path = "GameDefinition/MoveKeys.txt"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      lift $ lines x)
  , rapplyVerbMap = EM.fromList [('!', "quaff"), (',', "eat"), ('?', "read")]
  }
