{-# LANGUAGE TemplateHaskell #-}
-- | The default screen layout and features definition.
module Client.UI.Content.Screen
  ( standardLayoutAndFeatures
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Language.Haskell.TH.Syntax
import System.IO (readFile)

import Game.LambdaHack.Client.UI.Content.Screen

-- | Description of default screen layout and features.
standardLayoutAndFeatures :: ScreenContentData
standardLayoutAndFeatures = ScreenContentData
  { rwidth = 80
  , rheight = 24
  -- ASCII art for the main menu. Only pure 7-bit ASCII characters are allowed.
  -- When displayed in the main menu screen, the picture is overwritten
  -- with game and engine version strings and keybindings.
  -- The keybindings overwrite places marked with left curly brace signs.
  -- This sign is forbidden anywhere else in the picture.
  -- The picture and the whole main menu is displayed dull white on black.
  -- The glyphs, or at least the character cells, are perfect squares.
  -- The picture should be exactly 45 rows by 80 columns.
  -- For larger screen sizes, the picture is centered and padded with spaces.
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
  }
