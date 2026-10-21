-- | Tests for CellStyle, the per-cell drawing decisions shared
-- by the frontends (@docs/wasm-frontend-unified-plan.md@, 0.2).
module CellStyleUnitTests (cellStyleUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.Frontend
  (CellGlyphs (..), CellStyle (..), rowColor, styleCell)
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

cellStyleUnitTests :: TestTree
cellStyleUnitTests = testGroup "cellStyleUnitTests"
  [ testCase "rowColor draws White as AltWhite on even rows only" $ do
      rowColor 0 Color.White @?= Color.AltWhite
      rowColor 2 Color.White @?= Color.AltWhite
      rowColor 1 Color.White @?= Color.White
  , testCase "rowColor leaves every other colour alone on even rows" $
      forM_ [Color.Black .. Color.BrWhite] $ \fg ->
        when (fg /= Color.White) $
          assertEqual (show fg) fg (rowColor 0 fg)
  , testCase "styleCell recolours by row" $ do
      csFg (styleCell browserGlyphs 0 $ cell Color.White '@') @?= Color.AltWhite
      csFg (styleCell browserGlyphs 1 $ cell Color.White '@') @?= Color.White
  , testCase "styleCell draws a space as the frontend's space" $ do
      csGlyph (styleCell browserGlyphs 1 $ cell Color.White ' ') @?= '\x00A0'
      csGlyph (styleCell sdlGlyphs 1 $ cell Color.White ' ') @?= ' '
  , testCase "styleCell draws a dim floor as the frontend's dim floor" $ do
      csGlyph (styleCell browserGlyphs 1 $ cell Color.White floorSymbol)
        @?= '\x22C5'
      csGlyph (styleCell browserGlyphs 1 $ cell Color.BrBlack floorSymbol)
        @?= '\x22C5'
      csGlyph (styleCell sdlGlyphs 1 $ cell Color.BrBlack floorSymbol)
        @?= '\x0007'
  , testCase "styleCell keeps a bright floor" $
      csGlyph (styleCell browserGlyphs 1 $ cell Color.BrWhite floorSymbol)
        @?= floorSymbol
  , testCase "styleCell judges the floor's brightness after the row recolour" $
      csGlyph (styleCell browserGlyphs 0 $ cell Color.White floorSymbol)
        @?= '\x22C5'
  , testCase "styleCell passes an ordinary glyph through" $
      csGlyph (styleCell browserGlyphs 1 $ cell Color.BrRed '@') @?= '@'
  , testCase "styleCell backlights HighlightBackground and nothing else" $
      forM_ [minBound .. maxBound] $ \bg ->
        assertEqual (show bg) (bg == Color.HighlightBackground)
                    (csBacklit $ styleCell browserGlyphs 1 $ highlit bg)
  , testCase "styleCell outlines None, Background and NoneCursor in black" $
      forM_ [ Color.HighlightNone, Color.HighlightBackground
            , Color.HighlightNoneCursor ] $ \bg ->
        assertEqual (show bg) Color.Black
                    (csBorder $ styleCell browserGlyphs 1 $ highlit bg)
  , testCase "styleCell outlines every other kind in its highlightToColor" $
      forM_ [minBound .. maxBound] $ \bg ->
        unless (bg `elem` [ Color.HighlightNone, Color.HighlightBackground
                          , Color.HighlightNoneCursor ]) $
          assertEqual (show bg) (Color.highlightToColor bg)
                      (csBorder $ styleCell browserGlyphs 1 $ highlit bg)
  ]

-- | The browser's substitute glyphs, as @terminal-core.ts@ draws them.
browserGlyphs :: CellGlyphs
browserGlyphs = CellGlyphs {cgDimFloor = '\x22C5', cgSpace = '\x00A0'}

-- | The SDL frontend's substitute glyphs under a bitmap map font.
sdlGlyphs :: CellGlyphs
sdlGlyphs = CellGlyphs {cgDimFloor = '\x0007', cgSpace = ' '}

cell :: Color.Color -> Char -> Color.AttrCharW32
cell fg c =
  Color.attrCharToW32 $ Color.AttrChar (Color.Attr fg Color.HighlightNone) c

highlit :: Color.Highlight -> Color.AttrCharW32
highlit bg =
  Color.attrCharToW32 $ Color.AttrChar (Color.Attr Color.BrCyan bg) '@'
