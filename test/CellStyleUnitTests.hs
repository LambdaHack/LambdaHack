-- | Tests for CellStyle, the per-cell drawing decisions shared by the
-- frontends, and for MouseCursor, the cursor they share, and the freshness
-- cases of the files in @ts-src/src/generated/@ that are computed from the
-- two.
module CellStyleUnitTests (cellStyleUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

#ifndef USE_BROWSER
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Text as T
import           Text.Printf (printf)
#endif

import           Game.LambdaHack.Client.UI.Frontend
  ( CellGlyphs (..)
  , CellStyle (..)
  , CursorPixel (..)
  , backlightRGB
  , rgbChannels
  , rowColor
  , scalableGlyphs
  , styleCell
  , xhairCursorHotspot
  , xhairCursorPixels
  , xhairCursorSize
  )
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

#ifndef USE_BROWSER
import           Game.LambdaHack.Common.Area (punindex)
import           Game.LambdaHack.Common.Point (Point (..))
import qualified Game.LambdaHack.Content.RuleKind as RK

import qualified Content.RuleKind

import FreshnessCase
#endif

cellStyleUnitTests :: TestTree
cellStyleUnitTests = testGroup "cellStyleUnitTests" $
  [ testCase "rowColor draws White as AltWhite on even rows only" $ do
      rowColor 0 Color.White @?= Color.AltWhite
      rowColor 2 Color.White @?= Color.AltWhite
      rowColor 1 Color.White @?= Color.White
  , testCase "rowColor leaves every other colour alone on even rows" $
      forM_ [minBound .. maxBound] $ \fg ->
        when (fg /= Color.White) $
          assertEqual (show fg) fg (rowColor 0 fg)
  , testCase "styleCell recolours by row" $ do
      csFg (styleCell scalableGlyphs 0 $ cell Color.White '@') @?= Color.AltWhite
      csFg (styleCell scalableGlyphs 1 $ cell Color.White '@') @?= Color.White
  , testCase "styleCell draws a space as the frontend's space" $ do
      csGlyph (styleCell scalableGlyphs 1 $ cell Color.White ' ') @?= ' '
      csGlyph (styleCell scalableGlyphs {cgSpace = '_'} 1
               $ cell Color.White ' ') @?= '_'
  , testCase "styleCell draws a dim floor as the frontend's dim floor" $ do
      csGlyph (styleCell scalableGlyphs 1 $ cell Color.White floorSymbol)
        @?= '\x22C5'
      csGlyph (styleCell scalableGlyphs 1 $ cell Color.BrBlack floorSymbol)
        @?= '\x22C5'
      csGlyph (styleCell sdlGlyphs 1 $ cell Color.BrBlack floorSymbol)
        @?= '\x0007'
  , testCase "styleCell keeps a bright floor" $
      csGlyph (styleCell scalableGlyphs 1 $ cell Color.BrWhite floorSymbol)
        @?= floorSymbol
  , -- Not a pin of whether the brightness is judged before or after the
    -- row recolour, which no input distinguishes, White and AltWhite both
    -- being dim.
    testCase "styleCell dims a White floor on an even row too" $
      csGlyph (styleCell scalableGlyphs 0 $ cell Color.White floorSymbol)
        @?= '\x22C5'
  , testCase "styleCell passes an ordinary glyph through" $
      csGlyph (styleCell scalableGlyphs 1 $ cell Color.BrRed '@') @?= '@'
  , testCase "styleCell backlights HighlightBackground and nothing else" $
      forM_ [minBound .. maxBound] $ \bg ->
        assertEqual (show bg) (bg == Color.HighlightBackground)
                    (csBacklit $ styleCell scalableGlyphs 1 $ highlit bg)
  , testCase "styleCell outlines none of None, Background and NoneCursor" $
      forM_ unoutlined $ \bg ->
        assertEqual (show bg) Nothing
                    (csBorder $ styleCell scalableGlyphs 1 $ highlit bg)
  , testCase "styleCell outlines every other kind in its highlightToColor" $
      forM_ [minBound .. maxBound] $ \bg ->
        unless (bg `elem` unoutlined) $
          assertEqual (show bg) (Just $ Color.highlightToColor bg)
                      (csBorder $ styleCell scalableGlyphs 1 $ highlit bg)
  , testCase "backlightRGB is a third of BrBlack in every channel" $ do
      let (r, g, b) = rgbChannels $ Color.colorToRGB Color.BrBlack
      rgbChannels backlightRGB @?= (r `div` 3, g `div` 3, b `div` 3)
  , -- Forcing xhairCursorSize also runs its assertion that the art's rows
    -- are of one width.
    testCase "xhairCursorSize is whole bytes per row for SDL" $
      fst xhairCursorSize `mod` 8 @?= 0
  , testCase "xhairCursorHotspot is the centre the art is symmetric about" $ do
      let (x, y) = xhairCursorHotspot
      forM_ [1 .. y] $ \k ->
        xhairCursorPixels !! (y - k) @?= xhairCursorPixels !! (y + k)
      forM_ xhairCursorPixels $ \row ->
        forM_ [1 .. x] $ \k -> row !! (x - k) @?= row !! (x + k)
      xhairCursorPixels !! y !! x @?= CursorWhite
  ]
  ++ freshnessTests

-- | The SDL frontend's substitute glyphs under a bitmap map font.
sdlGlyphs :: CellGlyphs
sdlGlyphs = CellGlyphs {cgDimFloor = '\x0007', cgSpace = ' '}

-- | The highlights no outline is drawn for.
unoutlined :: [Color.Highlight]
unoutlined =
  [Color.HighlightNone, Color.HighlightBackground, Color.HighlightNoneCursor]

cell :: Color.Color -> Char -> Color.AttrCharW32
cell = Color.attrChar2ToW32

highlit :: Color.Highlight -> Color.AttrCharW32
highlit bg = attrCell Color.BrCyan bg '@'

attrCell :: Color.Color -> Color.Highlight -> Char -> Color.AttrCharW32
attrCell fg bg c = Color.attrCharToW32 $ Color.AttrChar (Color.Attr fg bg) c

-- | The freshness cases of the files computed here.
freshnessTests :: [TestTree]
#ifndef USE_BROWSER
freshnessTests =
  [ fresh "palette.ts" paletteTs
  , fresh "fixtures.json" fixturesJson
  , fresh "cursor.ts" cursorTs
  ]
 where
  fresh = freshnessCase source

source :: FilePath
source = "CellStyleUnitTests.hs"

-- | The palette @terminal-core.ts@ renders colours with.
paletteTs :: Text
paletteTs = T.unlines $
  generatedHeader source
  ++ [ ""
     , "// Color enum index -> hex (Definition/Color.hs colorToRGB),"
     , "// indexed by fromEnum."
     , "export const PALETTE: readonly string[] = [" ]
  ++ [ "  \"" <> Color.colorToRGB c <> "\", // " <> tshow c
     | c <- [minBound :: Color.Color .. maxBound] ]
  ++ [ "];"
     , ""
     , "// Highlight enum index -> Color enum index"
     , "// (Definition/Color.hs highlightToColor)."
     , "export const HIGHLIGHT_TO_COLOR: readonly number[] = [" ]
  ++ [ "  " <> tshow (fromEnum c) <> ", // " <> tshow hi <> " -> " <> tshow c
     | hi <- [minBound .. maxBound]
     , let c = Color.highlightToColor hi ]
  ++ [ "];"
     , ""
     , "// The fill of a cell under HighlightBackground"
     , "// (Frontend/CellStyle.hs backlightRGB)."
     , "export const BACKLIGHT_FILL = \"" <> backlightRGB <> "\";"
     , ""
     , "// The glyph of a dim floor (Frontend/CellStyle.hs scalableGlyphs)."
     , "export const DIM_FLOOR = "
       <> jsonString (T.singleton $ cgDimFloor scalableGlyphs) <> ";"
     , ""
     , "// Color and Highlight enum indices by constructor"
     , "// (Definition/Color.hs), as fromEnum gives them."
     , "export const COLOR = {" ]
  ++ indices ([minBound .. maxBound] :: [Color.Color])
  ++ [ "} as const;"
     , ""
     , "export const HIGHLIGHT = {" ]
  ++ indices ([minBound .. maxBound] :: [Color.Highlight])
  ++ [ "} as const;" ]
 where
  indices :: (Show a, Enum a) => [a] -> [Text]
  indices l = [ "  " <> tshow c <> ": " <> tshow (fromEnum c) <> "," | c <- l ]

-- | Fixtures that @terminal-core.test.ts@ asserts @styledCell@ and
-- @cellPosition@ against: 'styleCell' on 'scalableGlyphs', rendered
-- to hex as @styledCell@ renders it, and 'punindex' at the sample game's
-- 'RK.rWidthMax', which the engine's content validation makes the screen
-- width and @TieKnot@ makes the width @toEnum@ decodes with.
fixturesJson :: Text
fixturesJson = jsonDocument source
  [ ("cells", map cellFixture cellSample)
  , ("positions", map positionFixture positionSample) ]
 where
  cellFixture (row, w) =
    let CellStyle{..} = styleCell scalableGlyphs row w
    in jsonObject
         [ ("w", tshow $ Color.attrCharW32 w)
         , ("row", tshow row)
         , ("char", jsonString $ T.singleton csGlyph)
         , ("color", jsonString $ Color.colorToRGB csFg)
         , ("background", jsonString $ if csBacklit
                                       then backlightRGB
                                       else Color.colorToRGB Color.Black)
         , ("border", maybe "null" (jsonString . Color.colorToRGB) csBorder) ]
  width = RK.rWidthMax Content.RuleKind.standardRules
  positionFixture i =
    let Point{..} = punindex width i
    in jsonObject [ ("i", tshow i), ("width", tshow width)
                  , ("col", tshow px), ("row", tshow py) ]
  positionSample = [0, 1, width - 1, width, width + 1, 2 * width - 1]
                   ++ [5 * width + 7]

-- | The cursor the browser is to show over the map, as SDL shows it.
cursorTs :: Text
cursorTs = T.unlines $
  generatedHeader source
  ++ [ ""
     , "// The crosshair mouse cursor (Frontend/MouseCursor.hs"
     , "// xhairCursorPixels) as an SVG data URI, and its hotspot, the pixel"
     , "// at the mouse position."
     , "export const XHAIR_CURSOR_URL ="
     , "  \"data:image/svg+xml,"
       <> T.concatMap uriEscape xhairCursorSvg <> "\";"
     , "export const XHAIR_CURSOR_HOTSPOT_X = " <> tshow hotspotX <> ";"
     , "export const XHAIR_CURSOR_HOTSPOT_Y = " <> tshow hotspotY <> ";" ]
 where
  (hotspotX, hotspotY) = xhairCursorHotspot

-- | 'xhairCursorPixels' as SVG: a path per colour, a one-pixel-high
-- rectangle per run of that colour's pixels in a row.
xhairCursorSvg :: Text
xhairCursorSvg =
  let (width, height) = xhairCursorSize
      path colour pixel =
        "<path fill='" <> colour <> "' d='"
        <> T.concat [ "M" <> tshow x <> "," <> tshow y <> "h" <> tshow n
                      <> "v1h-" <> tshow n <> "z"
                    | (y, row) <- zip [0 :: Int ..] xhairCursorPixels
                    , (x, n) <- pixelRuns pixel row ]
        <> "'/>"
  in "<svg xmlns='http://www.w3.org/2000/svg' width='" <> tshow width
     <> "' height='" <> tshow height <> "' shape-rendering='crispEdges'>"
     <> path "#000000" CursorBlack <> path "#FFFFFF" CursorWhite
     <> "</svg>"

-- | The start and length of each run of the pixel in a row.
pixelRuns :: CursorPixel -> [CursorPixel] -> [(Int, Int)]
pixelRuns pixel = go 0
 where
  go _ [] = []
  go x row@(p : _) =
    let (run, rest) = span (== p) row
        n = length run
    in [(x, n) | p == pixel] ++ go (x + n) rest

-- | A character percent-encoded where a data URI that CSS takes unquoted
-- needs it.
uriEscape :: Char -> Text
uriEscape c
  | c `elem` uriSafe = T.singleton c
  | otherwise = assert (n < 0x80) $ T.pack $ printf "%%%02X" n
 where
  n = fromEnum c
  uriSafe = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_.~/:=,;"

-- | Cells for each glyph rule and each highlight, on an even row
-- and an odd, and for each colour, each once.
cellSample :: [(Int, Color.AttrCharW32)]
cellSample = nubOrd $
  [ (row, cell fg c) | c <- ['@', ' ', floorSymbol]
                     , fg <- [Color.White, Color.BrBlack, Color.BrWhite]
                     , row <- [0, 1] ]
  ++ [ (1, cell fg '@') | fg <- [minBound .. maxBound] ]
  ++ [ (row, attrCell Color.White bg '@')
     | bg <- [minBound .. maxBound], row <- [0, 1] ]

#else
freshnessTests = []
#endif
