-- | Tests for CellStyle, the per-cell drawing decisions shared
-- by the frontends, and for MouseCursor, the cursor they share,
-- and the freshness case for the files in @ts-src/src/generated/@
-- that are computed from the two (@docs/wasm-frontend-unified-plan.md@, 0.2).
module CellStyleUnitTests (cellStyleUnitTests, freshnessOptions) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Char (digitToInt)
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Test.Tasty.Options

#ifndef USE_BROWSER
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import           Numeric (showHex)
import           System.Directory (doesFileExist)
#endif

import           Game.LambdaHack.Client.UI.Frontend
  ( CellGlyphs (..)
  , CellStyle (..)
  , CursorPixel (..)
  , backlightRGB
  , rowColor
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
#endif

cellStyleUnitTests :: TestTree
cellStyleUnitTests = testGroup "cellStyleUnitTests" $
  [ testCase "rowColor draws White as AltWhite on even rows only" $ do
      rowColor 0 Color.White @?= Color.AltWhite
      rowColor 2 Color.White @?= Color.AltWhite
      rowColor 1 Color.White @?= Color.White
  , testCase "rowColor leaves every other colour alone on even rows" $
      forM_ allColors $ \fg ->
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
  , testCase "backlightRGB is a third of BrBlack in every channel" $
      hexChannels backlightRGB
        @?= map (`div` 3) (hexChannels $ Color.colorToRGB Color.BrBlack)
  , testCase "xhairCursorSize is the art's, in whole bytes per row for SDL" $ do
      let (width, height) = xhairCursorSize
      length xhairCursorPixels @?= height
      forM_ xhairCursorPixels $ \row -> length row @?= width
      width `mod` 8 @?= 0
  , testCase "xhairCursorHotspot is the centre the art is symmetric about" $ do
      let (x, y) = xhairCursorHotspot
      forM_ [1 .. y] $ \k ->
        xhairCursorPixels !! (y - k) @?= xhairCursorPixels !! (y + k)
      forM_ xhairCursorPixels $ \row ->
        forM_ [1 .. x] $ \k -> row !! (x - k) @?= row !! (x + k)
      xhairCursorPixels !! y !! x @?= CursorWhite
  ]
  ++ freshnessTests

-- | The browser's substitute glyphs, as @terminal-core.ts@ draws them.
browserGlyphs :: CellGlyphs
browserGlyphs = CellGlyphs {cgDimFloor = '\x22C5', cgSpace = '\x00A0'}

-- | The SDL frontend's substitute glyphs under a bitmap map font.
sdlGlyphs :: CellGlyphs
sdlGlyphs = CellGlyphs {cgDimFloor = '\x0007', cgSpace = ' '}

-- | Every colour, 'Color.Color' having no 'Bounded' instance.
allColors :: [Color.Color]
allColors = [Color.Black .. Color.BrWhite]

cell :: Color.Color -> Char -> Color.AttrCharW32
cell fg = attrCell fg Color.HighlightNone

-- | The red, green and blue of a @#RRGGBB@ colour.
hexChannels :: Text -> [Int]
hexChannels t = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2] -> map channel [(r1, r2), (g1, g2), (b1, b2)]
  _ -> error $ "not #RRGGBB: " ++ show t
 where
  channel (high, low) = 16 * digitToInt high + digitToInt low

highlit :: Color.Highlight -> Color.AttrCharW32
highlit bg = attrCell Color.BrCyan bg '@'

attrCell :: Color.Color -> Color.Highlight -> Char -> Color.AttrCharW32
attrCell fg bg c = Color.attrCharToW32 $ Color.AttrChar (Color.Attr fg bg) c

-- | The freshness case's switch, @--regenerate@, under which it rewrites
-- the committed files rather than comparing them.
newtype Regenerate = Regenerate Bool

instance IsOption Regenerate where
  defaultValue = Regenerate False
  parseValue = fmap Regenerate . safeReadBool
  optionName = return "regenerate"
  optionHelp = return "Rewrite the files in ts-src/src/generated/"
  optionCLParser = flagCLParser Nothing (Regenerate True)

-- | The options the freshness case reads, for the suite's ingredients.
freshnessOptions :: [OptionDescription]
freshnessOptions = [Option (Proxy :: Proxy Regenerate)]

-- | The freshness case, one test per file: each committed file
-- in @ts-src/src/generated/@ against what it is computed from here,
-- since @make build-ts@ bundles the committed copies as they stand.
-- None in a browser build, which runs under @make test-wasm@, away
-- from the files.
freshnessTests :: [TestTree]
#ifndef USE_BROWSER
freshnessTests =
  [ freshnessCase "palette.ts" paletteTs
  , freshnessCase "fixtures.json" fixturesJson
  , freshnessCase "cursor.ts" cursorTs
  ]

freshnessCase :: FilePath -> Text -> TestTree
freshnessCase name content = askOption $ \(Regenerate regenerate) ->
  testCase ("generated " ++ name ++ " is fresh") $ do
    -- Relative to the directory the .cabal file is in, where cabal runs
    -- the suite, and which the SDL fontset cases rely on too.
    let path = "ts-src/src/generated/" ++ name
        bytes = T.encodeUtf8 content
    exists <- doesFileExist path
    committed <- if exists then Just <$> BS.readFile path else return Nothing
    if | committed == Just bytes -> return ()
       | regenerate -> BS.writeFile path bytes
       | otherwise ->
         assertFailure $ path ++ " differs from what CellStyleUnitTests.hs"
                         ++ " computes; rewrite it with"
                         ++ " cabal test --test-options=--regenerate"

-- | The header of a generated TypeScript file.
generatedHeader :: [Text]
generatedHeader =
  [ "// Generated by the freshness case in test/CellStyleUnitTests.hs, which"
  , "// fails while this file differs from what it computes; rewrite it with"
  , "// cabal test --test-options=--regenerate, not by hand."
  ]

-- | The palette @terminal-core.ts@ renders colours with.
paletteTs :: Text
paletteTs = T.unlines $
  generatedHeader
  ++ [ ""
     , "// Color enum index -> hex (Definition/Color.hs colorToRGB),"
     , "// indexed by fromEnum."
     , "export const PALETTE: readonly string[] = [" ]
  ++ [ "  \"" <> Color.colorToRGB c <> "\", // " <> tshow c | c <- allColors ]
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
     , "export const BACKLIGHT_FILL = \"" <> backlightRGB <> "\";" ]

-- | Fixtures that @terminal-core.test.ts@ asserts @styledCell@ and
-- @cellPosition@ against: 'styleCell' on the browser's glyphs, rendered
-- to hex as @styledCell@ renders it, and 'punindex' at the sample game's
-- 'RK.rWidthMax', which the engine's content validation makes the screen
-- width and @TieKnot@ makes the width @toEnum@ decodes with.
fixturesJson :: Text
fixturesJson = T.unlines $
  [ "{"
  , "  \"source\": "
    <> jsonString "test/CellStyleUnitTests.hs, the freshness case"
    <> ","
  , "  \"cells\": [" ]
  ++ jsonItems (map cellFixture cellSample)
  ++ [ "  ],"
     , "  \"positions\": [" ]
  ++ jsonItems (map positionFixture positionSample)
  ++ [ "  ]"
     , "}" ]
 where
  cellFixture (row, w) =
    let CellStyle{..} = styleCell browserGlyphs row w
    in jsonObject
         [ ("w", tshow $ Color.attrCharW32 w)
         , ("row", tshow row)
         , ("char", jsonString $ T.singleton csGlyph)
         , ("color", jsonString $ Color.colorToRGB csFg)
         , ("background", jsonString $ if csBacklit
                                       then backlightRGB
                                       else Color.colorToRGB Color.Black)
         , ("border", jsonString $ Color.colorToRGB csBorder) ]
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
  generatedHeader
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
  | otherwise = assert (n < 0x80) $
      T.pack ['%', hexDigits !! (n `div` 16), hexDigits !! (n `mod` 16)]
 where
  n = fromEnum c
  uriSafe = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_.~/:=,;"
  hexDigits = "0123456789ABCDEF"

-- | Cells for each glyph rule and each highlight, on an even row
-- and an odd, and for each colour.
cellSample :: [(Int, Color.AttrCharW32)]
cellSample =
  [ (row, cell fg c) | c <- ['@', ' ', floorSymbol]
                     , fg <- [Color.White, Color.BrBlack, Color.BrWhite]
                     , row <- [0, 1] ]
  ++ [ (1, cell fg '@') | fg <- allColors ]
  ++ [ (row, attrCell Color.White bg '@')
     | bg <- [minBound .. maxBound], row <- [0, 1] ]

jsonItems :: [Text] -> [Text]
jsonItems items =
  zipWith (\item comma -> "    " <> item <> comma) items
          (replicate (length items - 1) "," ++ [""])

jsonObject :: [(Text, Text)] -> Text
jsonObject fields =
  "{" <> T.intercalate ", " [jsonString k <> ": " <> v | (k, v) <- fields]
  <> "}"

-- | A JSON string, kept ASCII by escaping everything else.
jsonString :: Text -> Text
jsonString t = "\"" <> T.concatMap escape t <> "\""
 where
  escape c
    | c == '"' || c == '\\' = T.pack ['\\', c]
    | c >= ' ' && c <= '~' = T.singleton c
    | otherwise =
      let hex = showHex (fromEnum c) ""
      in assert (fromEnum c <= 0xFFFF) $
           T.pack $ "\\u" ++ replicate (4 - length hex) '0' ++ hex
#else
freshnessTests = []
#endif
