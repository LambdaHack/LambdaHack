-- | Tests for OverlayLayout, the overlay layout shared by the frontends,
-- and the freshness cases for the files in @ts-src/src/generated/@ that pin
-- any re-implementation of its proportional pen and any decoder of its
-- words on the wire (@docs/wasm-frontend-unified-plan.md@, 2.1 and 2.3).
module OverlayLayoutUnitTests (overlayLayoutUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Exception (AssertionFailed, evaluate, try)
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Test.Tasty.QuickCheck
  ( Gen
  , choose
  , conjoin
  , counterexample
  , elements
  , forAll
  , frequency
  , listOf
  , testProperty
  , (===)
  )

import           Game.LambdaHack.Client.UI.Frame
  (OverlaySpace, SingleFrame (..))
import           Game.LambdaHack.Client.UI.Frontend
  ( CellLayer (..)
  , CellRun (..)
  , FrameLayout (..)
  , PropChunk (..)
  , PropCursor
  , PropFit (..)
  , PropFont (..)
  , PropLine (..)
  , chunkPropLine
  , decodeFrameLayout
  , encodeFrameLayout
  , layOutFrame
  , layOutMono
  , layOutSquare
  , lmBoxSize
  , mkLayoutMetrics
  , pcAllSpace
  , pcFont
  , propCutoff
  , propFit
  , propLineStartFits
  , rowColor
  , startPropLine
  , trimmedMarkerRun
  )
import           Game.LambdaHack.Client.UI.Overlay
  (AttrString, attrStringToString)
import           Game.LambdaHack.Client.UI.PointUI (PointUI (..))
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Definition.Color as Color

#ifndef USE_BROWSER
import FreshnessCase
  (freshnessCase, jsonBool, jsonDocument, jsonObject, jsonString)
#endif

overlayLayoutUnitTests :: TestTree
overlayLayoutUnitTests = testGroup "overlayLayoutUnitTests" $
  chunkingTests ++ layoutTests ++ placementTests ++ propertyTests
  ++ wireTests ++ freshnessTests

chunkingTests :: [TestTree]
chunkingTests =
  [ testCase "chunkPropLine: the empty line has no chunks" $
      chunkPropLine 1 [] @?= []
  , testCase "chunkPropLine: one character is one chunk" $
      chunkPropLine 1 (str Color.Red "a") @?= [PropChunk Color.Red "a"]
  , testCase "chunkPropLine: leading spaces take the next character's colour" $
      chunkPropLine 1 ([sp, sp] ++ str Color.Red "ab")
        @?= [PropChunk Color.Red "  ab"]
  , testCase "chunkPropLine: trailing spaces join the run to their left" $
      chunkPropLine 1 (str Color.Red "ab" ++ [sp, sp])
        @?= [PropChunk Color.Red "ab  "]
  , testCase "chunkPropLine: spaces between two colours join the left run" $
      chunkPropLine 1 (str Color.Red "a" ++ [sp, sp] ++ str Color.Blue "b")
        @?= [PropChunk Color.Red "a  ", PropChunk Color.Blue "b"]
  , testCase "chunkPropLine: an all-space line is one chunk, in their colour" $
      chunkPropLine 1 [sp, sp, sp] @?= [PropChunk Color.White "   "]
  , testCase "chunkPropLine: a colour change with no space splits the line" $
      chunkPropLine 1 (str Color.Red "a" ++ str Color.Blue "b")
        @?= [PropChunk Color.Red "a", PropChunk Color.Blue "b"]
  , testCase "chunkPropLine: coloured spaces are a colour, and all-space" $ do
      -- The chunker's spaces are 'Color.spaceAttrW32' alone, while 'pcAllSpace'
      -- asks 'Char.isSpace' of the text; unifying the two would change which
      -- lines get the trimmed-line marker.
      let chunks = chunkPropLine 1 $ str Color.Red "a" ++ str Color.Blue "  "
                                     ++ str Color.Red "b"
      chunks @?= [ PropChunk Color.Red "a", PropChunk Color.Blue "  "
                 , PropChunk Color.Red "b" ]
      map pcAllSpace chunks @?= [False, True, False]
  , testCase
      "chunkPropLine: White is AltWhite on even rows, regular on both" $ do
      let chunksOn row = chunkPropLine row (str Color.White "a")
      chunksOn 0 @?= [PropChunk Color.AltWhite "a"]
      chunksOn 1 @?= [PropChunk Color.White "a"]
      map pcFont (chunksOn 0 ++ chunksOn 1) @?= [PropRegular, PropRegular]
  , testCase "pcFont: bold below White and for BrBlack, regular otherwise" $
      forM_ [minBound .. maxBound] $ \fg ->
        assertEqual (show fg)
                    (if fg `elem` [ Color.White, Color.AltWhite, Color.BrRed
                                  , Color.BrGreen, Color.BrYellow, Color.BrBlue
                                  , Color.BrMagenta, Color.BrCyan
                                  , Color.BrWhite ]
                     then PropRegular
                     else PropBold)
                    (pcFont $ PropChunk fg "a")
  , testCase "chunkPropLine asserts the highlight of a chunk's colour" $
      assertAsserts $ chunkPropLine 1
        [Color.attrCharToW32 $ Color.AttrChar backlit 'a']
  ]

layoutTests :: [TestTree]
layoutTests =
  [ testCase "layOutFrame drops a line starting at the last column" $
      map plStartX (flProp $ layOutFrame 10 $ frame
        [(PointUI 17 0, str Color.Red "a"), (PointUI 18 1, str Color.Red "b")]
        [] [])
        @?= [17]
  , testCase "layOutFrame lays each overlay out in its own section" $
      layOutFrame 10 (frame [(PointUI 0 0, str Color.Red "p")]
                            [(PointUI 2 1, str Color.Red "s")]
                            [(PointUI 3 2, str Color.Red "m")])
        @?= FrameLayout
              { flProp = [PropLine 0 0 [PropChunk Color.Red "p"]]
              , flSquare = [CellRun LayerSquare 1 1 (str Color.Red "s")]
              , flMono = [CellRun LayerMono 2 3 (str Color.Red "m")] }
  , testCase "layOutSquare cuts a line at the screen's width in squares" $
      layOutSquare 10 [(PointUI 14 0, str Color.Red "abcde")]
        @?= [CellRun LayerSquare 0 7 (str Color.Red "abc")]
  , testCase "layOutMono cuts a line at the screen's width in half-cells" $
      layOutMono 10 [(PointUI 17 0, str Color.Red "abcde")]
        @?= [CellRun LayerMono 0 17 (str Color.Red "abc")]
  , testCase "layOutSquare and layOutMono drop the runs left empty" $ do
      layOutSquare 10 [(PointUI 20 0, str Color.Red "a"), (PointUI 0 1, [])]
        @?= []
      layOutMono 10 [(PointUI 20 0, str Color.Red "a"), (PointUI 0 1, [])]
        @?= []
  , testCase "layOutMono asserts the highlight of every cell drawn" $
      assertAsserts $ layOutMono 10
        [(PointUI 0 0, [ Color.attrCharToW32 $ Color.AttrChar backlit 'a'])]
  ]

-- | One row per branch of 'propCutoff' and 'propFit', at metrics whose last
-- column starts at 144 and whose screen ends at 160.
--
-- Each row fails when the line it exercises is perturbed, as run on this
-- module's first version: the cutoff's @>=@ weakened to @>@ fails the second
-- by its comparison, and the fifth, the seventh and the eighth by 'propFit''s
-- assertion that a cut chunk ends its line; strengthened to @pcurXPx + 1 >=@,
-- the first by its comparison and the eighth by 'propFit''s precondition; the
-- fit test's @<=@ weakened to @<@, the fourth; the fitting width replaced by
-- @remaining@, the third; the @- boxSize@ dropped, the fifth, the seventh
-- and the eighth; the @not allSpace@ flipped, the fifth to the eighth; and
-- the all-space cut moved a cell short, the sixth --- these five by their
-- comparisons.
placementTests :: [TestTree]
placementTests =
  [ testCase "propCutoff: a pen short of the last column goes on" $
      propCutoff lm (cursorAt 3 143) @?= False
  , testCase "propCutoff: a pen at the last column stops" $
      propCutoff lm (cursorAt 3 144) @?= True
  , testCase "propFit: a chunk narrower than the rest of the row fits" $
      propFit lm (cursorAt 3 100) letters 50
        @?= PropFit 100 50 Nothing (cursorAt 3 150)
  , testCase "propFit: a chunk exactly as wide as the rest of the row fits" $
      propFit lm (cursorAt 3 100) letters 60
        @?= PropFit 100 60 Nothing (cursorAt 3 160)
  , testCase "propFit: an overrun is cut a cell short, raising the marker" $
      propFit lm (cursorAt 3 100) letters 61
        @?= PropFit 100 44 (Just $ trimmedMarkerRun lm 3) (cursorAt 3 144)
  , testCase "propFit: an all-space overrun is cut at the edge, no marker" $
      propFit lm (cursorAt 3 100) spaces 61
        @?= PropFit 100 60 Nothing (cursorAt 3 160)
  , testCase "propFit: a cut chunk ends its line" $
      placeLine (PropLine 12 3 [letters, letters]) [70, 5]
        @?= [PropFit 96 48 (Just $ trimmedMarkerRun lm 3) (cursorAt 3 144)]
  , testCase "propFit: a cell and a pixel left cut one pixel wide" $
      propFit lm (cursorAt 3 143) letters 100
        @?= PropFit 143 1 (Just $ trimmedMarkerRun lm 3) (cursorAt 3 144)
  ]
 where
  lm = mkLayoutMetrics 8 10
  placeLine :: PropLine -> [Int] -> [PropFit]
  placeLine line = go (startPropLine lm line) . zip (plChunks line)
   where
    go _ [] = []
    go cur _ | propCutoff lm cur = []
    go cur ((chunk, widthRaw) : rest) =
      let fit = propFit lm cur chunk widthRaw
      in fit : go (pfNext fit) rest

propertyTests :: [TestTree]
propertyTests =
  [ testProperty "chunkPropLine is total" $
      forAll genRowLine $ \(row, al) ->
        concatMap (T.unpack . pcText) (chunkPropLine row al)
          === attrStringToString al
  , testProperty "chunkPropLine's chunks are single-coloured" $
      forAll genRowLine $ \(row, al) ->
        let chunks = chunkPropLine row al
            slices = splitPlaces (map (T.length . pcText) chunks) al
        in and [ rowColor row (Color.fgFromW32 ac) == pcColor c
                 || ac == Color.spaceAttrW32
               | (c, slice) <- zip chunks slices, ac <- slice ]
  , testProperty "chunkPropLine's chunks are non-empty and add up" $
      forAll genRowLine $ \(row, al) ->
        let lengths = map (T.length . pcText) $ chunkPropLine row al
        in all (> 0) lengths && sum lengths == length al
  , testProperty "propFit keeps a chunk on screen and the pen moving on" $
      forAll genPlacement $ \(h, w, x, allSpace, widthRaw) ->
        let lm = mkLayoutMetrics h w
            screen = w * lmBoxSize lm
            PropFit{..} = propFit lm (cursorAt 0 x) (chunkOf allSpace) widthRaw
        in conjoin
             [ counterexample "wider than measured" $ pfWidthPx <= widthRaw
             , counterexample "past the screen" $ pfXPx + pfWidthPx <= screen
             , counterexample "the marker's cell drawn over" $
                 isNothing pfMarker
                 || pfWidthPx < widthRaw
                    && pfXPx + pfWidthPx <= screen - lmBoxSize lm
             , counterexample "the pen not moved by the width" $
                 pfXPx == x && pfWidthPx >= 0
                 && pfNext == cursorAt 0 (x + pfWidthPx) ]
  , testProperty "propFit scales with the metrics" $
      forAll ((,) <$> genPlacement <*> choose (1, 10)) $
        \((h, w, x, allSpace, widthRaw), k) ->
          let fit = propFit (mkLayoutMetrics h w) (cursorAt 0 x)
                            (chunkOf allSpace) widthRaw
          in propFit (mkLayoutMetrics (k * h) w) (cursorAt 0 (k * x))
                     (chunkOf allSpace) (k * widthRaw)
             === PropFit { pfXPx = k * pfXPx fit
                         , pfWidthPx = k * pfWidthPx fit
                         , pfMarker = pfMarker fit
                         , pfNext = cursorAt 0 (k * (pfXPx fit
                                                     + pfWidthPx fit)) }
  , testProperty "propLineStartFits is propCutoff at a line's start" $
      forAll ((,,) <$> choose (1, 20) <*> choose (2, 100) <*> choose (0, 300)) $
        \(h, w, xUI) ->
          let lm = mkLayoutMetrics h w
          in propLineStartFits w xUI
             === not (propCutoff lm (startPropLine lm (PropLine xUI 0 [])))
  , testProperty "layOutSquare and layOutMono cut to the screen and no more" $
      forAll ((,) <$> choose (2, 30) <*> genOverlay) $ \(width, ov) ->
        let fits limit col al = col + length al <= limit && not (null al)
            squares = layOutSquare width ov
            monos = layOutMono width ov
        in conjoin
             [ counterexample "a square run past the screen" $
                 all (\CellRun{..} -> crCol + length crCells <= width) squares
             , counterexample "a mono run past the screen" $
                 all (\CellRun{..} -> crCol + length crCells <= 2 * width)
                     monos
             , counterexample "a square line that fits not drawn whole" $
                 and [ CellRun LayerSquare row (x `div` 2) al `elem` squares
                     | (PointUI x row, al) <- ov
                     , fits width (x `div` 2) al ]
             , counterexample "a mono line that fits not drawn whole" $
                 and [ CellRun LayerMono row x al `elem` monos
                     | (PointUI x row, al) <- ov
                     , fits (2 * width) x al ] ]
  ]

wireTests :: [TestTree]
wireTests =
  [ testCase "encodeFrameLayout writes the sections in order, word by word" $
      wireWords
        @?= [ 1, 1, 1                -- a line, a square run and a mono run
            , 2, 0, 2                -- the line, at x 2 on row 0, two chunks:
            , 0, 8, 0, 2, 104, 233   -- regular, AltWhite, letters, "h\233"
            , 1, 4, 1, 2, 32, 32     -- bold, Blue, all space, two spaces
            , 1, 2, 1, cell Color.Red 's'    -- row 1, column 2, a cell
            , 2, 3, 1, cell Color.Green 'm'  -- row 2, half-cell 3, a cell
            ]
  , testCase "encodeFrameLayout writes no overlays as three zero counts" $
      encodeFrameLayout (layOutFrame 80 $ frame [] [] []) @?= [0, 0, 0]
  , testCase "decodeFrameLayout refuses words it cannot have written" $ do
      decodeFrameLayout (init wireWords) @?= Nothing
      decodeFrameLayout (wireWords ++ [0]) @?= Nothing
      -- The first chunk's face, then its all-space word, flipped.
      decodeFrameLayout (take 6 wireWords ++ [1] ++ drop 7 wireWords)
        @?= Nothing
      decodeFrameLayout (take 8 wireWords ++ [1] ++ drop 9 wireWords)
        @?= Nothing
  , testProperty "decodeFrameLayout inverts encodeFrameLayout" $
      forAll ((,,,) <$> choose (2, 40) <*> genOverlay <*> genOverlay
                    <*> genOverlay) $ \(width, prop, square, mono) ->
        let layout = layOutFrame width $ frame prop square mono
        in decodeFrameLayout (encodeFrameLayout layout) === Just layout
  ]
 where
  wireWords = encodeFrameLayout $ layOutFrame 10 wireFrame
  cell fg c = Color.attrCharW32 $ Color.attrChar2ToW32 fg c

-- | A frame with one overlay of each kind, on a screen 10 squares wide:
-- a proportional line of a regular chunk, recoloured for its even row
-- and with a letter beyond ASCII, and a bold one all coloured spaces.
wireFrame :: SingleFrame
wireFrame =
  frame [(PointUI 2 0, str Color.White "h\233" ++ str Color.Blue "  ")]
        [(PointUI 4 1, str Color.Red "s")]
        [(PointUI 3 2, str Color.Green "m")]

-- | A cursor at the given row and device x, started through metrics whose
-- half-cell is one device unit, so that it may start anywhere.
cursorAt :: Int -> Int -> PropCursor
cursorAt row x = startPropLine (mkLayoutMetrics 1 2) (PropLine x row [])

-- | A chunk of letters or of spaces, whose text matters to 'propFit' only
-- through 'pcAllSpace'.
chunkOf :: Bool -> PropChunk
chunkOf allSpace = if allSpace then spaces else letters

letters, spaces :: PropChunk
letters = PropChunk Color.White "abc"
spaces = PropChunk Color.White "   "

-- | Metrics in the domain 'mkLayoutMetrics' asserts, a cursor in the domain
-- 'propFit' asserts, short of the last column, whether the chunk is all
-- space, and a measured width, often near the rest of the row, where
-- the branches part.
genPlacement :: Gen (Int, Int, Int, Bool, Int)
genPlacement = do
  h <- choose (1, 20)
  w <- choose (2, 100)
  let boxSize = lmBoxSize $ mkLayoutMetrics h w
  x <- choose (0, (w - 1) * boxSize - 1)
  allSpace <- elements [False, True]
  let remaining = w * boxSize - x
  widthRaw <- frequency [ (1, choose (0, 2 * remaining))
                        , (1, elements [ remaining - 1, remaining
                                       , remaining + 1 ]) ]
  return (h, w, x, allSpace, widthRaw)

-- | A cell of a legal overlay line: any foreground, often one of two
-- so that same-colour runs occur, a background without a highlight
-- but the cursor's, and the default space often.
genCell :: Gen Color.AttrCharW32
genCell = frequency
  [ (1, return Color.spaceAttrW32)
  , (3, do
      fg <- frequency [ (3, elements [Color.Red, Color.White])
                      , (1, elements [minBound .. maxBound]) ]
      bg <- elements [Color.HighlightNone, Color.HighlightNoneCursor]
      c <- elements "ab $"
      return $! Color.attrCharToW32 $ Color.AttrChar (Color.Attr fg bg) c) ]

genLine :: Gen AttrString
genLine = listOf genCell

-- | A row and a line to draw on it.
genRowLine :: Gen (Int, AttrString)
genRowLine = (,) <$> choose (0, 3) <*> genLine

genOverlay :: Gen OverlaySpace
genOverlay =
  listOf $ (,) <$> (PointUI <$> choose (0, 70) <*> choose (0, 5)) <*> genLine

-- | The list cut into consecutive pieces of the given lengths.
splitPlaces :: [Int] -> [a] -> [[a]]
splitPlaces [] _ = []
splitPlaces (n : ns) l = let (piece, rest) = splitAt n l
                         in piece : splitPlaces ns rest

-- | A frame with the given proportional, square and mono overlays.
frame :: OverlaySpace -> OverlaySpace -> OverlaySpace -> SingleFrame
frame prop square mono = SingleFrame { singleArray = PointArray.empty
                                     , singlePropOverlay = prop
                                     , singleSquareOverlay = square
                                     , singleMonoOverlay = mono }

str :: Color.Color -> String -> AttrString
str fg = map (Color.attrChar2ToW32 fg)

sp :: Color.AttrCharW32
sp = Color.spaceAttrW32

backlit :: Color.Attr
backlit = Color.Attr Color.Red Color.HighlightBackground

-- | Force every element of the list and expect an assertion to fail.
assertAsserts :: Show a => [a] -> Assertion
assertAsserts l = do
  forced <- try $ evaluate $ length $ show l
  case forced of
    Left (_ :: AssertionFailed) -> return ()
    Right _ -> assertFailure $ "no assertion failed in " ++ show l

-- | The freshness cases for the table pinning re-implementations
-- of 'propCutoff' and 'propFit' and for the fixtures pinning decoders
-- of 'encodeFrameLayout''s words. None in a browser build, which runs under
-- @make test-wasm@, away from the files.
freshnessTests :: [TestTree]
#ifndef USE_BROWSER
freshnessTests = [ freshnessCase source "prop-fit.json" propFitJson
                 , freshnessCase source "overlay-wire.json" overlayWireJson ]

source :: FilePath
source = "OverlayLayoutUnitTests.hs"

-- | The pen's branch table: rows @(halfSize, width, cursorX, allSpace,
-- widthRaw)@, each either @stop@, where 'propCutoff' drops the chunk, or the
-- chunk's @x@, @widthPx@ and the @marker@ it raises, if any, the cell run
-- 'trimmedMarkerRun' puts in the row's last column. Every branch of both
-- functions is in it at three metrics, the narrowest screen 'mkLayoutMetrics'
-- allows among them: pens at the line's start and a pixel short of the last
-- column, each with widths from nothing to well past the rest of the row,
-- either side of it and on it, for a chunk of letters and one of spaces; and
-- pens at the last column, past it and at the screen's edge, which stop.
propFitJson :: Text
propFitJson = jsonDocument source [("rows", map row sample)]
 where
  sample = concat
    [ [ (h, w, x, allSpace, widthRaw)
      | x <- [0, lastColumn - 1]
      , let remaining = screen - x
      , allSpace <- [False, True]
      , widthRaw <- [0, remaining - 1, remaining, remaining + 1, 2 * screen] ]
      ++ [ (h, w, x, False, 0) | x <- [lastColumn, lastColumn + 1, screen] ]
    | (h, w) <- [(1, 2), (8, 80), (9, 3)]
    , let boxSize = lmBoxSize $ mkLayoutMetrics h w
          lastColumn = (w - 1) * boxSize
          screen = w * boxSize ]
  row (h, w, x, allSpace, widthRaw) =
    let lm = mkLayoutMetrics h w
        cur = cursorAt 0 x
        inputs = [ ("halfSize", tshow h), ("width", tshow w)
                 , ("cursorX", tshow x), ("allSpace", jsonBool allSpace)
                 , ("widthRaw", tshow widthRaw) ]
    in jsonObject $ inputs ++
         if propCutoff lm cur
         then [("stop", jsonBool True)]
         else let PropFit{..} = propFit lm cur (chunkOf allSpace) widthRaw
              in [ ("stop", jsonBool False), ("x", tshow pfXPx)
                 , ("widthPx", tshow pfWidthPx)
                 , ("marker", maybe "null" cellRunJson pfMarker) ]

-- | The wire's fixtures: frames' overlays as 'encodeFrameLayout' writes
-- them, each case named, with the width of the screen they are laid out
-- for and the layout they decode to, as @overlay-core.ts@ types it.
-- The frames: one with no overlays; proportional lines, of both faces, one
-- dropped at the last column and one empty; square runs, a highlight
-- in one and the other cut at the screen's edge; mono runs, likewise cut
-- and with the cursor's highlight; and 'wireFrame', with all three.
overlayWireJson :: Text
overlayWireJson = jsonDocument source [("cases", map wireCase frames)]
 where
  frames =
    [ ("no overlays", 80, frame [] [] [])
    , ( "proportional lines", 10
      , frame [ (PointUI 0 1, [sp, sp] ++ str Color.BrYellow "ab"
                              ++ str Color.Red "c")
              , (PointUI 18 2, str Color.Red "dropped")
              , (PointUI 4 3, []) ]
              [] [] )
    , ( "square runs", 10
      , frame [] [ (PointUI 0 4, [attrCell backlit '@'])
                 , (PointUI 14 0, str Color.Blue "abcd") ] [] )
    , ( "mono runs", 10
      , frame [] [] [ (PointUI 17 1, str Color.Green "abcd")
                    , (PointUI 0 0, [attrCell cursor 'x']) ] )
    , ("all three", 10, wireFrame) ]
  attrCell attr c = Color.attrCharToW32 $ Color.AttrChar attr c
  cursor = Color.Attr Color.BrWhite Color.HighlightNoneCursor
  wireCase (name, width, f) =
    let layout = layOutFrame width f
    in jsonObject [ ("name", jsonString name), ("width", tshow width)
                  , ("words", jsonArray $ map tshow $ encodeFrameLayout layout)
                  , ("layout", layoutJson layout) ]
  layoutJson FrameLayout{..} =
    jsonObject [ ("prop", jsonArray $ map propLineJson flProp)
               , ("square", jsonArray $ map cellRunJson flSquare)
               , ("mono", jsonArray $ map cellRunJson flMono) ]
  propLineJson PropLine{..} =
    jsonObject [ ("startX", tshow plStartX), ("row", tshow plRow)
               , ("chunks", jsonArray $ map chunkJson plChunks) ]
  chunkJson chunk@PropChunk{..} =
    jsonObject [ ( "face"
                 , jsonString $ case pcFont chunk of
                     PropRegular -> "propRegular"
                     PropBold -> "propBold" )
               , ("color", tshow $ fromEnum pcColor)
               , ("allSpace", jsonBool $ pcAllSpace chunk)
               , ("text", jsonString pcText) ]

-- | A cell run, as @overlay-core.ts@ types it.
cellRunJson :: CellRun -> Text
cellRunJson CellRun{..} =
  jsonObject [ ("col", tshow crCol), ("row", tshow crRow)
             , ("words", jsonArray $ map (tshow . Color.attrCharW32) crCells) ]

jsonArray :: [Text] -> Text
jsonArray items = "[" <> T.intercalate ", " items <> "]"
#else
freshnessTests = []
#endif
