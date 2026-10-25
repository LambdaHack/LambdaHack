-- | The overlay layout of the frontends that draw the three overlays
-- themselves, SDL and the browser: every per-line and per-chunk decision,
-- cut along the measurement line. Chunking, colours, faces and the two
-- fixed-pitch line cuts take no metrics at all. The proportional pen, which
-- advances by rendered widths only a frontend can measure, goes through two
-- functions: 'propCutoff', asked before a chunk is rendered, and 'propFit',
-- given the width just measured. The table the test suite writes into
-- @ts-src/src/generated/@ (@docs/wasm-frontend-unified-plan.md@, 2.1) pins
-- any re-implementation of those two to them, such as the browser carries.
-- A frontend across an FFI is handed the layout as the words
-- 'encodeFrameLayout' writes, which fixtures the suite writes there too
-- pin its decoder to (2.3).
module Game.LambdaHack.Client.UI.Frontend.OverlayLayout
  ( -- * Cell metrics
    LayoutMetrics, lmHalfSize, lmWidth, lmBoxSize, mkLayoutMetrics
    -- * The layout, which takes no metrics
  , PropFont(..), PropChunk(..), pcFont, pcAllSpace, PropLine(..)
  , CellLayer(..), cellPitch, CellRun(..), FrameLayout(..)
  , layOutFrame, layOutSquare, layOutMono, propLineStartFits
    -- * The proportional pen, which advances by measured widths
  , PropCursor, startPropLine, propCutoff, PropFit(..), propFit
  , trimmedMarkerRun
    -- * The layout on the wire to a frontend across an FFI
  , encodeFrameLayout
    -- * Operations both internal and used in unit tests
  , chunkPropLine, decodeFrameLayout
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get)
import           Data.Bits (toIntegralSized)
import qualified Data.Char as Char
import qualified Data.Text as T
import           Data.Word (Word32)

import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.CellStyle (rowColor)
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import qualified Game.LambdaHack.Definition.Color as Color

-- | Cell metrics in one consumer's own device units: SDL pixels
-- of the loaded map font, or the device pixels of the browser's canvas
-- painter, which takes its cell from the loaded font as SDL does.
data LayoutMetrics = LayoutMetrics
  { lmHalfSize :: Int  -- ^ one 'PointUI' step, and the mono pitch
  , lmWidth    :: Int  -- ^ @rwidth coscreen@, screen width in squares
  }
  deriving (Show, Eq)

-- | One square cell: the square pitch and the row height of all three
-- layers. Derived, not stored, so that the map font determining the cell
-- size of all the others cannot come apart in a consumer or in a test.
lmBoxSize :: LayoutMetrics -> Int
lmBoxSize = (2 *) . lmHalfSize

-- | The only constructor exported. SDL passes half the cell its loaded map
-- font determines, the font opened at a point size already multiplied by
-- @sallFontsScale@; the browser is to pass its painter's @halfSize@, half the
-- cell it takes from its loaded map font the same way. The width is the same
-- @rwidth coscreen@ the caller passes to 'layOutFrame': two different values
-- there would leave the line filter and the pen clamping against different
-- screens, so a consumer binds it once and uses that binding twice.
mkLayoutMetrics :: Int -> Int -> LayoutMetrics
mkLayoutMetrics halfSize width =
  assert (halfSize > 0 && width > 1 `blame` (halfSize, width))
  $ LayoutMetrics halfSize width

-- | Which of the two proportional faces draws a chunk.
--
-- Deliberately not 'DisplayFont': that type names the three overlay
-- layers, is kept abstract so that 'FontSetup' overrides cannot be bypassed,
-- and refuses width questions outright, @textSize PropFont@ being an error;
-- nor has it a bold constructor. Two constructors here and no square
-- or mono ones: those layers have one face each.
data PropFont = PropRegular | PropBold
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | The face rule, on the colour after the even-row recolour. 'Color.AltWhite'
-- sorts between 'Color.White' and 'Color.BrBlack', so the recolour cannot flip
-- a chunk from regular to bold, which is why 'chunkPropLine' may apply it
-- before the face is read off the colour here.
propFontOf :: Color.Color -> PropFont
propFontOf fg | fg >= Color.White && fg /= Color.BrBlack = PropRegular
              | otherwise = PropBold

-- | One maximal same-colour run of a proportional line. It carries no
-- position: its x is the running sum of the measured widths of the chunks
-- before it, which is why the pen's half of this module exists.
data PropChunk = PropChunk
  { pcColor :: Color.Color  -- ^ after the even-row recolour
  , pcText  :: Text         -- ^ the characters, colours dropped; never empty
  }
  deriving (Show, Eq)

-- | Which face draws this chunk. Derived, not stored, so it cannot be
-- supplied out of step with 'pcColor'.
pcFont :: PropChunk -> PropFont
pcFont = propFontOf . pcColor

-- | Whether the chunk's text is all white space, derived likewise. It decides
-- whether an overrunning chunk may claim the last cell and whether its cut
-- raises the trimmed-line marker, so it is the one input of 'propFit'
-- a consumer must be given rather than re-derive.
pcAllSpace :: PropChunk -> Bool
pcAllSpace = T.all Char.isSpace . pcText

-- | One proportional line, still at its logical 'PointUI' start.
data PropLine = PropLine
  { plStartX :: Int  -- ^ 'PointUI' x, in half-cells; multiplied by
                     --   'lmHalfSize' only in 'startPropLine'
  , plRow    :: Int
  , plChunks :: [PropChunk]
  }
  deriving (Show, Eq)

-- | Which fixed-pitch layer a run belongs to.
data CellLayer = LayerSquare | LayerMono
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Device advance of one cell: a box for the square layer, a half
-- for the mono one.
cellPitch :: LayoutMetrics -> CellLayer -> Int
cellPitch lm = \case
  LayerSquare -> lmBoxSize lm
  LayerMono -> lmHalfSize lm

-- | A run of cells in one fixed-pitch layer, already cut to the screen.
--
-- The cells stay raw 'Color.AttrCharW32' words, background included,
-- since the square layer draws highlights. The even-row recolour and
-- the glyph substitutions are not applied here: they are the per-cell rules
-- of "Game.LambdaHack.Client.UI.Frontend.CellStyle", which the painter
-- applies to each cell.
data CellRun = CellRun
  { crLayer :: CellLayer
  , crRow   :: Int
  , crCol   :: Int         -- ^ leftmost cell, in units of 'cellPitch':
                           --   square columns for the square layer,
                           --   'PointUI' half-cells for the mono one
  , crCells :: AttrString  -- ^ never empty
  }
  deriving (Show, Eq)

-- | One frame's overlays. The field order is the draw order --- proportional,
-- then square, then mono, last because a mono warning about an overrun is
-- to be drawn over the proportional text beneath it --- so that a consumer
-- drawing the fields in order obeys the rule without knowing it. The map
-- is not here: it is not an overlay.
data FrameLayout = FrameLayout
  { flProp   :: [PropLine]
  , flSquare :: [CellRun]
  , flMono   :: [CellRun]
  }
  deriving (Show, Eq)

-- | Lay out a frame's overlays, given @rwidth coscreen@. A proportional line
-- whose start 'propLineStartFits' rejects is dropped here, the test taking
-- no metrics, so a consumer never sees one.
layOutFrame :: Int -> SingleFrame -> FrameLayout
layOutFrame width SingleFrame{..} =
  FrameLayout
    { flProp = [ PropLine x row (chunkPropLine row al)
               | (PointUI x row, al) <- singlePropOverlay
               , propLineStartFits width x ]
    , flSquare = layOutSquare width singleSquareOverlay
    , flMono = layOutMono width singleMonoOverlay }

-- | Split a proportional line into maximal same-colour chunks. A space,
-- meaning 'Color.spaceAttrW32', matches any colour: a run of them joins
-- the chunk to its left, and a run that starts the line takes the colour
-- of the next other character, or its own when the line is all such spaces.
-- The colour is recoloured for the row by 'rowColor'.
--
-- Asserts that the cell each chunk takes its colour from has no highlight
-- but the cursor's.
--
-- Cuts nothing: cutting is 'propFit''s, so the chunks' texts concatenate
-- to the line's.
chunkPropLine :: Int -> AttrString -> [PropChunk]
chunkPropLine _ [] = []
chunkPropLine row (w : rest) =
  let isSpace = (== Color.spaceAttrW32)
      Color.AttrChar{acAttr=Color.Attr{fg=fgRaw, bg}} =
        Color.attrCharFromW32 $ fromMaybe w $ find (not . isSpace) (w : rest)
      sameAttr ac = Color.fgFromW32 ac == fgRaw
                    || isSpace ac  -- matches all colours
      (sameRest, otherRest) = span sameAttr rest
      !_A = assert (unhighlighted bg) ()
      chunk = PropChunk { pcColor = rowColor row fgRaw
                        , pcText = T.pack $ attrStringToString $ w : sameRest }
  in chunk : chunkPropLine row otherRest

-- | The square overlay's lines, cut to the screen's width in squares,
-- with the runs left empty dropped. The 'Int' is @rwidth coscreen@.
layOutSquare :: Int -> OverlaySpace -> [CellRun]
layOutSquare width ov =
  [ CellRun LayerSquare row col cut
  | (pUI, al) <- ov
  , let PointSquare col row = uiToSquare pUI
        cut = take (width - col) al
  , not (null cut) ]

-- | The mono overlay's lines, cut to the screen's width in half-cells,
-- with the runs left empty dropped. Asserts that no cell drawn has
-- a highlight but the cursor's. The 'Int' is @rwidth coscreen@.
layOutMono :: Int -> OverlaySpace -> [CellRun]
layOutMono width ov =
  [ assert (all (unhighlighted . Color.bgFromW32) cut `blame` cut)
    $ CellRun LayerMono row x cut
  | (PointUI x row, al) <- ov
  , let cut = take (2 * width - x) al
  , not (null cut) ]

-- | Whether a cell's highlight is none but the cursor's.
unhighlighted :: Color.Highlight -> Bool
unhighlighted = (`elem` [Color.HighlightNone, Color.HighlightNoneCursor])

-- | Whether a proportional line starting at this 'PointUI' x may be drawn
-- at all, given @rwidth coscreen@: the half of 'propCutoff' expressible
-- in logical units, equal to it on the line's first chunk because
-- 'lmBoxSize' is twice 'lmHalfSize' and 'lmHalfSize' is positive:
--
-- > xUI * halfSize >= (width - 1) * boxSize  <=>  xUI >= 2 * (width - 1)
propLineStartFits :: Int -> Int -> Bool
propLineStartFits width xUI = xUI < 2 * (width - 1)

-- | Pen state for one proportional line, in device units. Constructed only
-- by 'startPropLine' and advanced only by 'propFit', so no consumer can put
-- to 'propCutoff' an x this module did not compute.
data PropCursor = PropCursor
  { pcurXPx :: Int
  , pcurRow :: Int
  }
  deriving (Show, Eq)

-- | The one place a logical x becomes a device x.
startPropLine :: LayoutMetrics -> PropLine -> PropCursor
startPropLine LayoutMetrics{lmHalfSize} PropLine{plStartX, plRow} =
  PropCursor (plStartX * lmHalfSize) plRow

-- | Whether the pen has reached the last column, in which case this chunk
-- and every later chunk of the line are dropped. Takes no measurement,
-- so that a consumer asking it first never renders a chunk it then
-- throws away.
propCutoff :: LayoutMetrics -> PropCursor -> Bool
propCutoff lm PropCursor{pcurXPx} =
  pcurXPx >= (lmWidth lm - 1) * lmBoxSize lm

-- | Where a measured chunk goes, how much of it survives, and whether
-- the cut raises the trimmed-line marker.
data PropFit = PropFit
  { pfXPx     :: Int            -- ^ device x to draw at
  , pfWidthPx :: Int            -- ^ device width to draw and to clip to:
                                --   the whole chunk when it fits, else
                                --   the width it is cut to
  , pfMarker  :: Maybe CellRun  -- ^ to draw before the next chunk
  , pfNext    :: PropCursor
  }
  deriving (Show, Eq)

-- | Place one measured chunk. A chunk that fits is drawn whole. One that
-- overruns the screen is cut: an all-space chunk to the screen's edge,
-- any other one cell short of it, leaving that cell to the trimmed-line
-- marker. Either cut leaves the pen at or past the cutoff, so a cut chunk
-- ends its line and a line gets at most one marker.
--
-- The consumer supplies the chunk's natural rendered width in its own device
-- units, rounded up, so that "it fits" is never optimistic: SDL the width
-- of the rendered surface, and the browser is to supply the ceiling of
-- @measureText@.
--
-- Precondition: @not (propCutoff lm cur)@, which is also what keeps
-- the width of a chunk cut one cell short positive.
propFit :: LayoutMetrics -> PropCursor -> PropChunk -> Int -> PropFit
propFit lm cur chunk widthRaw =
  assert (not (propCutoff lm cur) && widthRaw >= 0
          `blame` (lm, cur, widthRaw))
  $ assert (width == widthRaw || width > 0 && propCutoff lm next
            `blame` (lm, cur, widthRaw, width))
  $ PropFit { pfXPx = pcurXPx cur
            , pfWidthPx = width
            , pfMarker = if trimmed
                         then Just (trimmedMarkerRun lm (pcurRow cur))
                         else Nothing
            , pfNext = next }
 where
  boxSize = lmBoxSize lm
  allSpace = pcAllSpace chunk
  remaining = lmWidth lm * boxSize - pcurXPx cur
  width | widthRaw <= remaining = widthRaw
        | allSpace = remaining
        | otherwise = remaining - boxSize
  trimmed = width /= widthRaw && not allSpace
  next = cur {pcurXPx = pcurXPx cur + width}

-- | The @$@ stamped into the last square column of a row whose proportional
-- text was cut, 'Color.trimmedLineAttrW32', as an ordinary square-layer run,
-- so that no frontend needs marker code of its own. Distinct from the @$@
-- the engine splices into an over-long line before a frontend sees it,
-- which counts logical cells where this one reacts to a rendered width;
-- the two share the word and are not to be unified.
trimmedMarkerRun :: LayoutMetrics -> Int -> CellRun
trimmedMarkerRun LayoutMetrics{lmWidth} row =
  CellRun { crLayer = LayerSquare
          , crRow = row
          , crCol = lmWidth - 1
          , crCells = [Color.trimmedLineAttrW32] }

-- | A frame's overlays as the words a frontend across an FFI decodes,
-- the browser in @ts-src/src/overlay-core.ts@: one scalar to a word,
-- the cells' 'Color.AttrCharW32' words the only ones that pack several,
-- and a count before every repeated group, counting its elements. In order:
--
-- > nProp nSquare nMono               the counts of the three sections
-- > nProp proportional lines, each:
-- >   startX row nChunks              then nChunks chunks, each:
-- >     face colour allSpace nChars   then nChars codepoints
-- > nSquare square runs, each:
-- >   row col nCells                  then nCells AttrCharW32 words
-- > nMono mono runs, each as a square run
--
-- The sections are the fields of 'FrameLayout', in its draw order, so no
-- run carries its layer. @face@ is 'fromEnum' of 'pcFont', 0 for
-- 'PropRegular' and 1 for 'PropBold', @colour@ that of 'pcColor', and
-- @allSpace@ that of 'pcAllSpace', the one input of 'propFit' a consumer
-- is given rather than re-derive. @startX@ is in 'PointUI' half-cells,
-- the device x being the consumer's 'startPropLine' to compute.
-- Crashes on a scalar no word holds.
encodeFrameLayout :: FrameLayout -> [Word32]
encodeFrameLayout FrameLayout{..} =
  [count flProp, count flSquare, count flMono]
  ++ concatMap propLine flProp
  ++ concatMap (cellRun LayerSquare) flSquare
  ++ concatMap (cellRun LayerMono) flMono
 where
  scalar :: Int -> Word32
  scalar = toIntegralCrash
  count :: [a] -> Word32
  count = scalar . length
  propLine PropLine{..} =
    [scalar plStartX, scalar plRow, count plChunks]
    ++ concatMap propChunk plChunks
  propChunk chunk@PropChunk{..} =
    [ scalar $ fromEnum $ pcFont chunk, scalar $ fromEnum pcColor
    , scalar $ fromEnum $ pcAllSpace chunk, scalar $ T.length pcText ]
    ++ map (scalar . Char.ord) (T.unpack pcText)
  cellRun layer CellRun{..} =
    assert (crLayer == layer `blame` (layer, crLayer))
    $ [scalar crRow, scalar crCol, count crCells]
      ++ map Color.attrCharW32 crCells

-- | The layout 'encodeFrameLayout' wrote the words of, if it wrote them:
-- none for words too few or too many, or for a chunk whose face or
-- all-space word is out of step with its colour and text.
decodeFrameLayout :: [Word32] -> Maybe FrameLayout
decodeFrameLayout = evalStateT $ do
  nProp <- int
  nSquare <- int
  nMono <- int
  flProp <- replicateM nProp propLine
  flSquare <- replicateM nSquare $ cellRun LayerSquare
  flMono <- replicateM nMono $ cellRun LayerMono
  get >>= guard . null
  return FrameLayout{..}
 where
  word :: StateT [Word32] Maybe Word32
  word = StateT uncons
  int :: StateT [Word32] Maybe Int
  int = word >>= lift . toIntegralSized
  propLine = do
    plStartX <- int
    plRow <- int
    nChunks <- int
    plChunks <- replicateM nChunks propChunk
    return PropLine{..}
  propChunk = do
    face <- int
    colour <- int
    allSpace <- int
    nChars <- int
    codes <- replicateM nChars int
    guard $ colour <= fromEnum (maxBound :: Color.Color)
            && all (<= Char.ord maxBound) codes
    let chunk = PropChunk { pcColor = toEnum colour
                          , pcText = T.pack $ map Char.chr codes }
    guard $ face == fromEnum (pcFont chunk)
            && allSpace == fromEnum (pcAllSpace chunk)
    return chunk
  cellRun crLayer = do
    crRow <- int
    crCol <- int
    nCells <- int
    crCells <- replicateM nCells $ Color.AttrCharW32 <$> word
    return CellRun{..}
