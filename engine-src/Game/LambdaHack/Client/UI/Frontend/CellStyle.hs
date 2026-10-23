-- | The per-cell drawing decisions for the frontends that draw the screen
-- cell by cell, SDL and the browser: the glyph a cell shows, its colour,
-- its fill and the outline around it, if any, which a frontend renders through
-- a thin step of its own. The browser's, @styledCell@
-- in @ts-src/src/terminal-core.ts@, is pinned to 'styleCell' by fixtures
-- the test suite writes into @ts-src/src/generated/@.
module Game.LambdaHack.Client.UI.Frontend.CellStyle
  ( CellStyle(..), CellGlyphs(..), styleCell, rowColor, backlightRGB
  , rgbChannels
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Char (digitToInt)
import qualified Data.Text as T

import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- | How to draw one cell.
data CellStyle = CellStyle
  { csGlyph   :: Char               -- ^ the character to draw
  , csFg      :: Color.Color        -- ^ the colour to draw it in
  , csBacklit :: Bool               -- ^ filled with 'backlightRGB', not black
  , csBorder  :: Maybe Color.Color  -- ^ the outline around the cell, if any
  }
  deriving (Show, Eq)

-- | The characters a frontend draws in place of two that its fonts
-- or its output medium show badly, a frontend parameter rather than
-- a frontend name.
data CellGlyphs = CellGlyphs
  { cgDimFloor :: Char  -- ^ for the floor symbol in a dim colour
  , cgSpace    :: Char  -- ^ for a space
  }
  deriving (Show, Eq)

-- | Decide how to draw a cell, given the frontend's substitute glyphs
-- and the cell's screen row.
--
-- A cell is outlined in its highlight's 'Color.highlightToColor', as SDL's
-- @chooseAndDrawHighlight@ draws a rectangle around it, but
-- 'Color.HighlightNone', 'Color.HighlightBackground'
-- and 'Color.HighlightNoneCursor' are not outlined, although the second's
-- 'Color.highlightToColor' is 'Color.BrBlack'.
styleCell :: CellGlyphs -> Int -> Color.AttrCharW32 -> CellStyle
styleCell CellGlyphs{..} row w =
  let Color.AttrChar{acAttr=Color.Attr{fg=fgRaw, bg}, acChar} =
        Color.attrCharFromW32 w
      fg = rowColor row fgRaw
      glyph | acChar == ' ' = cgSpace
            | acChar == floorSymbol && not (Color.isBright fg) = cgDimFloor
            | otherwise = acChar
      border = case bg of
        Color.HighlightNone -> Nothing
        Color.HighlightBackground -> Nothing
        Color.HighlightNoneCursor -> Nothing
        _ -> Just $ Color.highlightToColor bg
  in CellStyle { csGlyph = glyph
               , csFg = fg
               , csBacklit = bg == Color.HighlightBackground
               , csBorder = border }

-- | The colour a foreground colour is drawn in on the given screen row:
-- 'Color.White' is drawn as 'Color.AltWhite' on even rows.
rowColor :: Int -> Color.Color -> Color.Color
rowColor row fg | even row && fg == Color.White = Color.AltWhite
                | otherwise = fg

-- | The fill of a cell under 'Color.HighlightBackground', the vision
-- backlight: a third of @'Color.colorToRGB' 'Color.BrBlack'@, since it covers
-- the whole cell where a glyph covers a small part of it. No 'Color.Color'
-- constructor has this value.
backlightRGB :: Text
backlightRGB = "#251F1F"

-- | The red, green and blue of a @#RRGGBB@ colour, such as
-- 'Color.colorToRGB' gives.
rgbChannels :: Text -> (Int, Int, Int)
rgbChannels t = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2] -> (channel r1 r2, channel g1 g2, channel b1 b2)
  _ -> error $ "not #RRGGBB" `showFailure` t
 where
  channel high low = 16 * digitToInt high + digitToInt low
