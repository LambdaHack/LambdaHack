-- | The mouse cursor the frontends show over the game screen, a crosshair,
-- one character of ASCII art per pixel, so that the SDL frontend builds
-- its cursor, and the test suite writes the browser's, from one definition.
module Game.LambdaHack.Client.UI.Frontend.MouseCursor
  ( CursorPixel(..), xhairCursorPixels, xhairCursorSize, xhairCursorHotspot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

-- | A pixel of a two-colour cursor.
data CursorPixel = CursorClear | CursorBlack | CursorWhite
  deriving (Show, Eq)

-- | The crosshair cursor, row by row from the top.
xhairCursorPixels :: [[CursorPixel]]
xhairCursorPixels =
  let pixel '.' = CursorBlack
      pixel '#' = CursorWhite
      pixel _ = CursorClear
  in map (map pixel)
    [ "            ...                 "
    , "            .#.                 "
    , "        ..  .#.  ..             "
    , "      ..##  .#.  ##..           "
    , "     .##    .#.    ##.          "
    , "    .#      .#.      #.         "
    , "   .#       .#.       #.        "
    , "   .#       ...       #.        "
    , "  .#                   #.       "
    , "  .#                   #.       "
    , "                                "
    , "             .                  "
    , "........    .#.    ........     "
    , ".######.   .###.   .######.     "
    , "........    .#.    ........     "
    , "             .                  "
    , "                                "
    , "  .#                   #.       "
    , "  .#                   #.       "
    , "   .#       ...       #.        "
    , "   .#       .#.       #.        "
    , "    .#      .#.      #.         "
    , "     .##    .#.    ##.          "
    , "      ..##  .#.  ##..           "
    , "        ..  .#.  ..             "
    , "            .#.                 "
    , "            ...                 " ]

-- | The width and height of 'xhairCursorPixels'.
xhairCursorSize :: (Int, Int)
xhairCursorSize = case xhairCursorPixels of
  row : rows -> assert (all ((== length row) . length) rows)
                       (length row, 1 + length rows)
  [] -> (0, 0)

-- | The pixel of 'xhairCursorPixels' at the mouse position, as @(x, y)@
-- from the top-left corner.
xhairCursorHotspot :: (Int, Int)
xhairCursorHotspot = (13, 13)
