module Attr where

import Control.Monad
import qualified Data.Binary as Binary

data Color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrBlack
  | BrRed
  | BrGreen
  | BrYellow
  | BrBlue
  | BrMagenta
  | BrCyan
  | BrWhite
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Binary.Binary Color where
  put = Binary.putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) Binary.getWord8

defBG, defFG :: Color
defBG = Black
defFG = White

isBright :: Color -> Bool
isBright c = fromEnum c > 7

-- Mimics the Linux console; good old retro feel and more useful than xterm.
colorToRGB :: Color -> String
colorToRGB Black     = "#000000"
colorToRGB Red       = "#AA0000"
colorToRGB Green     = "#00AA00"
colorToRGB Yellow    = "#AA5500"
colorToRGB Blue      = "#0000AA"
colorToRGB Magenta   = "#AA00AA"
colorToRGB Cyan      = "#00AAAA"
colorToRGB White     = "#AAAAAA"
colorToRGB BrBlack   = "#555555"
colorToRGB BrRed     = "#FF5555"
colorToRGB BrGreen   = "#55FF55"
colorToRGB BrYellow  = "#FFFF55"
colorToRGB BrBlue    = "#5555FF"
colorToRGB BrMagenta = "#FF55FF"
colorToRGB BrCyan    = "#55FFFF"
colorToRGB BrWhite   = "#FFFFFF"

-- Human-readable names, for item descriptions. L
-- Lots of licentia poetica, since the colors are artificial and rarely seen.
colorToName :: Color -> String
colorToName Black     = "black"
colorToName Red       = "apple red"
colorToName Green     = "forest green"
colorToName Yellow    = "brown"
colorToName Blue      = "ultramarine"
colorToName Magenta   = "purple"
colorToName Cyan      = "cyan"
colorToName White     = "silver gray"
colorToName BrBlack   = "charcoal"
colorToName BrRed     = "coral red"
colorToName BrGreen   = "emerald green"
colorToName BrYellow  = "yellow"
colorToName BrBlue    = "royal blue"
colorToName BrMagenta = "magenta"
colorToName BrCyan    = "aquamarine"
colorToName BrWhite   = "white"
