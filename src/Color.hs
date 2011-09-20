module Color where

import Control.Monad
import qualified Data.Binary as Binary

-- TODO: since this type may be essential to speed, consider implementing
-- it as an Int, with color numbered as they are on terminals, see
-- http://www.haskell.org/haskellwiki/Performance/Data_types#Enumerations
-- If we ever switch to 256 colours, the Int implementation or similar
-- will be more natural, anyway.

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

type Attr = (Color.Color, Color.Color)
defaultAttr :: Attr
defaultAttr = (Color.defFG, Color.defBG)

isBright :: Color -> Bool
isBright c = fromEnum c > 7  -- for terminals that display bright via bold

-- | Due to limitation of curses, only these are legal backgrounds.
legalBG :: [Color]
legalBG = [Black, White, Blue, Magenta]

-- Heavily modified Linux console colors.
colorToRGB :: Color -> String
colorToRGB Black     = "#000000"
colorToRGB Red       = "#D50000"
colorToRGB Green     = "#00AA00"
colorToRGB Yellow    = "#AA5500"  -- brown
colorToRGB Blue      = "#203AF0"
colorToRGB Magenta   = "#AA00AA"
colorToRGB Cyan      = "#00AAAA"
colorToRGB White     = "#C5BCB8"
colorToRGB BrBlack   = "#6F5F5F"
colorToRGB BrRed     = "#FF5555"
colorToRGB BrGreen   = "#75FF45"
colorToRGB BrYellow  = "#FFE855"
colorToRGB BrBlue    = "#4090FF"
colorToRGB BrMagenta = "#FF77FF"
colorToRGB BrCyan    = "#60FFF0"
colorToRGB BrWhite   = "#FFFFFF"

-- For reference, the original Linux console colors.
-- Good old retro feel and more useful than xterm (e.g. brown).
colorToRGB' :: Color -> String
colorToRGB' Black     = "#000000"
colorToRGB' Red       = "#AA0000"
colorToRGB' Green     = "#00AA00"
colorToRGB' Yellow    = "#AA5500"  -- brown
colorToRGB' Blue      = "#0000AA"
colorToRGB' Magenta   = "#AA00AA"
colorToRGB' Cyan      = "#00AAAA"
colorToRGB' White     = "#AAAAAA"
colorToRGB' BrBlack   = "#555555"
colorToRGB' BrRed     = "#FF5555"
colorToRGB' BrGreen   = "#55FF55"
colorToRGB' BrYellow  = "#FFFF55"
colorToRGB' BrBlue    = "#5555FF"
colorToRGB' BrMagenta = "#FF55FF"
colorToRGB' BrCyan    = "#55FFFF"
colorToRGB' BrWhite   = "#FFFFFF"

-- Human-readable names, for item descriptions. The simple set.
colorToName :: Color -> String
colorToName Black     = "black"
colorToName Red       = "red"
colorToName Green     = "green"
colorToName Yellow    = "brown"
colorToName Blue      = "blue"
colorToName Magenta   = "purple"
colorToName Cyan      = "cyan"
colorToName White     = "ivory"
colorToName BrBlack   = "gray"
colorToName BrRed     = "coral"
colorToName BrGreen   = "lime"
colorToName BrYellow  = "yellow"
colorToName BrBlue    = "azure"
colorToName BrMagenta = "pink"
colorToName BrCyan    = "aquamarine"
colorToName BrWhite   = "white"

-- The fancy set.
colorToName' :: Color -> String
colorToName' Black     = "smoky black"
colorToName' Red       = "apple red"
colorToName' Green     = "forest green"
colorToName' Yellow    = "mahogany"
colorToName' Blue      = "royal blue"
colorToName' Magenta   = "indigo"
colorToName' Cyan      = "teal"
colorToName' White     = "silver gray"
colorToName' BrBlack   = "charcoal"
colorToName' BrRed     = "salmon"
colorToName' BrGreen   = "emerald"
colorToName' BrYellow  = "amber"
colorToName' BrBlue    = "sky blue"
colorToName' BrMagenta = "magenta"
colorToName' BrCyan    = "turquoise"
colorToName' BrWhite   = "ghost white"
