module Color
  (Color(..), Attr, defBG, defFG, defaultAttr, isBright, legalBG, colorToRGB)
  where

import Control.Monad
import qualified Data.Binary as Binary

-- TODO: if the file grows much larger, split into Utils/Color.hs and Attr.hs

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
_olorToRGB :: Color -> String
_olorToRGB Black     = "#000000"
_olorToRGB Red       = "#AA0000"
_olorToRGB Green     = "#00AA00"
_olorToRGB Yellow    = "#AA5500"  -- brown
_olorToRGB Blue      = "#0000AA"
_olorToRGB Magenta   = "#AA00AA"
_olorToRGB Cyan      = "#00AAAA"
_olorToRGB White     = "#AAAAAA"
_olorToRGB BrBlack   = "#555555"
_olorToRGB BrRed     = "#FF5555"
_olorToRGB BrGreen   = "#55FF55"
_olorToRGB BrYellow  = "#FFFF55"
_olorToRGB BrBlue    = "#5555FF"
_olorToRGB BrMagenta = "#FF55FF"
_olorToRGB BrCyan    = "#55FFFF"
_olorToRGB BrWhite   = "#FFFFFF"
