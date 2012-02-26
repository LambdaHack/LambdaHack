-- | Colours and text attributes.
module Game.LambdaHack.Color
  ( -- * Colours
    Color(..), defBG, defFG, isBright, legalBG, colorToRGB
    -- * Text attributes and the screen
  , Attr(..), defaultAttr, SingleFrame(..)
  ) where

import qualified Data.Binary as Binary

-- TODO: since this type may be essential to speed, consider implementing
-- it as an Int, with color numbered as they are on terminals, see
-- http://www.haskell.org/haskellwiki/Performance/Data_types#Enumerations
-- If we ever switch to 256 colours, the Int implementation or similar
-- will be more natural, anyway.
-- | Colours supported by the major frontends.
data Color =
    Black
  | Red
  | Green
  | Brown
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
  put = Binary.putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) Binary.getWord8

-- | The default colours, to optimize attribute setting.
defBG, defFG :: Color
defBG = Black
defFG = White

-- | Text attributes: foreground and backgroud colors.
data Attr = Attr
  { fg :: !Color  -- ^ foreground colour
  , bg :: !Color  -- ^ backgroud color
  }
  deriving (Show, Eq, Ord)

-- | The default attribute, to optimize attribute setting.
defaultAttr :: Attr
defaultAttr = Attr defFG defBG

-- | The data sufficent to draw a single game screen frame.
data SingleFrame =
    SingleFrame
      { sflevel  :: ![[(Attr, Char)]]  -- ^ content of the screen, line by line
      , sfTop    :: !String            -- ^ an extra line to show at the top
      , sfBottom :: !String            -- ^ an extra line to show at the bottom
      }
  | NoFrame
  deriving Eq

-- | A helper for the terminal frontends that display bright via bold.
isBright :: Color -> Bool
isBright c = c >= BrBlack

-- | Due to the limitation of the curses library used in the curses frontend,
-- only these are legal backgrounds.
legalBG :: [Color]
legalBG = [Black, White, Blue, Magenta]

-- | Translationg to heavily modified Linux console color RGB values.
colorToRGB :: Color -> String
colorToRGB Black     = "#000000"
colorToRGB Red       = "#D50000"
colorToRGB Green     = "#00AA00"
colorToRGB Brown     = "#AA5500"
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

-- | For reference, the original Linux console colors.
-- Good old retro feel and more useful than xterm (e.g. brown).
_olorToRGB :: Color -> String
_olorToRGB Black     = "#000000"
_olorToRGB Red       = "#AA0000"
_olorToRGB Green     = "#00AA00"
_olorToRGB Brown     = "#AA5500"
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
