{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, MagicHash #-}
-- | Colours and text attributes.
module Game.LambdaHack.Common.Color
  ( -- * Colours
    Color(..), defFG, defBG, isBright, legalBG, darkCol, brightCol, stdCol
  , colorToRGB
    -- * Text attributes and the screen
  , Attr(..), defAttr
  , AttrChar(..)
  , AttrCharW32(AttrCharW32), attrCharW32
  , attrCharToW32, attrCharFromW32
  , fgFromW32, bgFromW32, charFromW32, attrFromW32, attrEnumFromW32
  , spaceAttrW32, retAttrW32
  , attrChar2ToW32
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import qualified Data.Char as Char
import Data.Hashable (Hashable)
import Data.Word (Word32)
import GHC.Exts (Int (I#))
import GHC.Generics (Generic)
import GHC.Prim (int2Word#)
import GHC.Word (Word32 (W32#))

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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary Color where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Color

-- | The default colours, to optimize attribute setting.
defFG, defBG :: Color
defFG = White
defBG = Black

-- | Text attributes: foreground and backgroud colors.
data Attr = Attr
  { fg :: !Color  -- ^ foreground colour
  , bg :: !Color  -- ^ backgroud color
  }
  deriving (Show, Eq, Ord)

instance Enum Attr where
  fromEnum Attr{..} = unsafeShiftL (fromEnum fg) 8 + fromEnum bg
  toEnum n = Attr (toEnum $ unsafeShiftR n 8)
                  (toEnum $ n .&. (2 ^ (8 :: Int) - 1))

-- | The default attribute, to optimize attribute setting.
defAttr :: Attr
defAttr = Attr defFG defBG

data AttrChar = AttrChar
  { acAttr :: !Attr
  , acChar :: !Char
  }
  deriving (Show, Eq, Ord)

-- This implementation is faster than @Int@, because some vector updates
-- can be done without going to and from @Int@.
newtype AttrCharW32 = AttrCharW32 {attrCharW32 :: Word32}
  deriving (Show, Eq, Enum, Binary)

attrCharToW32 :: AttrChar -> AttrCharW32
attrCharToW32 !AttrChar{acAttr=Attr{..}, acChar} = AttrCharW32 $ toEnum $
  unsafeShiftL (fromEnum fg) 8 + fromEnum bg + unsafeShiftL (Char.ord acChar) 16

attrCharFromW32 :: AttrCharW32 -> AttrChar
attrCharFromW32 !w =
  AttrChar (Attr (toEnum $ fromEnum
                  $ unsafeShiftR (attrCharW32 w) 8 .&. (2 ^ (8 :: Int) - 1))
                 (toEnum $ fromEnum
                  $ attrCharW32 w .&. (2 ^ (8 :: Int) - 1)))
           (Char.chr $ fromEnum $ unsafeShiftR (attrCharW32 w) 16)

{- surprisingly, this is slower:
attrCharFromW32 :: AttrCharW32 -> AttrChar
attrCharFromW32 !w = AttrChar (attrFromW32 w) (charFromW32 w)
-}

fgFromW32 :: AttrCharW32 -> Color
{-# INLINE fgFromW32 #-}
fgFromW32 w =
  toEnum $ fromEnum $ unsafeShiftR (attrCharW32 w) 8 .&. (2 ^ (8 :: Int) - 1)

bgFromW32 :: AttrCharW32 -> Color
{-# INLINE bgFromW32 #-}
bgFromW32 w =
  toEnum $ fromEnum $ attrCharW32 w .&. (2 ^ (8 :: Int) - 1)

charFromW32 :: AttrCharW32 -> Char
{-# INLINE charFromW32 #-}
charFromW32 w =
  Char.chr $ fromEnum $ unsafeShiftR (attrCharW32 w) 16

attrFromW32 :: AttrCharW32 -> Attr
{-# INLINE attrFromW32 #-}
attrFromW32 w = Attr (fgFromW32 w) (bgFromW32 w)

attrEnumFromW32 :: AttrCharW32 -> Int
{-# INLINE attrEnumFromW32 #-}
attrEnumFromW32 !w = fromEnum $ attrCharW32 w .&. (2 ^ (16 :: Int) - 1)

spaceAttrW32 :: AttrCharW32
spaceAttrW32 = attrCharToW32 $ AttrChar defAttr ' '

retAttrW32 :: AttrCharW32
retAttrW32 = attrCharToW32 $ AttrChar defAttr '\n'

-- | A helper for the terminal frontends that display bright via bold.
isBright :: Color -> Bool
isBright c = c >= BrBlack

-- TODO: smart constructor for bg
-- | Due to the limitation of the curses library used in the curses frontend,
-- only these are legal backgrounds.
legalBG :: [Color]
legalBG = [Black, Blue, White, BrBlack]

-- | Colour sets.
darkCol, brightCol, stdCol :: [Color]
darkCol   = [Red .. Cyan]
brightCol = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol    = darkCol ++ brightCol

-- | Translationg to heavily modified Linux console color RGB values.
colorToRGB :: Color -> Text
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
_olorToRGB :: Color -> Text
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

attrChar2ToW32 :: Color -> Char -> AttrCharW32
{-# INLINE attrChar2ToW32 #-}
attrChar2ToW32 fg acChar =
  case unsafeShiftL (fromEnum fg) 8 + unsafeShiftL (Char.ord acChar) 16 of
    I# i -> AttrCharW32 $ W32# (int2Word# i)
{- the hacks save one allocation (?) (before fits-in-32bits check) compared to
  unsafeShiftL (fromEnum fg) 8 + unsafeShiftL (Char.ord acChar) 16
-}
