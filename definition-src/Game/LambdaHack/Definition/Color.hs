{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, MagicHash #-}
-- | Colours and text attributes.
module Game.LambdaHack.Definition.Color
  ( -- * Colours
    Color(..)
  , defFG, isBright, darkCol, brightCol, stdCol, legalFgCol
  , cVeryBadEvent, cBadEvent, cRisk, cGraveRisk, cVeryGoodEvent, cGoodEvent
  , cVista, cSleep, cWakeUp, cGreed, cNeutralEvent, cRareNeutralEvent
  , cIdentification, cMeta, cBoring, cGameOver
  , colorToRGB
    -- * Complete text attributes
  , Highlight (..), Attr(..)
  , highlightToColor, defAttr
    -- * Characters with attributes
  , AttrChar(..), AttrCharW32(..)
  , attrCharToW32, attrCharFromW32
  , fgFromW32, bgFromW32, charFromW32, attrFromW32
  , spaceAttrW32, nbspAttrW32, spaceCursorAttrW32, trimmedLineAttrW32
  , attrChar2ToW32, attrChar1ToW32
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import           Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import qualified Data.Char as Char
import           Data.Hashable (Hashable)
import           GHC.Exts (Int (I#))
import           GHC.Generics (Generic)
import           GHC.Prim (int2Word#)
import           GHC.Word (Word32 (W32#))

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
  | AltWhite  -- only use for frontend hacks
  | BrBlack
  | BrRed
  | BrGreen
  | BrYellow
  | BrBlue
  | BrMagenta
  | BrCyan
  | BrWhite
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary Color where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Hashable Color

instance NFData Color

-- | The default colours, to optimize attribute setting.
defFG :: Color
defFG = White

-- | A helper for the terminal frontends that display bright via bold.
isBright :: Color -> Bool
isBright c = c > BrBlack

-- | Colour sets. Sorted.
darkCol, brightCol, stdCol, legalFgCol :: [Color]
darkCol = [Red .. Cyan]
brightCol = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol = darkCol ++ brightCol
legalFgCol = darkCol ++ [White, BrBlack] ++ brightCol ++ [BrWhite]

-- See the discussion of colours and the table of colours at
-- https://github.com/LambdaHack/LambdaHack/wiki/Display#colours
-- Another mention of colours, concerning terrain, is in PLAYING.md manual.
-- The manual and this code should follow the wiki.
cVeryBadEvent, cBadEvent, cRisk, cGraveRisk, cVeryGoodEvent, cGoodEvent, cVista, cSleep, cWakeUp, cGreed, cNeutralEvent, cRareNeutralEvent, cIdentification, cMeta, cBoring, cGameOver :: Color
cVeryBadEvent = Red
cBadEvent = BrRed
cRisk = Magenta
cGraveRisk = BrMagenta
cVeryGoodEvent = Green
cGoodEvent = BrGreen
cVista = BrGreen
cSleep = Blue
cWakeUp = BrBlue
cGreed = BrBlue
cNeutralEvent = Cyan
cRareNeutralEvent = BrCyan
cIdentification = Brown
cMeta = BrYellow
cBoring = White
cGameOver = BrWhite

-- | Translationg to heavily modified Linux console color RGB values.
--
-- Warning: SDL frontend sadly duplicates this code.
colorToRGB :: Color -> Text
colorToRGB Black     = "#000000"
colorToRGB Red       = "#D50505"
colorToRGB Green     = "#059D05"
colorToRGB Brown     = "#CA4A05"
colorToRGB Blue      = "#0556F4"
colorToRGB Magenta   = "#AF0EAF"
colorToRGB Cyan      = "#059696"
colorToRGB White     = "#B8BFCB"
colorToRGB AltWhite  = "#C4BEB1"
colorToRGB BrBlack   = "#6F5F5F"
colorToRGB BrRed     = "#FF5555"
colorToRGB BrGreen   = "#65F136"
colorToRGB BrYellow  = "#EBD642"
colorToRGB BrBlue    = "#4D98F4"
colorToRGB BrMagenta = "#FF77FF"
colorToRGB BrCyan    = "#52F4E5"
colorToRGB BrWhite   = "#FFFFFF"

-- -- | For reference, the original Linux console colors.
-- -- Good old retro feel and more useful than xterm (e.g. brown).
-- colorToRGB :: Color -> Text
-- colorToRGB Black     = "#000000"
-- colorToRGB Red       = "#AA0000"
-- colorToRGB Green     = "#00AA00"
-- colorToRGB Brown     = "#AA5500"
-- colorToRGB Blue      = "#0000AA"
-- colorToRGB Magenta   = "#AA00AA"
-- colorToRGB Cyan      = "#00AAAA"
-- colorToRGB White     = "#AAAAAA"
-- colorToRGB AltWhite  = "#AAAAAA"
-- colorToRGB BrBlack   = "#555555"
-- colorToRGB BrRed     = "#FF5555"
-- colorToRGB BrGreen   = "#55FF55"
-- colorToRGB BrYellow  = "#FFFF55"
-- colorToRGB BrBlue    = "#5555FF"
-- colorToRGB BrMagenta = "#FF55FF"
-- colorToRGB BrCyan    = "#55FFFF"
-- colorToRGB BrWhite   = "#FFFFFF"

-- | Additional map cell highlight, e.g., a colorful square around the cell
-- or a colorful background.
--
-- Note: the highlight underscored by the terminal cursor is
-- the maximal element of this type present of this screen.
data Highlight =
    HighlightNone
  | HighlightGreen
  | HighlightBlue
  | HighlightBrown
  | HighlightCyan
  | HighlightGrey
  | HighlightWhite
  | HighlightMagenta
  | HighlightRed
  | HighlightYellow
  | HighlightYellowAim
  | HighlightRedAim
  | HighlightNoneCursor
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

highlightToColor :: Highlight -> Color
highlightToColor hi = case hi of
  HighlightNone -> Black  -- should be transparent, but is OK in web frontend
  HighlightGreen -> Green
  HighlightBlue -> Blue
  HighlightBrown -> Brown
  HighlightCyan -> Cyan
  HighlightGrey -> BrBlack
  HighlightWhite -> White  -- bright, but no saturation, so doesn't obscure much
  HighlightMagenta -> BrMagenta  -- very rare, so bright is fine
  HighlightRed -> Red
  HighlightYellow -> BrYellow  -- obscures, but mostly used around bright white
  HighlightYellowAim -> BrYellow
  HighlightRedAim -> Red
  HighlightNoneCursor -> Black  -- used in vty for cursor via @maxIndexByA@

-- | Text attributes: foreground color and highlight.
data Attr = Attr
  { fg :: Color      -- ^ foreground colour
  , bg :: Highlight  -- ^ highlight
  }
  deriving (Show, Eq)

-- | The default attribute, to optimize attribute setting.
defAttr :: Attr
defAttr = Attr defFG HighlightNone

-- | Character to display, with its attribute.
data AttrChar = AttrChar
  { acAttr :: Attr
  , acChar :: Char
  }
  deriving (Show, Eq)

-- This implementation is faster than @Int@, because some vector updates
-- can be done without going to and from @Int@.
-- | Optimized representation of 'AttrChar'.
newtype AttrCharW32 = AttrCharW32 {attrCharW32 :: Word32}
  deriving (Show, Eq, Ord, Enum, Binary)

attrCharToW32 :: AttrChar -> AttrCharW32
attrCharToW32 AttrChar{acAttr=Attr{..}, acChar} = AttrCharW32 $ toEnum $
  unsafeShiftL (fromEnum fg) 8 + fromEnum bg + unsafeShiftL (Char.ord acChar) 16

attrCharFromW32 :: AttrCharW32 -> AttrChar
attrCharFromW32 !w = AttrChar (attrFromW32 w) (charFromW32 w)

fgFromW32 :: AttrCharW32 -> Color
{-# INLINE fgFromW32 #-}
fgFromW32 w =
  toEnum $ unsafeShiftR (fromEnum $ attrCharW32 w) 8 .&. (2 ^ (8 :: Int) - 1)

bgFromW32 :: AttrCharW32 -> Highlight
{-# INLINE bgFromW32 #-}
bgFromW32 w =
  toEnum $ fromEnum $ attrCharW32 w .&. (2 ^ (8 :: Int) - 1)

charFromW32 :: AttrCharW32 -> Char
{-# INLINE charFromW32 #-}
charFromW32 w =
  Char.chr $ unsafeShiftR (fromEnum $ attrCharW32 w) 16

attrFromW32 :: AttrCharW32 -> Attr
{-# INLINE attrFromW32 #-}
attrFromW32 w = Attr (fgFromW32 w) (bgFromW32 w)

spaceAttrW32 :: AttrCharW32
spaceAttrW32 = attrCharToW32 $ AttrChar defAttr ' '

nbspAttrW32 :: AttrCharW32
nbspAttrW32 = attrCharToW32 $ AttrChar defAttr '\x00a0'

spaceCursorAttrW32 :: AttrCharW32
spaceCursorAttrW32 =
  attrCharToW32 $ AttrChar (defAttr {bg = HighlightNoneCursor}) ' '

trimmedLineAttrW32 :: AttrCharW32
trimmedLineAttrW32 = attrChar2ToW32 BrBlack '$'

attrChar2ToW32 :: Color -> Char -> AttrCharW32
{-# INLINE attrChar2ToW32 #-}
attrChar2ToW32 fg acChar =
  case unsafeShiftL (fromEnum fg) 8 + unsafeShiftL (Char.ord acChar) 16 of
    I# i -> AttrCharW32 $ W32# (int2Word# i)
{- the hacks save one allocation (?) (before fits-in-32bits check) compared to
  AttrCharW32 $ toEnum
  $ unsafeShiftL (fromEnum fg) 8 + unsafeShiftL (Char.ord acChar) 16
-}

attrChar1ToW32 :: Char -> AttrCharW32
{-# INLINE attrChar1ToW32 #-}
attrChar1ToW32 =
  let fgNum = unsafeShiftL (fromEnum White) 8
  in \acChar ->
    case fgNum + unsafeShiftL (Char.ord acChar) 16 of
      I# i -> AttrCharW32 $ W32# (int2Word# i)
