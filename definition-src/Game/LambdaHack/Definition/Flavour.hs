{-# LANGUAGE DeriveGeneric #-}
-- | The appearance of in-game items, as communicated to the player.
module Game.LambdaHack.Definition.Flavour
  ( -- * The @Flavour@ type
    Flavour
  , -- * Constructors
    zipPlain, zipFancy, zipLiquid, zipGlassPlain, zipGlassFancy, zipStory
  , dummyFlavour, stdFlavList
  , -- * Accessors
    flavourToColor, flavourToName
    -- * Assorted
  , colorToPlainName, colorToFancyName, colorToTeamName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , FancyName, colorToLiquidName, colorToGlassPlainName, colorToGlassFancyName
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import GHC.Generics (Generic)

import Game.LambdaHack.Definition.Color

data FancyName = Plain | Fancy | Liquid | GlassPlain | GlassFancy | Story
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | The type of item flavours.
data Flavour = Flavour
  { fancyName :: FancyName  -- ^ how fancy should the colour description be
  , baseColor :: Color      -- ^ the colour of the flavour
  }
  deriving (Show, Eq, Ord, Generic)

instance Enum Flavour where
  fromEnum Flavour{..} =
    unsafeShiftL (fromEnum fancyName) 8 + fromEnum baseColor
  toEnum n = Flavour (toEnum $ unsafeShiftR n 8)
                     (toEnum $ n .&. (2 ^ (8 :: Int) - 1))

instance Binary Flavour where
  put = put . (toIntegralCrash :: Int -> Word16) . fromEnum
  get = fmap (toEnum . (into :: Word16 -> Int)) get  -- @Int doesn't suffice

dummyFlavour :: Flavour
dummyFlavour = Flavour Story Black

stdFlavList :: [Flavour]
stdFlavList = [Flavour fn bc | fn <- [minBound..maxBound], bc <- stdCol]

-- | Turn a colour set into a flavour set.
zipPlain, zipFancy, zipLiquid, zipGlassPlain, zipGlassFancy, zipStory :: [Color] -> [Flavour]
zipPlain = map (Flavour Plain)
zipFancy = map (Flavour Fancy)
zipLiquid = map (Flavour Liquid)
zipGlassPlain = map (Flavour GlassPlain)
zipGlassFancy = map (Flavour GlassFancy)
zipStory = map (Flavour Story)

-- | Get the underlying base colour of a flavour.
flavourToColor :: Flavour -> Color
flavourToColor Flavour{baseColor} = baseColor

-- | Construct the full name of a flavour.
flavourToName :: Flavour -> Text
flavourToName Flavour{fancyName=Plain, ..} = colorToPlainName baseColor
flavourToName Flavour{fancyName=Fancy, ..} = colorToFancyName baseColor
flavourToName Flavour{fancyName=Liquid, ..} = colorToLiquidName baseColor
flavourToName Flavour{fancyName=GlassPlain, ..} =
  colorToGlassPlainName baseColor
flavourToName Flavour{fancyName=GlassFancy, ..} =
  colorToGlassFancyName baseColor
flavourToName Flavour{fancyName=Story, ..} =
  colorToStoryName baseColor

-- | Human-readable names for item colors. The plain set.
colorToPlainName :: Color -> Text
colorToPlainName Black     = "black"
colorToPlainName Red       = "red"
colorToPlainName Green     = "green"
colorToPlainName Brown     = "brown"
colorToPlainName Blue      = "blue"
colorToPlainName Magenta   = "purple"
colorToPlainName Cyan      = "cyan"
colorToPlainName White     = "ivory"
colorToPlainName AltWhite  = error "colorToPlainName: illegal color"
colorToPlainName BrBlack   = "gray"
colorToPlainName BrRed     = "coral"
colorToPlainName BrGreen   = "lime"
colorToPlainName BrYellow  = "yellow"
colorToPlainName BrBlue    = "azure"
colorToPlainName BrMagenta = "pink"
colorToPlainName BrCyan    = "aquamarine"
colorToPlainName BrWhite   = "white"

-- | Human-readable names for item colors. The fancy set.
colorToFancyName :: Color -> Text
colorToFancyName Black     = "smoky-black"
colorToFancyName Red       = "apple-red"
colorToFancyName Green     = "forest-green"
colorToFancyName Brown     = "mahogany"
colorToFancyName Blue      = "royal-blue"
colorToFancyName Magenta   = "indigo"
colorToFancyName Cyan      = "teal"
colorToFancyName White     = "silver-gray"
colorToFancyName AltWhite  = error "colorToFancyName: illegal color"
colorToFancyName BrBlack   = "charcoal"
colorToFancyName BrRed     = "salmon"
colorToFancyName BrGreen   = "emerald"
colorToFancyName BrYellow  = "amber"
colorToFancyName BrBlue    = "sky-blue"
colorToFancyName BrMagenta = "magenta"
colorToFancyName BrCyan    = "turquoise"
colorToFancyName BrWhite   = "ghost-white"

-- | Human-readable names for item colors. The liquid set.
colorToLiquidName :: Color -> Text
colorToLiquidName Black     = "tarry"
colorToLiquidName Red       = "bloody"
colorToLiquidName Green     = "moldy"
colorToLiquidName Brown     = "muddy"
colorToLiquidName Blue      = "oily"
colorToLiquidName Magenta   = "swirling"
colorToLiquidName Cyan      = "bubbling"
colorToLiquidName White     = "cloudy"
colorToLiquidName AltWhite  = error "colorToLiquidName: illegal color"
colorToLiquidName BrBlack   = "pitchy"
colorToLiquidName BrRed     = "red-speckled"
colorToLiquidName BrGreen   = "sappy"
colorToLiquidName BrYellow  = "golden"
colorToLiquidName BrBlue    = "blue-speckled"
colorToLiquidName BrMagenta = "hazy"
colorToLiquidName BrCyan    = "misty"
colorToLiquidName BrWhite   = "shining"

-- | Human-readable names for item colors. The plain glass set.
colorToGlassPlainName :: Color -> Text
colorToGlassPlainName color = colorToPlainName color <+> "glass"

-- | Human-readable names for item colors. The fancy glass set.
colorToGlassFancyName :: Color -> Text
colorToGlassFancyName color = colorToFancyName color <+> "crystal"

-- | Human-readable names for story item colors.
colorToStoryName :: Color -> Text
colorToStoryName Black     = "unfathomable"
colorToStoryName Red       = "depressing"
colorToStoryName Green     = "confidence-boosting"
colorToStoryName Brown     = "mundane"
colorToStoryName Blue      = "fleeting"
colorToStoryName Magenta   = "complex"
colorToStoryName Cyan      = "wierd"
colorToStoryName White     = "obvious"
colorToStoryName AltWhite  = error "colorToStoryName: illegal color"
colorToStoryName BrBlack   = "inconclusive"
colorToStoryName BrRed     = "troubling"
colorToStoryName BrGreen   = "cherished"
colorToStoryName BrYellow  = "glaring"
colorToStoryName BrBlue    = "profound"
colorToStoryName BrMagenta = "torturous"
colorToStoryName BrCyan    = "peculiar"
colorToStoryName BrWhite   = "explosive"

-- | Simple names for team colors (bright colours preferred).
colorToTeamName :: Color -> Text
colorToTeamName BrBlack   = "black"
colorToTeamName BrRed     = "red"
colorToTeamName BrGreen   = "green"
colorToTeamName BrYellow  = "yellow"
colorToTeamName BrBlue    = "blue"
colorToTeamName BrMagenta = "pink"
colorToTeamName BrCyan    = "cyan"
colorToTeamName BrWhite   = "white"
colorToTeamName c = colorToFancyName c
