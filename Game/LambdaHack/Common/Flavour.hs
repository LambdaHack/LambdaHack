{-# LANGUAGE DeriveGeneric #-}
-- | The appearance of in-game items, as communicated to the player.
module Game.LambdaHack.Common.Flavour
  ( -- * The @Flavour@ type
    Flavour
  , -- * Constructors
    zipPlain, zipFancy, zipLiquid, stdFlav
  , -- * Accessors
    flavourToColor, flavourToName
    -- * Assorted
  , colorToPlainName, colorToFancyName, colorToTeamName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , FancyName, colorToLiquidName
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Color

data FancyName = Plain | Fancy | Liquid
  deriving (Show, Eq, Ord, Generic)

instance Hashable FancyName

instance Binary FancyName

-- | The type of item flavours.
data Flavour = Flavour
  { fancyName :: FancyName  -- ^ how fancy should the colour description be
  , baseColor :: Color      -- ^ the colour of the flavour
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Flavour

instance Binary Flavour

-- | Turn a colour set into a flavour set.
zipPlain, zipFancy, zipLiquid :: [Color] -> [Flavour]
zipPlain = map (Flavour Plain)
zipFancy = map (Flavour Fancy)
zipLiquid = map (Flavour Liquid)

-- | The standard full set of flavours.
stdFlav :: [Flavour]
stdFlav = zipPlain stdCol ++ zipFancy stdCol ++ zipLiquid stdCol

-- | Get the underlying base colour of a flavour.
flavourToColor :: Flavour -> Color
flavourToColor Flavour{baseColor} = baseColor

-- | Construct the full name of a flavour.
flavourToName :: Flavour -> Text
flavourToName Flavour{fancyName=Plain, ..} = colorToPlainName baseColor
flavourToName Flavour{fancyName=Fancy, ..} = colorToFancyName baseColor
flavourToName Flavour{fancyName=Liquid, ..} = colorToLiquidName baseColor

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
colorToLiquidName BrBlack   = "pitchy"
colorToLiquidName BrRed     = "red-speckled"
colorToLiquidName BrGreen   = "sappy"
colorToLiquidName BrYellow  = "gold"
colorToLiquidName BrBlue    = "blue-speckled"
colorToLiquidName BrMagenta = "hazy"
colorToLiquidName BrCyan    = "misty"
colorToLiquidName BrWhite   = "shining"

-- | Simple names for team colors (bright colours preferred).
colorToTeamName :: Color -> Text
colorToTeamName BrRed     = "red"
colorToTeamName BrGreen   = "green"
colorToTeamName BrYellow  = "yellow"
colorToTeamName BrBlue    = "blue"
colorToTeamName BrMagenta = "pink"
colorToTeamName BrCyan    = "cyan"
colorToTeamName BrWhite   = "white"
colorToTeamName c = colorToFancyName c
