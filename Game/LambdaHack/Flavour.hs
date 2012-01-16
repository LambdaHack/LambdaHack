-- | The appearance of in-game items, as communicated to the player.
module Game.LambdaHack.Flavour
  ( -- * The @Flavour@ type
    Flavour
  , -- * Constructors
    zipPlain, zipFancy, darkCol, brightCol, stdCol, stdFlav
  , -- * Accessors
    flavourToColor, flavourToName
  ) where

import qualified Data.List as L
import Data.Binary

import Game.LambdaHack.Color

-- TODO: add more variety, as the number of items increases
-- | The type of item flavours.
data Flavour = Flavour
  { fancyName :: Bool   -- ^ should the colour description be fancy or plain
  , baseColor :: Color  -- ^ the colour of the flavour
  }
  deriving (Show, Eq, Ord)

instance Binary Flavour where
  put Flavour{..} = do
    put fancyName
    put baseColor
  get = do
    fancyName <- get
    baseColor <- get
    return Flavour{..}

-- | Turn a colour set into a flavour set.
zipPlain, zipFancy :: [Color] -> [Flavour]
zipPlain cs = L.map (Flavour False) cs
zipFancy cs = L.map (Flavour True) cs

-- | Colour sets.
darkCol, brightCol, stdCol :: [Color]
darkCol   = [Red .. Cyan]
brightCol = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol    = darkCol ++ brightCol

-- | The standard full set of flavours.
stdFlav :: [Flavour]
stdFlav = zipPlain stdCol ++ zipFancy stdCol

-- | Get the underlying base colour of a flavour.
flavourToColor :: Flavour -> Color
flavourToColor Flavour{baseColor} = baseColor

-- | Construct the full name of a flavour.
flavourToName :: Flavour -> String
flavourToName Flavour{..} | fancyName = colorToFancyName baseColor
flavourToName Flavour{..} | otherwise = colorToPlainName baseColor

-- | Human-readable names, for item colors. The simple set.
colorToPlainName :: Color -> String
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

-- | Human-readable names, for item colors. The fancy set.
colorToFancyName :: Color -> String
colorToFancyName Black     = "smoky black"
colorToFancyName Red       = "apple red"
colorToFancyName Green     = "forest green"
colorToFancyName Brown     = "mahogany"
colorToFancyName Blue      = "royal blue"
colorToFancyName Magenta   = "indigo"
colorToFancyName Cyan      = "teal"
colorToFancyName White     = "silver gray"
colorToFancyName BrBlack   = "charcoal"
colorToFancyName BrRed     = "salmon"
colorToFancyName BrGreen   = "emerald"
colorToFancyName BrYellow  = "amber"
colorToFancyName BrBlue    = "sky blue"
colorToFancyName BrMagenta = "magenta"
colorToFancyName BrCyan    = "turquoise"
colorToFancyName BrWhite   = "ghost white"
