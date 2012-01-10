module Game.LambdaHack.Flavour
  ( Flavour
  , zipPlain, zipFancy, darkCol, brightCol, stdCol, stdFlav
  , flavourToColor, flavourToName
  ) where

import qualified Data.List as L

import Game.LambdaHack.Color

-- TODO: add more variety, as the number of items increases
type Flavour = (Color, Bool)  -- the flag tells to use fancy color names

zipPlain, zipFancy :: [Color] -> [(Color, Bool)]
zipPlain cs = L.zip cs (repeat False)
zipFancy cs = L.zip cs (repeat True)

darkCol, brightCol, stdCol :: [Color]
darkCol   = [Red .. Cyan]
brightCol = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol    = darkCol ++ brightCol

stdFlav :: [(Color, Bool)]
stdFlav = zipPlain stdCol ++ zipFancy stdCol

flavourToName :: Flavour -> String
flavourToName (c, False) = colorToName c
flavourToName (c, True)  = colorToName' c

flavourToColor :: Flavour -> Color
flavourToColor (c, _) = c

-- Human-readable names, for item descriptions. The simple set.
colorToName :: Color -> String
colorToName Black     = "black"
colorToName Red       = "red"
colorToName Green     = "green"
colorToName Brown     = "brown"
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
colorToName' Brown     = "mahogany"
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
