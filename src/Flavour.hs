module Flavour where

import qualified Data.List as L
import Color

-- TODO: add more variety, as the number of items increases
type Flavour = (Color, Bool)  -- the flag tells to use fancy color names

zipPlain, zipFancy :: [Color] -> [(Color, Bool)]
zipPlain cs = L.zip cs (repeat False)
zipFancy cs = L.zip cs (repeat True)

darkCol, brightCol, stdCol :: [Color]
darkCol    = [Red .. Cyan]
brightCol  = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol     = darkCol ++ brightCol

stdFlav :: [(Color, Bool)]
stdFlav    = zipPlain stdCol ++ zipFancy stdCol

flavourToName :: Flavour -> String
flavourToName (c, False) = colorToName c
flavourToName (c, True) = colorToName' c

flavourToColor :: Flavour -> Color
flavourToColor (c, _) = c
