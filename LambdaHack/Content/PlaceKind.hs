-- | Rooms, halls and passages for LambdaHack.
module Content.PlaceKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.PlaceKind

cdefs :: Content.CDefs PlaceKind
cdefs = Content.CDefs
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validate = pvalidate
  , content =
      [rect, pillar, pillarC, pillar3, colonnade, colonnadeW]
  }
rect,        pillar, pillarC, pillar3, colonnade, colonnadeW :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|."
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "pillar room"
  , pfreq    = [("rogue", 1000)]  -- Needs a large area, hence high frequency.
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|...."
               , "|.O.."
               , "|...."
               , "|...."
              ]
  }
pillarC = pillar
  { ptopLeft = [ "-----"
               , "|O..."
               , "|...."
               , "|...."
               , "|...."
              ]
  }
pillar3 = pillar
  { ptopLeft = [ "-----"
               , "|O.O."
               , "|...."
               , "|O..."
               , "|...."
              ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 5000)]  -- Needs a large area, hence high frequency.
  , pcover   = CTile
  , pfence   = FFloor
  , ptopLeft = [ ".O.O"
               , "O.O."
               , ".O.O"
               , "O.O."
               ]
  }
colonnadeW = colonnade
  { ptopLeft = [ "O.O."
               , "...."
               , "O.O."
               , "...."
               ]
  }
