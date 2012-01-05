module Content.PlaceKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.PlaceKind

cdefs :: Content.CDefs PlaceKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = rvalidate
  , content =
      [rect, pillar, pillarC, pillar3, colonnade, colonnadeW]
  }
rect,        pillar, pillarC, pillar3, colonnade, colonnadeW :: PlaceKind

rect = PlaceKind  -- room is valid for any nonempty area, hence low frequency
  { rsymbol  = 'r'
  , rname    = "room"
  , rfreq    = 100
  , rcover   = CStretch
  , rfence   = FNone
  , rtopLeft = [ "--"
               , "|."
               ]
  }
pillar = PlaceKind
  { rsymbol  = 'p'
  , rname    = "pillar"
  , rfreq    = 1000  -- needs a large area, hence high frequency
  , rcover   = CStretch
  , rfence   = FNone
  , rtopLeft = [ "-----"
               , "|...."
               , "|.O.."
               , "|...."
               , "|...."
              ]
  }
pillarC = pillar
  { rtopLeft = [ "-----"
               , "|O..."
               , "|...."
               , "|...."
               , "|...."
              ]
  }
pillar3 = pillar
  { rtopLeft = [ "-----"
               , "|O.O."
               , "|...."
               , "|O..."
               , "|...."
              ]
  }
colonnade = PlaceKind
  { rsymbol  = 'c'
  , rname    = "colonnade"
  , rfreq    = 5000  -- needs a larger area, hence higher frequency
  , rcover   = CTile
  , rfence   = FFloor
  , rtopLeft = [ ".O.O"
               , "O.O."
               , ".O.O"
               , "O.O."
               ]
  }
colonnadeW = colonnade
  { rtopLeft = [ "O.O."
               , "...."
               , "O.O."
               , "...."
               ]
  }
