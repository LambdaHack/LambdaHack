module Content.RoomKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.RoomKind

cdefs :: Content.CDefs RoomKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = rvalidate
  , content =
      [rect, pillar, colonnade, colonnadeW]
  }
rect,        pillar, colonnade, colonnadeW :: RoomKind

rect = RoomKind  -- room is valid for any nonempty area, hence low frequency
  { rsymbol  = 'r'
  , rname    = "room"
  , rfreq    = 100
  , rcover   = CStretch
  , rfence   = FNone
  , rtopLeft = [ "--"
               , "|."
               ]
  }
pillar = RoomKind
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
colonnade = RoomKind
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
