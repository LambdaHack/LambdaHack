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
      [rect, oval, colonnade, colonnadeW]
  }
rect,        oval, colonnade, colonnadeW :: RoomKind

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
oval = RoomKind  -- needs a large area, hence high frequency
  { rsymbol  = 'o'
  , rname    = "oval room"
  , rfreq    = 1000
  , rcover   = CStretch
  , rfence   = FFloor
  , rtopLeft = [ "...|--."
               , ".--|..."
               , ".|....."
               , "--....."
               , "|......"
               , "|......"
               , "......."
               ]
  }
colonnade = RoomKind
  { rsymbol  = 'c'
  , rname    = "colonnade"
  , rfreq    = 100
  , rcover   = CTile
  , rfence   = FFloor
  , rtopLeft = [ ".O"
               , "O."
               ]
  }
colonnadeW = colonnade
  { rfence   = FFloor
  , rtopLeft = [ "O.O."
               , "...."
               , "O.O."
               , "...."
               ]
  }
