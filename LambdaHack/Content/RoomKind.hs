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
      [rect, oval, ovalW, ovalD, colonnade, colonnadeF, colonnadeW, cells]
  }
rect,        oval, ovalW, ovalD, colonnade, colonnadeF, colonnadeW, cells :: RoomKind

rect = RoomKind  -- this room is valid for any nonempty area, hence low frequency
  { rsymbol  = 'r'
  , rname    = "room"
  , rfreq    = 100
  , rcover   = CTile
  , rfence   = FWall
  , rtopLeft = ["."]
  }
oval = RoomKind  -- needs a large area, hence high frequency
  { rsymbol  = 'o'
  , rname    = "oval room"
  , rfreq    = 100
  , rcover   = CStretch
  , rfence   = FWall
  , rtopLeft = [ "####.."
               , "##...."
               , "#....."
               , "#....."
               , "......"
               , "......"
               ]
  }
ovalW = oval  -- without outer solid fence, the pattern visible from outside
  { rfence   = FFloor
  , rtopLeft = [ "....+#"
               , "..###."
               , ".##..."
               , ".#...."
               , "+#...."
               , "#....."
               ]
  }
ovalD = ovalW
  { rtopLeft = [ ".###+"
               , "##..."
               , "#...."
               , "#...."
               , "+...."
               ]
  }
colonnade = RoomKind
  { rsymbol  = 'c'
  , rname    = "colonnade"
  , rfreq    = 50
  , rcover   = CTile
  , rfence   = FNone
  , rtopLeft = [ ".#.#"
               , "#.#."
               , ".#.#"
               , "#.#."
               ]
  }
colonnadeF = colonnade
  { rfence   = FFloor
  , rtopLeft = [ "#.#."
               , ".#.#"
               , "#.#."
               , ".#.#"
               ]
  }
colonnadeW = colonnade
  { rfence   = FWall
  , rtopLeft = [ "...."
               , ".#.#"
               , "...."
               , ".#.#"
               ]
  }
cells = RoomKind
  { rsymbol  = '#'
  , rname    = "cells"
  , rfreq    = 50
  , rcover   = CReflect
  , rfence   = FWall
  , rtopLeft = [ "..#"
               , "..#"
               , "##."
               ]
  }
-- TODO: obtain all the reet as rooms nested within rooms. 3 rooms are enough,
-- with 1 or 2 tiles between rooms, on all sides, only vertical, only horizontal,
