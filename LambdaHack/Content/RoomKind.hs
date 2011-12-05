module Content.RoomKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.RoomKind

cdefs :: Content.CDefs RoomKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , valid = validRoom
  , content =
      [rect, oval, ovalW, colonnade, colonnadeW, colonnade2]
  }
rect,        oval, ovalW, colonnade, colonnadeW, colonnade2 :: RoomKind

rect = RoomKind  -- this room is valid for any area, hence low frequency
  { rsymbol           = 'r'
  , rname             = "room"
  , rfreq             = 100
  , rcover            = True
  , rfence            = True
  , rtopLeft          = []
  }
oval = RoomKind  -- needs a large area, hence high frequency
  { rsymbol           = 'o'
  , rname             = "oval room"
  , rfreq             = 100
  , rcover            = False
  , rfence            = True
  , rtopLeft          = [ "####.."
                        , "##...."
                        , "#....."
                        , "#....."
                        , "......"
                        , "......"
                       ]
  }
ovalW = oval  -- without outer solid fence, the pattern visible from outside
  { rfence            = False
  , rtopLeft          = [ "....###"
                        , "..###.."
                        , ".##...."
                        , ".#....."
                        , "##....."
                        , "#......"
                        , "#......"
                       ]
  }
colonnade = RoomKind
  { rsymbol           = 'c'
  , rname             = "colonnade"
  , rfreq             = 50
  , rcover            = True
  , rfence            = True
  , rtopLeft          = [ ".#"
                        , "#."
                        ]
  }
colonnadeW = colonnade
  { rfence            = False
  , rtopLeft          = [ "#.#."
                        , ".#.#"
                        , "#.#."
                        , ".#.#"
                        ]
  }
colonnade2 = colonnade
  { rtopLeft          = [ "..#"
                        , "..#"
                        , "##."
                        ]
  }
-- TODO: obtain all the reet as rooms nested within rooms. 3 rooms are enough,
-- with 1 or 2 tiles between rooms, on all sides, only vertical, only horizontal,
