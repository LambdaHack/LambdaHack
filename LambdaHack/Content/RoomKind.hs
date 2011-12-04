module Content.RoomKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.RoomKind

cdefs :: Content.CDefs RoomKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , content =
      [rect, oval, ovalW, colonnade, colonnadeR, colonnade2, peristyle, peristyleV, peristyle2, peristyle2V, peristyle3, peristyle3V, pilaster, pilasterV, pilaster2, pilaster2V]
  }
rect,        oval, ovalW, colonnade, colonnadeR, colonnade2, peristyle, peristyleV, peristyle2, peristyle2V, peristyle3, peristyle3V, pilaster, pilasterV, pilaster2, pilaster2V :: RoomKind

rect = RoomKind  -- this room is valid for any area
  { rsymbol           = 'r'
  , rname             = "room"
  , rfreq             = 1
  , rborderH          = (0, 0)
  , rborderV          = (0, 0)
  , rtopLeft          = []
  }
oval = RoomKind  -- needs a large area, hence high frequency
  { rsymbol           = 'o'
  , rname             = "oval room"
  , rfreq             = 500
  , rborderH          = (0, 0)
  , rborderV          = (0, 0)
  , rtopLeft          = [ "#####.."
                        , "###...."
                        , "##....."
                        , "#......"
                        , "#......"
                        , "......."
                        , "......."
                       ]
  }
ovalW = oval  -- to use without the outer border
  { rborderH          = (1, 1)
  , rborderV          = (1, 1)
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
  , rfreq             = 200
  , rborderH          = (0, 0)
  , rborderV          = (0, 0)
  , rtopLeft          = [ ".#"
                        , "#."
                        ]
  }
colonnadeR = colonnade
  { rtopLeft          = [ "#."
                        , ".#"
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
-- inner rooms with or without it's border.
peristyle = RoomKind
  { rsymbol           = 'e'
  , rname             = "peristyle"
  , rfreq             = 200
  , rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ ".."
                        , ".#"
                        ]
  }
peristyleV = peristyle
  { rborderH          = (0, 0)
  }
peristyle2 = peristyle
  { rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "#."
                        , ".#"
                        ]
  }
peristyle2V = peristyle
  { rborderH          = (0, 0)
  }
peristyle3 = peristyle
  { rborderH          = (3, 3)
  , rborderV          = (3, 3)
  , rtopLeft          = [ "#.#"
                        , ".#."
                        , "#.#"
                        ]
  }
peristyle3V = peristyle
  { rborderH          = (0, 0)
  }
pilaster = RoomKind
  { rsymbol           = 'p'
  , rname             = "pilaster room"
  , rfreq             = 200
  , rborderH          = (2, 1)
  , rborderV          = (1, 2)
  , rtopLeft          = [ ".#"
                        , "#."
                        ]
  }
pilasterV = pilaster
  { rborderH          = (0, 0)
  , rborderV          = (1, 2)
  , rtopLeft          = [ ".."
                        , "#."
                        ]
  }
pilaster2 = pilaster
  { rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "..#"
                        , "..#"
                        , "##."
                        ]
  }
pilaster2V = pilaster
  { rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "..."
                        , "##."
                        ]
  }
