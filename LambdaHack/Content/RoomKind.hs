module Content.RoomKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.RoomKind

cdefs :: Content.CDefs RoomKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , content =
      [rect, colonnade, pilaster, pilasterV, peristyle, peristyleV, peristyle3]
  }
rect,        colonnade, pilaster, pilasterV, peristyle, peristyleV, peristyle3 :: RoomKind

rect = RoomKind
  { rsymbol           = 'r'
  , rname             = "room"
  , rfreq             = 100
  , rborderH          = (1, 1)
  , rborderV          = (1, 1)
  , rtopLeft          = [ "##"
                        , "#."
                        ]
  }
colonnade = RoomKind
  { rsymbol           = 'c'
  , rname             = "colonnade"
  , rfreq             = 20
  , rborderH          = (1, 1)
  , rborderV          = (1, 1)
  , rtopLeft          = [ "###"
                        , "#.#"
                        , "##."
                        ]
  }
pilaster = RoomKind
  { rsymbol           = 'p'
  , rname             = "pilaster room"
  , rfreq             = 15
  , rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "###"
                        , "#.#"
                        , "##."
                        ]
  }
pilasterV = pilaster
  { rborderH          = (1, 1)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "###"
                        , "#.."
                        ]
  }
peristyle = RoomKind
  { rsymbol           = 'e'
  , rname             = "peristyle"
  , rfreq             = 10
  , rborderH          = (2, 2)
  , rborderV          = (2, 2)
  , rtopLeft          = [ "#.#"
                        , ".#."
                        , "#.."
                        ]
  }
peristyleV = peristyle
  { rborderV          = (3, 2)
  }
peristyle3 = peristyle
  { rborderH          = (2, 3)
  , rborderV          = (3, 2)
  }
