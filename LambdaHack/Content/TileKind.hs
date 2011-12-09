module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Feature
import Game.LambdaHack.Content.TileKind

cdefs :: Content.CDefs TileKind
cdefs = Content.CDefs
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown]
  }
wall,        doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "A wall."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "An open door."
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit, Change '+', Closable]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "A closed door."
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Exit, Change '\'', Openable]
  }
doorSecret = wall
  { tfeature = [Hidden, Change '+', Secret (7, 2)]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "A staircase up."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Climbable, Cause Effect.Teleport]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "A staircase down."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Teleport]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "An unknown space."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 0
  , tfeature = []
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "Dirt."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tcolor   = BrYellow
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear]
  }
floorRoomLit = floorCorridorLit
  { tfeature = Boring : tfeature floorCorridorLit
  }
floorRoomDark = floorCorridorDark
  { tfeature = Boring : tfeature floorCorridorDark
  }
floorRed = floorCorridorLit
  { tname    = "Brick pavement."
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Special : tfeature floorCorridorLit
  }
floorBlue = floorRed
  { tname    = "Granite cobblestones."
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "Mossy stone path."
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "Rotting mahogany deck."
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
