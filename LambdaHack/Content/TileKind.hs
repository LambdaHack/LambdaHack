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
      [wall, wallV, wallH, pillar, doorOpen, doorClosed, doorSecret, doorOpenV, doorClosedV, doorSecretV, doorOpenH, doorClosedH, doorSecretH, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown]
  }
wall,        wallV, wallH, pillar, doorOpen, doorClosed, doorSecret, doorOpenV, doorClosedV, doorSecretV, doorOpenH, doorClosedH, doorSecretH, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "rock"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "wall"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "wall"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Walkable, Clear, Exit, Closable
               , ChangeTo "closed door", ChangeFrom "open door"
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Exit, Openable
               , ChangeTo "open door", ChangeFrom "closed door"
               ]
  }
doorSecret = wall
  { tfeature = [Hidden, Secret (7, 2), ChangeTo "closed door"]
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special, Walkable, Clear, Exit, Closable
               , ChangeTo "vertical closed door"
               , ChangeFrom "vertical open door"
               ]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special, Exit, Openable
               , ChangeTo "vertical open door"
               , ChangeFrom "vertical closed door"
               ]
  }
doorSecretV = wallV
  { tfeature = [ Special, Hidden, Secret (7, 2)
               , ChangeTo "vertical closed door"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special, Walkable, Clear, Exit, Closable
               , ChangeTo "horizontal closed door"
               , ChangeFrom "horizontal open door"
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special, Exit, Openable
               , ChangeTo "horizontal open door"
               , ChangeFrom "horizontal closed door"
               ]
  }
doorSecretH = wallH
  { tfeature = [ Special, Hidden, Secret (7, 2)
               , ChangeTo "horizontal closed door"
               ]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Climbable, Cause Effect.Teleport]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Teleport]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tcolor   = defFG
  , tcolor2  = BrWhite
  , tfreq    = 0
  , tfeature = [Boring]
  }
floorCorridorLit = TileKind
  { tsymbol  = '#'
  , tname    = "dirt"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tfeature = [Walkable, Clear]
  }
floorArenaLit = floorCorridorLit
  { tsymbol  = '.'
  , tname    = "floor"
  }
floorArenaDark = floorCorridorDark
  { tsymbol  = '.'
  , tname    = "floor"
  , tcolor2  = BrBlack
  }
floorRoomLit = floorArenaLit
  { tfeature = Boring : tfeature floorArenaLit
  }
floorRoomDark = floorArenaDark
  { tfeature = Boring : tfeature floorArenaDark
  }
floorRed = floorArenaLit
  { tname    = "brick pavement"
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Special : tfeature floorArenaLit
  }
floorBlue = floorRed
  { tname    = "granite cobblestones"
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
