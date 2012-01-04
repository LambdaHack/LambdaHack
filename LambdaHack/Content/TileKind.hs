module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Feature
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Random as Random

cdefs :: Content.CDefs TileKind
cdefs = Content.CDefs
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, pillar, wallV, doorSecretV, doorClosedV, doorOpenV, wallH, doorSecretH, doorClosedH, doorOpenH, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown]
  }
wall,        pillar, wallV, doorSecretV, doorClosedV, doorOpenV, wallH, doorSecretH, doorClosedH, doorOpenH, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "rock"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 60
  , tfeature = [Special]
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "wall"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
doorSecretV = wallV
  { tfeature = [ Hidden, Secret (Random.RollDice 7 2)
               , ChangeTo "vertical closed door"
               ]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special  -- too hard to choose V or H a closed door for a room
               , Exit, Openable
               , ChangeTo "vertical open door"
               , ChangeFrom "vertical closed door"
               ]
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special  -- to avoid mixing it up with horizontal wall in rooms
               , Walkable, Clear, Exit, Closable
               , ChangeTo "vertical closed door"
               , ChangeFrom "vertical open door"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "wall"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
doorSecretH = wallH
  { tfeature = [ Hidden, Secret (Random.RollDice 7 2)
               , ChangeTo "horizontal closed door"
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special  -- too hard to choose V or H a closed door for a room
               , Exit, Openable
               , ChangeTo "horizontal open door"
               , ChangeFrom "horizontal closed door"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Special  -- to avoid mixing it up with vertical wall in rooms
               , Walkable, Clear, Exit, Closable
               , ChangeTo "horizontal closed door"
               , ChangeFrom "horizontal open door"
               ]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Ascendable, Cause Effect.Ascend]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Descend]
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
  , tname    = "corridor"
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
  , tname    = "stone floor"
  }
floorArenaDark = floorCorridorDark
  { tsymbol  = '.'
  , tname    = "stone floor"
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
  , tfreq    = 30
  , tfeature = Special : tfeature floorArenaLit
  }
floorBlue = floorRed
  { tname    = "granite cobblestones"
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfreq    = 100
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfreq    = 100
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  , tfreq    = 10
  }
