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
  , tfreq    = [("legend", 100), ("fillerWall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tfreq    = [("legend", 100), ("noiseSet", 60)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "wall"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
doorSecretV = wallV
  { tfreq    = [("hidden", 100)]
  , tfeature = [ Hidden, Secret (Random.RollDice 7 2)
               , ChangeTo "vertical closed door"
               ]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("vertical closed door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [ Exit, Openable
               , ChangeTo "vertical open door"
               ]
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [("vertical open door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, Exit, Closable
               , ChangeTo "vertical closed door"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "wall"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
doorSecretH = wallH
  { tfreq    = [("hidden", 100)]
  , tfeature = [ Hidden, Secret (Random.RollDice 7 2)
               , ChangeTo "horizontal closed door"
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("horizontal closed door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [ Exit, Openable
               , ChangeTo "horizontal open door"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [("horizontal open door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, Exit, Closable
               , ChangeTo "horizontal closed door"
               ]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Ascendable, Cause Effect.Ascend]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Descend]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = BrWhite
  , tfeature = []
  }
floorCorridorLit = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = []
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tfreq    = [("legend", 100), ("darkCorridor", 1)]
  , tfeature = [Walkable, Clear]
  }
floorArenaLit = floorCorridorLit
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = [("noiseSet", 100), ("floorArenaLit", 1)]
  }
floorArenaDark = floorCorridorDark
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = []
  , tcolor2  = BrBlack
  }
floorRoomLit = floorArenaLit
  { tfreq    = [("legend", 100), ("floorRoomLit", 1)]
  , tfeature = Boring : tfeature floorArenaLit
  }
floorRoomDark = floorArenaDark
  { tfreq    = [("floorRoomDark", 1)]
  , tfeature = Boring : tfeature floorArenaDark
  }
floorRed = floorArenaLit
  { tname    = "brick pavement"
  , tfreq    = [("path", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Path : tfeature floorArenaLit
  }
floorBlue = floorRed
  { tname    = "granite cobblestones"
  , tfreq    = [("path", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tfreq    = [("path", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tfreq    = [("path", 10)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
