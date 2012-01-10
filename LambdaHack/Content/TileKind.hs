-- | Terrain tiles for LambdaHack.
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
      [wall, pillar, wallV, doorHiddenV, doorClosedV, doorOpenV, wallH, doorHiddenH, doorClosedH, doorOpenH, stairsUpDark, stairsUpLit, stairsDownDark, stairsDownLit, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown]
  }
wall,        pillar, wallV, doorHiddenV, doorClosedV, doorOpenV, wallH, doorHiddenH, doorClosedH, doorOpenH, stairsUpDark, stairsUpLit, stairsDownDark, stairsDownLit, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "rock"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("fillerWall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("noiseSet", 60)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "wall"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
doorHiddenV = wallV
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
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
doorHiddenH = wallH
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
stairsUpDark = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("darkLegend", 100)]
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Ascendable, Cause Effect.Ascend]
  }
stairsUpLit = stairsUpDark
  { tfreq    = [("litLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = Lit : tfeature stairsUpDark
  }
stairsDownDark = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("darkLegend", 100)]
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Descendable, Cause Effect.Descend]
  }
stairsDownLit = stairsDownDark
  { tfreq    = [("litLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = Lit : tfeature stairsDownDark
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
  { tfreq    = [("darkCorridor", 1)]
-- Disabled, because dark corridors and yellow light does not fit LambdaHack.
--  , tcolor   = BrYellow
--  , tcolor2  = BrBlack
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
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor2  = BrBlack
  }
floorRoomLit = floorArenaLit
  { tfreq    = [("litLegend", 100), ("floorRoomLit", 1)]
  , tfeature = Boring : tfeature floorArenaLit
  }
floorRoomDark = floorArenaDark
  { tfreq    = [("darkLegend", 100)]
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
