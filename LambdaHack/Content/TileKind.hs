-- | Terrain tiles for LambdaHack.
module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Feature
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, hardRock, pillar, pillarCache, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpDark, stairsUpLit, stairsDark, stairsLit, stairsDownDark, stairsDownLit, escapeUpDark, escapeUpLit, escapeDownDark, escapeDownLit, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorBrownDark, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit]
  }
wall,        hardRock, pillar, pillarCache, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpDark, stairsUpLit, stairsDark, stairsLit, stairsDownDark, stairsDownLit, escapeUpDark, escapeUpLit, escapeDownDark, escapeDownLit, unknown, floorCorridorLit, floorCorridorDark, floorArenaLit, floorArenaDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorBrownDark, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [("fillerWall", 1), ("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = defBG
  , tcolor2  = defBG
  , tfeature = []
  }
hardRock = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [("basic outer fence", 1)]
  , tcolor   = BrWhite
  , tcolor2  = BrWhite
  , tfeature = [Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("cachable", 70)
               , ("litLegend", 100), ("darkLegend", 100)
               , ("noiseSet", 55), ("combatSet", 3) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit]
  }
pillarCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("litLegend", 100), ("darkLegend", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit, Cause $ Effect.CreateItem 1, ChangeTo "cachable"]
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("combatSet", 8)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfeature = [Lit]
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "granite wall"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit, HideAs "suspect vertical wall"]
  }
wallSuspectV = TileKind
  { tsymbol  = '|'
  , tname    = "moldy wall"
  , tfreq    = [("suspect vertical wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit, Suspect, RevealAs "vertical closed door"]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("vertical closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Lit
               , Exit
               , OpenTo "vertical open door"
               , HideAs "suspect vertical wall"
               ]
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [("vertical open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, Exit
               , CloseTo "vertical closed door"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "granite wall"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit, HideAs "suspect horizontal wall"]
  }
wallSuspectH = TileKind
  { tsymbol  = '-'
  , tname    = "scratched wall"
  , tfreq    = [("suspect horizontal wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit, Suspect, RevealAs "horizontal closed door"]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("horizontal closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Lit
               , Exit
               , OpenTo "horizontal open door"
               , HideAs "suspect horizontal wall"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [("horizontal open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, Exit
               , CloseTo "horizontal closed door"
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
  , tfeature = [Walkable, Clear, Exit, Cause $ Effect.Ascend 1]
  }
stairsUpLit = stairsUpDark
  { tfreq    = [("litLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = Lit : tfeature stairsUpDark
  }
stairsDark = TileKind
  { tsymbol  = '>'
  , tname    = "staircase"
  , tfreq    = [("darkLegend", 100)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan  -- TODO
  , tfeature = [ Walkable, Clear, Exit
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
stairsLit = stairsDark
  { tfreq    = [("litLegend", 100)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan  -- TODO
  , tfeature = Lit : tfeature stairsDark
  }
stairsDownDark = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Cause $ Effect.Ascend (-1)]
  }
stairsDownLit = stairsDownDark
  { tfreq    = [("litLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = Lit : tfeature stairsDownDark
  }
escapeUpDark = TileKind
  { tsymbol  = '<'
  , tname    = "exit trapdoor up"
  , tfreq    = [("darkLegend", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Exit, Cause Effect.Escape]
  }
escapeUpLit = escapeUpDark
  { tfreq    = [("litLegend", 100)]
  , tfeature = Lit : tfeature escapeUpDark
  }
escapeDownDark = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("darkLegend", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Exit, Cause Effect.Escape]
  }
escapeDownLit = escapeDownDark
  { tfreq    = [("litLegend", 100)]
  , tfeature = Lit : tfeature escapeDownDark
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = []
  }
floorCorridorLit = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [("floorCorridorLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tfreq    = [("floorCorridorDark", 1)]
-- Disabled, because dark corridors and yellow light does not fit LambdaHack.
--  , tcolor   = BrYellow
--  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear]
  }
floorArenaLit = floorCorridorLit
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = [ ("floorArenaLit", 1)
               , ("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100) ]
  }
floorArenaDark = floorCorridorDark
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = [("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100)]
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor2  = BrBlack
  }
floorItemLit = floorArenaLit
  { tfreq    = []
  , tfeature = CanItem : tfeature floorArenaLit
  }
floorItemDark = floorArenaDark
  { tfreq    = []
  , tfeature = CanItem : tfeature floorArenaDark
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("litLegend", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemLit
  }
floorActorItemDark = floorItemDark
  { tfreq    = [("darkLegend", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemDark
  }
floorRedDark = floorArenaDark
  { tname    = "brick pavement"
  , tfreq    = [("pathDark", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Path : tfeature floorArenaDark
  }
floorRedLit  = floorRedDark
  { tfreq    = [("pathLit", 30)]
  , tfeature = Lit : tfeature floorRedDark
  }
floorBlueDark = floorRedDark
  { tname    = "granite cobblestones"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorBlueLit = floorBlueDark
  { tfreq    = [("pathLit", 100)]
  , tfeature = Lit : tfeature floorBlueDark
  }
floorGreenDark = floorRedDark
  { tname    = "mossy stone path"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorGreenLit = floorGreenDark
  { tfreq    = [("pathLit", 100)]
  , tfeature = Lit : tfeature floorGreenDark
  }
floorBrownDark = floorRedDark
  { tname    = "rotting mahogany deck"
  , tfreq    = [("pathDark", 10)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorBrownLit = floorBrownDark
  { tfreq    = [("pathLit", 10)]
  , tfeature = Lit : tfeature floorBrownDark
  }
