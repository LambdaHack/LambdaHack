-- | Terrain tiles for LambdaHack.
module Content.TileKind ( cdefs ) where

import Control.Arrow (first)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Feature
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, hardRock, pillar, pillarCache, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpDark, stairsDark, stairsDownDark, escapeUpDark, escapeDownDark, unknown, floorCorridorDark, floorArenaDark, floorItemDark, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorBrownDark]
      ++ map makeLit [stairsDark, escapeUpDark, escapeDownDark, floorCorridorDark, floorRedDark, floorBlueDark, floorGreenDark, floorBrownDark]
      ++ map makeLitDefFG [stairsUpDark, stairsDownDark, floorArenaDark, floorItemDark, floorActorItemDark]
  }
wall,        hardRock, pillar, pillarCache, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpDark, stairsDark, stairsDownDark, escapeUpDark, escapeDownDark, unknown, floorCorridorDark, floorArenaDark, floorItemDark, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorBrownDark :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)]
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
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 55), ("combatSet", 3) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Lit]
  }
pillarCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("legendLit", 100), ("legendDark", 100) ]
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
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
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
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
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
  , tfreq    = [("legendDark", 100)]
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Cause $ Effect.Ascend 1]
  }
stairsDark = TileKind
  { tsymbol  = '>'
  , tname    = "staircase"
  , tfreq    = [("legendDark", 100)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan  -- TODO
  , tfeature = [ Walkable, Clear, Exit
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
stairsDownDark = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("legendDark", 100)]
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Cause $ Effect.Ascend (-1)]
  }
escapeUpDark = TileKind
  { tsymbol  = '<'
  , tname    = "exit trapdoor up"
  , tfreq    = [("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Exit, Cause Effect.Escape]
  }
escapeDownDark = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Exit, Cause Effect.Escape]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = []
  }
floorCorridorDark = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [("floorCorridorDark", 1)]
-- Disabled, because dark corridors and yellow light does not fit LambdaHack.
--  , tcolor   = BrYellow
--  , tcolor2  = BrBlack
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear]
  }
floorArenaDark = floorCorridorDark
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = [ ("floorArenaDark", 1)
               , ("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100) ]
-- Disabled, because the yellow artificial light does not fit LambdaHack.
--  , tcolor   = BrYellow
-- Dark room interior, OTOH, is fine:
  , tcolor2  = BrBlack
  }
floorItemDark = floorArenaDark
  { tfreq    = []
  , tfeature = CanItem : tfeature floorArenaDark
  }
floorActorItemDark = floorItemDark
  { tfreq    = [("legendDark", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemDark
  }
floorRedDark = floorArenaDark
  { tname    = "brick pavement"
  , tfreq    = [("pathDark", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Path : tfeature floorArenaDark
  }
floorBlueDark = floorRedDark
  { tname    = "granite cobblestones"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreenDark = floorRedDark
  { tname    = "mossy stone path"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrownDark = floorRedDark
  { tname    = "rotting mahogany deck"
  , tfreq    = [("pathDark", 10)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }

makeLit :: TileKind -> TileKind
makeLit k = let textLit :: Text -> Text
                textLit t = maybe t (<> "Lit") $ T.stripSuffix "Dark" t
                litFreq = map (first textLit) $ tfreq k
            in k { tfreq    = litFreq
                 , tfeature = Lit : tfeature k
                 }

makeLitDefFG :: TileKind -> TileKind
makeLitDefFG k = (makeLit k) {tcolor2  = defFG}
