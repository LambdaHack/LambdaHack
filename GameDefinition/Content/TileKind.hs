-- | Terrain tiles for LambdaHack.
module Content.TileKind ( cdefs ) where

import Control.Arrow (first)
import Data.Maybe
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
  , validate = validateTileKind
  , content =
      [wall, hardRock, pillar, pillarCache, lampPost, burningTree, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpLit, stairsLit, stairsDownLit, escapeUpLit, escapeDownLit, unknown, floorCorridorLit, floorArenaLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit]
      ++ map makeDark [wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsLit, escapeUpLit, escapeDownLit, floorCorridorLit]
      ++ map makeDarkColor [stairsUpLit, stairsDownLit, floorArenaLit, floorActorLit, floorItemLit, floorActorItemLit]
  }
wall,        hardRock, pillar, pillarCache, lampPost, burningTree, tree, wallV, wallSuspectV, doorClosedV, doorOpenV, wallH, wallSuspectH, doorClosedH, doorOpenH, stairsUpLit, stairsLit, stairsDownLit, escapeUpLit, escapeDownLit, unknown, floorCorridorLit, floorArenaLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit :: TileKind

wall = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)]
  , tcolor   = defBG
  , tcolor2  = defBG
  , tfeature = [Dark]
      -- Bedrock being dark is bad for AI (forces it to backtrack to explore
      -- bedrock at corridor turns) and induces human micromanagement
      -- if there can be corridors joined diagonally (humans have to check
      -- with the cursor if the dark space is bedrock or unexplored).
      -- Lit bedrock would be even worse for humans, because it's harder
      -- to guess which tiles are unknown and which can be explored bedrock.
      -- The setup of Allure is ideal, with lit bedrock that is easily
      -- distinguished from an unknown tile. However, LH follows the NetHack,
      -- not the Angband, visual tradition, so we can't improve the situation,
      -- unless we turn to subtle shades of black or non-ASCII glyphs,
      -- but that is yet different aesthetics and it's inconsistent
      -- with console frontends.
  }
hardRock = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [("basic outer fence", 1)]
  , tcolor   = BrWhite
  , tcolor2  = BrWhite
  , tfeature = [Dark, Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("cachable", 70)
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 55), ("skirmishSet", 5)
               , ("battleSet", 9) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("legendLit", 100), ("legendDark", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Cause $ Effect.CreateItem 1, ChangeTo "cachable"]
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("ambushOverLamp_O", 90)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , tfeature = []
  }
burningTree = TileKind
  { tsymbol  = 'O'
  , tname    = "burning tree"
  , tfreq    = [("ambushOverLamp_O", 10), ("ambushSet", 3)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = []
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [ ("skirmishSet", 12), ("ambushSet", 100)
               , ("skirmishOverTree_O", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfeature = [Dark]
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "granite wall"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect vertical wall Lit"]
  }
wallSuspectV = TileKind
  { tsymbol  = '|'
  , tname    = "moldy wall"
  , tfreq    = [("suspect vertical wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Suspect, RevealAs "vertical closed door Lit"]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("vertical closed door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ OpenTo "vertical open door Lit"
               , HideAs "suspect vertical wall Lit"
               ]
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [("vertical open door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "vertical closed door Lit"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "granite wall"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect horizontal wall Lit"]
  }
wallSuspectH = TileKind
  { tsymbol  = '-'
  , tname    = "scratched wall"
  , tfreq    = [("suspect horizontal wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Suspect, RevealAs "horizontal closed door Lit"]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("horizontal closed door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ OpenTo "horizontal open door Lit"
               , HideAs "suspect horizontal wall Lit"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [("horizontal open door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "horizontal closed door Lit"
               ]
  }
stairsUpLit = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend 1]
  }
stairsLit = TileKind
  { tsymbol  = '>'
  , tname    = "staircase"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan  -- TODO
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
stairsDownLit = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend (-1)]
  }
escapeUpLit = TileKind
  { tsymbol  = '<'
  , tname    = "exit trapdoor up"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause (Effect.Escape 1)]
  }
escapeDownLit = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause (Effect.Escape (-1))]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = [Dark]
  }
floorCorridorLit = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [("floorCorridorLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear]
  }
floorArenaLit = floorCorridorLit
  { tsymbol  = '.'
  , tname    = "stone floor"
  , tfreq    = [ ("floorArenaLit", 1)
               , ("arenaSet", 1), ("emptySet", 1), ("noiseSet", 100)
               , ("battleSet", 100), ("skirmishSet", 100), ("ambushSet", 1000) ]
  }
floorActorLit = floorArenaLit
  { tfreq    = []
  , tfeature = OftenActor : tfeature floorArenaLit
  }
floorItemLit = floorArenaLit
  { tfreq    = []
  , tfeature = OftenItem : tfeature floorArenaLit
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("legendLit", 100)]  -- no OftenItem in legendDark
  , tfeature = OftenActor : tfeature floorItemLit
  }
floorArenaShade = floorActorLit
  { tname    = "stone floor"  -- TODO: "shaded ground"
  , tfreq    = [("skirmishOverTree_s", 1), ("skirmishOverRock_s", 1)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature floorActorLit  -- no OftenItem
  }
floorRedLit = floorArenaLit
  { tname    = "brick pavement"
  , tfreq    = [("trailLit", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Trail : tfeature floorArenaLit
  }
floorBlueLit = floorRedLit
  { tname    = "granite cobblestones"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreenLit = floorRedLit
  { tname    = "mossy stone path"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrownLit = floorRedLit
  { tname    = "rotting mahogany deck"
  , tfreq    = [("trailLit", 10)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }

makeDark :: TileKind -> TileKind
makeDark k = let darkText :: Text -> Text
                 darkText t = maybe t (<> "Dark") $ T.stripSuffix "Lit" t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat OftenItem = Nothing
                 darkFeat feat = Just $ feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) {tcolor2 = BrBlack}
