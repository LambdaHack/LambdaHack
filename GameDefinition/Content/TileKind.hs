-- | Terrain tile definitions.
module Content.TileKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs

content :: [TileKind]
content =
  [unknown, unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, doorTrappedH, doorClosedH, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, wallGlassH, wallGlassHSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, fog, fogDark, smoke, smokeDark, doorOpen, doorOpenH, floorCorridor, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorAshes, shallowWater, shallowWaterSpice, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade, outdoorFence ]
  ++ map makeDark ldarkable
  ++ map makeDarkColor ldarkColorable

unknown,    unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, doorTrappedH, doorClosedH, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, wallGlassH, wallGlassHSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, fog, fogDark, smoke, smokeDark, doorOpen, doorOpenH, floorCorridor, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorAshes, shallowWater, shallowWaterSpice, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade, outdoorFence :: TileKind

ldarkable :: [TileKind]
ldarkable = [wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, doorTrapped, doorClosed, doorTrappedH, doorClosedH, wallGlass, wallGlassSpice, wallGlassH, wallGlassHSpice, doorOpen, doorOpenH, floorCorridor, shallowWater, shallowWaterSpice]

ldarkColorable :: [TileKind]
ldarkColorable = [tree, bush, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem]

-- Symbols to be used (the Nethack visual tradition imposes inconsistency):
--         LOS    noLOS
-- Walk    .|-#~  :;
-- noWalk  %^-|   -| O&<>+
--
-- can be opened ^&+
-- can be closed |-
-- some noWalk can be changed without opening, regardless of symbol
-- not used yet:
-- : (curtain, etc., not flowing, but solid and static)
-- `' (not visible enough when immobile)

-- Note that for AI hints and UI comfort, most multiple-use @Embed@ tiles
-- should have a variant, which after first use transforms into a different
-- colour tile without @ChangeTo@ and similar (which then AI no longer touches).
-- If a tile is supposed to be repeatedly activated by AI (e.g., cache),
-- it should keep @ChangeTo@ for the whole time.

-- * Main tiles, in other games modified and some removed

-- ** Not walkable

-- *** Not clear

unknown = TileKind  -- needs to have index 0 and alter 1; no other with 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
unknownOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown outer fence", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
basicOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [("basic outer fence", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
bedrock = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [Dark]
      -- Bedrock being dark is bad for AI (forces it to backtrack to explore
      -- bedrock at corridor turns) and induces human micromanagement
      -- if there can be corridors joined diagonally (humans have to check
      -- with the xhair if the dark space is bedrock or unexplored).
      -- Lit bedrock would be even worse for humans, because it's harder
      -- to guess which tiles are unknown and which can be explored bedrock.
      -- The setup of Allure is ideal, with lit bedrock that is easily
      -- distinguished from an unknown tile. However, LH follows the NetHack,
      -- not the Angband, visual tradition, so we can't improve the situation,
      -- unless we turn to subtle shades of black or non-ASCII glyphs,
      -- but that is yet different aesthetics.
  }
wall = TileKind
  { tsymbol  = '|'
  , tname    = "granite wall"
  , tfreq    = [ ("legendLit", 100), ("wall Lit", 100)
               , ("rectWindowsOver_!_Lit", 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs "suspect vertical wall Lit"]
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '|'
  , tname    = "suspect uneven wall"
  , tfreq    = [("suspect vertical wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped vertical door Lit"
               , ObscureAs "obscured vertical wall Lit"
               ]
  }
wallObscured = TileKind
  { tsymbol  = '|'
  , tname    = "scratched wall"
  , tfreq    = [("obscured vertical wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "scratch on wall"
               , HideAs "suspect vertical wall Lit"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "sandstone wall"
  , tfreq    = [ ("legendLit", 100), ("wallH Lit", 100)
               , ("rectWindowsOver_=_Lit", 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs "suspect horizontal wall Lit"]
  }
wallSuspectH = TileKind  -- only on client
  { tsymbol  = '-'
  , tname    = "suspect painted wall"
  , tfreq    = [("suspect horizontal wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped horizontal door Lit"
               , ObscureAs "obscured horizontal wall Lit"
               ]
  }
wallObscuredDefacedH = TileKind
  { tsymbol  = '-'
  , tname    = "defaced wall"
  , tfreq    = [("obscured horizontal wall Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "obscene pictogram"
               , HideAs "suspect horizontal wall Lit"
               ]
  }
wallObscuredFrescoedH = TileKind
  { tsymbol  = '-'
  , tname    = "frescoed wall"
  , tfreq    = [("obscured horizontal wall Lit", 10)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "subtle fresco"
               , HideAs "suspect horizontal wall Lit"
               ]  -- a bit beneficial, but AI would loop if allowed to trigger
                  -- so no @ConsideredByAI@
  }
pillar = TileKind
  { tsymbol  = '0'
  , tname    = "rock"
  , tfreq    = [ ("cachable", 70)
               , ("stair terminal Lit", 100), ("stair terminal Dark", 100)
               , ("legendLit", 100), ("legendDark", 100)
               , ("emptySetLit", 20), ("noiseSetLit", 700)
               , ("powerSetDark", 700)
               , ("battleSetDark", 200), ("brawlSetLit", 50)
               , ("shootoutSetLit", 10), ("zooSetDark", 10) ]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = '0'
  , tname    = "smoothed rock"
  , tfreq    = [("cachable", 30), ("cache", 1), ("stair terminal Dark", 4)]
                 -- treasure only in dark staircases
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed "treasure cache", Embed "treasure cache trap"
               , ChangeTo "cachable", ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger, unless wary of traps.
  }
lampPost = TileKind
  { tsymbol  = '0'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_0", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [("signboard unread", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ ConsideredByAI  -- changes after use, so safe for AI
               , RevealAs "signboard"  -- to display as hidden
               ]
  }
signboardRead = TileKind
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [("signboard", 1), ("escapeSetDark", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed "signboard", HideAs "signboard unread"]
  }
tree = TileKind
  { tsymbol  = '0'
  , tname    = "tree"
  , tfreq    = [ ("brawlSetLit", 140), ("shootoutSetLit", 10)
               , ("escapeSetLit", 35), ("ambushSetLit", 3)
               , ("treeShadeOver_0_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [ ("ambushSetDark", 3), ("zooSetDark", 7), ("battleSetDark", 50)
               , ("tree with fire", 30) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [ ("ambushSetDark", 15), ("zooSetDark", 70)
               , ("tree with fire", 70) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "big fire" : ChangeTo "tree with fire" : tfeature tree
      -- TODO: dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ ("rubble", 1), ("legendLit", 1), ("legendDark", 1)
               , ("stair terminal Lit", 4), ("stair terminal Dark", 4)
               , ("emptySetLit", 10), ("emptySetDark", 10)
               , ("noiseSetLit", 50), ("powerSetDark", 50)
               , ("zooSetDark", 100), ("ambushSetDark", 10) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [OpenTo "floorAshesLit", Embed "rubble"]
      -- It's not explorable, due to not being walkable nor clear and due
      -- to being a door (@OpenTo@), which is kind of OK, because getting
      -- the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
  }
rubbleSpice = rubble
  { tfreq    = [("smokeClumpOver_f_Lit", 1), ("smokeClumpOver_f_Dark", 1)]
  , tfeature = Spice : tfeature rubble
  }
doorTrapped = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [("trapped vertical door Lit", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed "doorway trap"
               , OpenTo "open vertical door Lit"
               , HideAs "suspect vertical wall Lit"
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("closed vertical door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open vertical door Lit"]  -- never hidden
  }
doorTrappedH = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [("trapped horizontal door Lit", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed "doorway trap"
               , OpenTo "open horizontal door Lit"
               , HideAs "suspect horizontal wall Lit"
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("closed horizontal door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open horizontal door Lit"]  -- never hidden
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("staircase up", 9), ("ordinary staircase up", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase up", ConsideredByAI]
  }
stairsTrappedUp = TileKind
  { tsymbol  = '<'
  , tname    = "windy staircase up"
  , tfreq    = [("staircase up", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase up", Embed "staircase trap up"
               , ConsideredByAI, ChangeTo "ordinary staircase up" ]
                 -- AI uses despite the trap; exploration more important
  }
stairsOutdoorUp = stairsUp
  { tname    = "signpost pointing backward"
  , tfreq    = [("staircase outdoor up", 1)]
  }
stairsGatedUp = stairsUp
  { tname    = "gated staircase up"
  , tfreq    = [("gated staircase up", 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("staircase down", 9), ("ordinary staircase down", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase down", ConsideredByAI]
  }
stairsTrappedDown = TileKind
  { tsymbol  = '>'
  , tname    = "crooked staircase down"
  , tfreq    = [("staircase down", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase down", Embed "staircase trap down"
               , ConsideredByAI, ChangeTo "ordinary staircase down" ]
  }
stairsOutdoorDown = stairsDown
  { tname    = "signpost pointing forward"
  , tfreq    = [("staircase outdoor down", 1)]
  }
stairsGatedDown = stairsDown
  { tname    = "gated staircase down"
  , tfreq    = [("gated staircase down", 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [("legendLit", 1), ("legendDark", 1), ("escape up", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 1), ("legendDark", 1), ("escape down", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [("escape outdoor down", 1)]
  }

-- *** Clear

wallGlass = TileKind
  { tsymbol  = '|'
  , tname    = "polished crystal wall"
  , tfreq    = [("glasshouseOver_!_Lit", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs "closed vertical door Lit", Clear]
  }
wallGlassSpice = wallGlass
  { tfreq    = [("rectWindowsOver_!_Lit", 20)]
  , tfeature = Spice : tfeature wallGlass
  }
wallGlassH = TileKind
  { tsymbol  = '-'
  , tname    = "polished crystal wall"
  , tfreq    = [("glasshouseOver_=_Lit", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs "closed horizontal door Lit", Clear]
  }
wallGlassHSpice = wallGlassH
  { tfreq    = [("rectWindowsOver_=_Lit", 20)]
  , tfeature = Spice : tfeature wallGlassH
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "icy outcrop"
  , tfreq    = [("powerSetDark", 300)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [Clear, Embed "frost", OpenTo "shallow water Lit"]
      -- Is door, due to @OpenTo@, so is not explorable, but it's OK, because
      -- it doesn't generate items nor clues. This saves on the need to
      -- get each ice pillar into sight range when exploring level.
  }
pulpit = TileKind
  { tsymbol  = '%'
  , tname    = "pulpit"
  , tfreq    = [("pulpit", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [Clear, Embed "pulpit"]
                 -- mixed blessing, so AI ignores, saved for player fun
  }
bush = TileKind
  { tsymbol  = '%'
  , tname    = "bush"
  , tfreq    = [ ("bush Lit", 1), ("shootoutSetLit", 30), ("escapeSetLit", 40)
               , ("ambushSetLit", 3), ("bushClumpOver_f_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [ ("battleSetDark", 30), ("zooSetDark", 30), ("ambushSetDark", 3)
               , ("bush with fire", 70) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [ ("ambushSetDark", 15), ("zooSetDark", 300)
               , ("bush with fire", 30) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "small fire" : ChangeTo "bush with fire" : tfeature bush
  }

-- ** Walkable

-- *** Not clear

fog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ ("fog Lit", 1), ("emptySetLit", 50), ("noiseSetLit", 100)
               , ("shootoutSetLit", 20)
               , ("fogClumpOver_f_Lit", 60), ("fogClumpOver_f_Dark", 60) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = [Walkable, NoItem, OftenActor]
  }
fogDark = fog
  { tname    = "thick fog"
  , tfreq    = [ ("emptySetDark", 50), ("powerSetDark", 100)
               , ("escapeSetDark", 50) ]
  , tfeature = Dark : tfeature fog
  }
smoke = TileKind
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ ("smoke Lit", 1), ("labTrailLit", 1), ("stair terminal Lit", 4)
               , ("smokeClumpOver_f_Lit", 3), ("smokeClumpOver_f_Dark", 3) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }
smokeDark = smoke
  { tname    = "lingering smoke"
  , tfreq    = [ ("stair terminal Dark", 4), ("ambushSetDark", 40)
               , ("zooSetDark", 20), ("battleSetDark", 5) ]
  , tfeature = Dark : tfeature smoke
  }

-- *** Clear

doorOpen = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [("open vertical door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "closed vertical door Lit"
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [("open horizontal door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "closed horizontal door Lit"
               ]
  }
floorCorridor = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [("floorCorridorLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]
  }
floorArena = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "stone floor"
  , tfreq    = [ ("floorArenaLit", 1), ("arenaSetLit", 1), ("emptySetLit", 900)
               , ("zooSetLit", 600) ]
  }
floorDamp = floorArena
  { tname    = "damp stone floor"
  , tfreq    = [ ("noiseSetLit", 600), ("powerSetLit", 600)
               , ("damp floor Lit", 1), ("stair terminal Lit", 20) ]
  }
floorDirt = floorArena
  { tname    = "dirt"
  , tfreq    = [ ("shootoutSetLit", 1000), ("escapeSetLit", 1000)
               , ("ambushSetLit", 1000), ("battleSetLit", 1000)
               , ("brawlSetLit", 1000), ("dirt Lit", 1) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [("treeShadeOver_s_Lit", 1), ("bushClumpOver_f_Lit", 1)]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [("floorActorLit", 1)]
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [("legendLit", 100)]
  , tfeature = VeryOftenItem : tfeature floorActor
  }
floorAshes = floorActor
  { tfreq    = [ ("smokeClumpOver_f_Lit", 2), ("smokeClumpOver_f_Dark", 2)
               , ("floorAshesLit", 1), ("floorAshesDark", 1) ]
  , tname    = "dirt and ash pile"
  , tcolor   = Brown
  , tcolor2  = Brown
  }
shallowWater = TileKind
  { tsymbol  = '~'
  , tname    = "water puddle"
  , tfreq    = [ ("shallow water Lit", 1), ("legendLit", 100)
               , ("emptySetLit", 5), ("noiseSetLit", 20)
               , ("powerSetLit", 20), ("shootoutSetLit", 5) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = Embed "shallow water" : tfeature floorActor
  }
shallowWaterSpice = shallowWater
  { tfreq    = [("fogClumpOver_f_Lit", 40)]
  , tfeature = Spice : tfeature shallowWater
  }
floorRed = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "brick pavement"
  , tfreq    = [("trailLit", 70), ("safeTrailLit", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = [Embed "straight path", Trail, Walkable, Clear]
  }
floorBlue = floorRed
  { tname    = "frozen trail"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [Embed "frozen ground", Trail, Walkable, Clear]
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tfreq    = [("trailLit", 70), ("safeTrailLit", 70)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tfreq    = [("trailLit", 50), ("safeTrailLit", 50)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorArenaShade = floorActor
  { tname    = "shaded ground"
  , tfreq    = [("shaded ground", 1), ("treeShadeOver_s_Lit", 2)]
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActor
  }

outdoorFence = TileKind
  { tsymbol  = ' '
  , tname    = "event horizon"
  , tfreq    = [("outdoor outer fence", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }

makeDark :: TileKind -> TileKind
makeDark k = let darkText :: GroupName TileKind -> GroupName TileKind
                 darkText t = maybe t (toGroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ fromGroupName t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (OpenWith grps t) =
                   Just $ OpenWith grps $ darkText t
                 darkFeat (CloseWith grps t) =
                   Just $ CloseWith grps $ darkText t
                 darkFeat (ChangeWith grps t) =
                   Just $ ChangeWith grps $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkText t
                 darkFeat VeryOftenItem = Just OftenItem
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) {tcolor2 = BrBlack}
