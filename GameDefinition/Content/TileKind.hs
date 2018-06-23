-- | Terrain tile definitions.
module Content.TileKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.TileKind

content :: [TileKind]
content =
  [unknown, unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, doorTrappedH, doorClosedH, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, wallGlassH, wallGlassHSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, floorFog, floorFogDark, floorSmoke, floorSmokeDark, doorOpen, doorOpenH, floorCorridor, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade, outdoorFence ]
  ++ map makeDark ldarkable
  ++ map makeDarkColor ldarkColorable

unknown,    unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, doorTrappedH, doorClosedH, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, wallGlassH, wallGlassHSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, floorFog, floorFogDark, floorSmoke, floorSmokeDark, doorOpen, doorOpenH, floorCorridor, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade, outdoorFence :: TileKind

ldarkable :: [TileKind]
ldarkable = [wall, wallSuspect, wallObscured, wallH, wallSuspectH, wallObscuredDefacedH, wallObscuredFrescoedH, doorTrapped, doorClosed, doorTrappedH, doorClosedH, wallGlass, wallGlassH, doorOpen, doorOpenH, floorCorridor]

ldarkColorable :: [TileKind]
ldarkColorable = [tree, bush, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem]

-- Symbols to be used (the Nethack visual tradition imposes inconsistency):
--         LOS    noLOS
-- Walk    .|-#   :;
-- noWalk  %^-|   -| O&<>+
--
-- can be opened ^&+
-- can be closed |-
-- some noWalk can be changed without opening, regardless of symbol
-- not used yet:
-- ~ (water, acid, ect.)
-- : (curtain, etc., not flowing, but solid and static)
-- `' (not visible enough, would need font modification)

-- Note that for AI hints and UI comfort, most multiple-use @Embed@ tiles
-- should have a variant, which after first use transforms into a different
-- colour tile without @ChangeTo@ and similar (which then AI no longer touches).
-- If a tile is supposed to be repeatedly activated by AI (e.g., cache),
-- it should keep @ChangeTo@ for the whole time.

-- * Main tiles, in other games modified and some removed

-- ** Not walkable

-- *** Not clear

unknown = TileKind  -- needs to have index 0 and alter 1
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
  , tfreq    = [("basic outer fence", 1), ("noise fence", 1)]
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
  , tfreq    = [("legendLit", 100), ("rectWindowsOver_!_Lit", 80)]
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
  , tfreq    = [("legendLit", 100), ("rectWindowsOver_=_Lit", 80)]
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
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("cachable", 70), ("stair terminal", 100)
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 70), ("battleSet", 250), ("brawlSetLit", 50)
               , ("shootoutSetLit", 10), ("zooSet", 10) ]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = 'O'
  , tname    = "smoothed rock"
  , tfreq    = [("cachable", 30), ("cache", 1), ("stair terminal", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed "treasure cache", Embed "treasure cache trap"
               , ChangeTo "cachable", ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger, unless wary of traps.
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_O", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = 'O'
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
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard", 1), ("escapeSetDark", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed "signboard", HideAs "signboard unread"]
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [ ("brawlSetLit", 140), ("shootoutSetLit", 10)
               , ("escapeSetLit", 35), ("treeShadeOver_O_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [("ambushSet", 3), ("zooSet", 7), ("tree with fire", 30)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [("ambushSet", 30), ("zooSet", 70), ("tree with fire", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "big fire" : ChangeTo "tree with fire" : tfeature tree
      -- dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = []  -- [("floorCorridorLit", 1)]
                   -- disabled while it's all or nothing per cave and per room;
                   -- we need a new mechanism, Spice is not enough, because
                   -- we don't want multicolor trailLit corridors
      -- ("rubbleOrNot", 70)
      -- until we can sync change of tile and activation, it always takes 1 turn
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [OpenTo "rubbleOrNot", Embed "rubble"]
  }
rubbleSpice = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ ("smokeClumpOver_f_Lit", 1), ("emptySet", 1), ("noiseSet", 5)
               , ("zooSet", 100), ("ambushSet", 20) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [Spice, OpenTo "rubbleSpiceOrNot", Embed "rubble"]
      -- It's not explorable, due to not being walkable nor clear and due
      -- to being a door (@OpenTo@), which is kind of OK, because getting
      -- the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
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
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
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
  , tfeature = [BuildAs "suspect vertical wall Lit", Clear]
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
  , tfeature = [BuildAs "suspect horizontal wall Lit", Clear]
  }
wallGlassHSpice = wallGlassH
  { tfreq    = [("rectWindowsOver_=_Lit", 20)]
  , tfeature = Spice : tfeature wallGlassH
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "ice"
  , tfreq    = [("noiseSet", 30)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [Clear, Embed "frost", OpenTo "damp stone floor"]
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
               , ("bushClumpOver_f_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [ ("battleSet", 30), ("ambushSet", 4), ("zooSet", 30)
               , ("bush with fire", 70) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [("ambushSet", 40), ("zooSet", 300), ("bush with fire", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "small fire" : ChangeTo "bush with fire" : tfeature bush
  }

-- ** Walkable

-- *** Not clear

floorFog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ ("lit fog", 1), ("emptySet", 5), ("shootoutSetLit", 20)
               , ("noiseSet", 10), ("fogClumpOver_f_Lit", 60) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = [Walkable, NoItem, OftenActor]
  }
floorFogDark = floorFog
  { tname    = "thick fog"
  , tfreq    = [("noiseSet", 10), ("escapeSetDark", 50)]
  , tfeature = Dark : tfeature floorFog
  }
floorSmoke = TileKind
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ ("lit smoke", 1), ("labTrailLit", 1), ("stair terminal", 2)
               , ("smokeClumpOver_f_Lit", 1) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }
floorSmokeDark = floorSmoke
  { tname    = "lingering smoke"
  , tfreq    = [("ambushSet", 60), ("zooSet", 20), ("battleSet", 5)]
  , tfeature = Dark : tfeature floorSmoke
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
  , tfreq    = [("floorCorridorLit", 99), ("rubbleOrNot", 30)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]
  }
floorArena = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "stone floor"
  , tfreq    = [ ("floorArenaLit", 1), ("rubbleSpiceOrNot", 30)
               , ("arenaSetLit", 1), ("emptySet", 97), ("zooSet", 600) ]
  }
floorNoise = floorArena
  { tname    = "damp stone floor"
  , tfreq    = [("noiseSet", 60), ("damp stone floor", 1)]
  }
floorDirt = floorArena
  { tname    = "dirt"
  , tfreq    = [ ("battleSet", 1000), ("brawlSetLit", 1000)
               , ("shootoutSetLit", 1000), ("escapeSetLit", 1000)
               , ("ambushSet", 1000) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [ ("treeShadeOver_s_Lit", 1), ("fogClumpOver_f_Lit", 40)
               , ("smokeClumpOver_f_Lit", 1), ("bushClumpOver_f_Lit", 1) ]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [("floorActorLit", 1)]  -- lit even in dark cave, so no items
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [("legendLit", 100)]
  , tfeature = OftenItem : tfeature floorActor
  }
floorRed = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "brick pavement"
  , tfreq    = [("trailLit", 30), ("alarmingTrailLit", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = [Trail, Walkable, Clear]
  }
floorBlue = floorRed
  { tname    = "cobblestone path"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tfreq    = [("trailLit", 10), ("alarmingTrailLit", 30)]
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
                              $ T.stripSuffix "Lit" $ tshow t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkText t
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) {tcolor2 = BrBlack}
