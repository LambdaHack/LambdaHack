-- | Terrain tile definitions.
module Content.TileKind
  ( -- * Group name patterns
    pattern UNKNOWN_SPACE, pattern UNKNOWN_OUTER_FENCE, pattern BASIC_OUTER_FENCE, pattern FILLER_WALL, pattern LEGEND_LIT, pattern LEGEND_DARK, pattern WALL_LIT, pattern RECT_WINDOWS_OVER_EXCL_LIT, pattern SUSPECT_VERTICAL_WALL_LIT, pattern OBSCURED_VERTICAL_WALL_LIT, pattern WALL_H_LIT, pattern RECT_WINDOWS_OVER_EQ_LIT, pattern SUSPECT_HORIZONTAL_WALL_LIT, pattern OBSCURED_HORIZONTAL_WALL_LIT, pattern CACHABLE, pattern STAIR_TERMINAL_LIT, pattern STAIR_TERMINAL_DARK, pattern EMPTY_SET_LIT, pattern NOISE_SET_LIT, pattern POWER_SET_DARK, pattern BATTLE_SET_DARK, pattern BRAWL_SET_LIT, pattern SHOOTOUT_SET_LIT, pattern ZOO_SET_DARK, pattern CACHE, pattern LAMP_POST_OVER_0, pattern SIGNBOARD_UNREAD, pattern SIGNBOARD, pattern ESCAPE_SET_DARK, pattern ESCAPE_SET_LIT, pattern AMBUSH_SET_LIT, pattern TREE_SHADOW_OVER_0_LIT, pattern AMBUSH_SET_DARK, pattern TREE_WITH_FIRE, pattern RUBBLE_PILE, pattern EMPTY_SET_DARK, pattern SMOKE_CLUMP_OVER_f_LIT, pattern SMOKE_CLUMP_OVER_f_DARK, pattern TRAPPED_VERTICAL_DOOR_LIT, pattern CLOSED_VERTICAL_DOOR_LIT, pattern TRAPPED_HORIZONAL_DOOR_LIT, pattern CLOSED_HORIZONTAL_DOOR_LIT, pattern STAIRCASE_UP, pattern ORDINARY_STAIRCASE_UP, pattern STAIRCASE_OUTDOOR_UP, pattern GATED_STAIRCASE_UP, pattern STAIRCASE_DOWN, pattern ORDINARY_STAIRCASE_DOWN, pattern STAIRCASE_OUTDOOR_DOWN, pattern GATED_STAIRCASE_DOWN, pattern ESCAPE_UP, pattern ESCAPE_DOWN, pattern ESCAPE_OUTDOOR_DOWN, pattern GLASSHOURSE_OVER_EXCL_LIT, pattern GLASSHOURSE_OVER_EQ_LIT, pattern PULPIT, pattern BUDH_LIT, pattern BUSH_CLUMP_OVER_f_LIT, pattern BUSH_WITH_FIRE, pattern FOG_LIT, pattern FOG_CLUMP_OVER_f_LIT, pattern FOG_CLUMP_OVER_f_DARK, pattern SMOKE_LIT, pattern LAB_TRAIL_LIT, pattern OPEN_VERTICAL_DOOR_LIT, pattern OPEN_HORIZONTAL_DOOR_LIT, pattern FLOOR_CORRIDOR_LIT, pattern FLOOR_ARENA_LIT, pattern ARENA_SET_LIT, pattern POWER_SET_LIT, pattern DAMP_FLOOR_LIT, pattern DIRT_LIT, pattern FLOOR_ACTOR_LIT, pattern FLOOR_ASHES_LIT, pattern FLOOR_ASHES_DARK, pattern SHALLOW_WATER_LIT, pattern TRAIL_LIT, pattern SAFE_TRAIL_LIT, pattern SHADED_GROUND, pattern OUTDOOR_OUTER_FENCE
    -- * Content
  , content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.ItemKindEmbed
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs

-- * Group name patterns

pattern UNKNOWN_SPACE, UNKNOWN_OUTER_FENCE, BASIC_OUTER_FENCE, FILLER_WALL, LEGEND_LIT, LEGEND_DARK, WALL_LIT, RECT_WINDOWS_OVER_EXCL_LIT, SUSPECT_VERTICAL_WALL_LIT, OBSCURED_VERTICAL_WALL_LIT, WALL_H_LIT, RECT_WINDOWS_OVER_EQ_LIT, SUSPECT_HORIZONTAL_WALL_LIT, OBSCURED_HORIZONTAL_WALL_LIT, CACHABLE, STAIR_TERMINAL_LIT, STAIR_TERMINAL_DARK, EMPTY_SET_LIT, NOISE_SET_LIT, POWER_SET_DARK, BATTLE_SET_DARK, BRAWL_SET_LIT, SHOOTOUT_SET_LIT, ZOO_SET_DARK, CACHE, LAMP_POST_OVER_0, SIGNBOARD_UNREAD, SIGNBOARD, ESCAPE_SET_DARK, ESCAPE_SET_LIT, AMBUSH_SET_LIT, TREE_SHADOW_OVER_0_LIT, AMBUSH_SET_DARK, TREE_WITH_FIRE, RUBBLE_PILE, EMPTY_SET_DARK, SMOKE_CLUMP_OVER_f_LIT, SMOKE_CLUMP_OVER_f_DARK, TRAPPED_VERTICAL_DOOR_LIT, CLOSED_VERTICAL_DOOR_LIT, TRAPPED_HORIZONAL_DOOR_LIT, CLOSED_HORIZONTAL_DOOR_LIT, STAIRCASE_UP, ORDINARY_STAIRCASE_UP, STAIRCASE_OUTDOOR_UP, GATED_STAIRCASE_UP, STAIRCASE_DOWN, ORDINARY_STAIRCASE_DOWN, STAIRCASE_OUTDOOR_DOWN, GATED_STAIRCASE_DOWN, ESCAPE_UP, ESCAPE_DOWN, ESCAPE_OUTDOOR_DOWN, GLASSHOURSE_OVER_EXCL_LIT, GLASSHOURSE_OVER_EQ_LIT, PULPIT, BUDH_LIT, BUSH_CLUMP_OVER_f_LIT, BUSH_WITH_FIRE, FOG_LIT, FOG_CLUMP_OVER_f_LIT, FOG_CLUMP_OVER_f_DARK, SMOKE_LIT, LAB_TRAIL_LIT, OPEN_VERTICAL_DOOR_LIT, OPEN_HORIZONTAL_DOOR_LIT, FLOOR_CORRIDOR_LIT, FLOOR_ARENA_LIT, ARENA_SET_LIT, POWER_SET_LIT, DAMP_FLOOR_LIT, DIRT_LIT, FLOOR_ACTOR_LIT, FLOOR_ASHES_LIT, FLOOR_ASHES_DARK, SHALLOW_WATER_LIT, TRAIL_LIT, SAFE_TRAIL_LIT, SHADED_GROUND, OUTDOOR_OUTER_FENCE :: GroupName TileKind

pattern UNKNOWN_SPACE = "unknown space"
pattern UNKNOWN_OUTER_FENCE = "unknown outer fence"
pattern BASIC_OUTER_FENCE = "basic outer fence"
pattern FILLER_WALL = "fillerWall"
pattern LEGEND_LIT = "legendLit"
pattern LEGEND_DARK = "legendDark"
pattern WALL_LIT = "wall Lit"
pattern RECT_WINDOWS_OVER_EXCL_LIT = "rectWindowsOver_!_Lit"
pattern SUSPECT_VERTICAL_WALL_LIT = "suspect vertical wall Lit"
pattern OBSCURED_VERTICAL_WALL_LIT = "obscured vertical wall Lit"
pattern WALL_H_LIT = "wallH Lit"
pattern RECT_WINDOWS_OVER_EQ_LIT = "rectWindowsOver_=_Lit"
pattern SUSPECT_HORIZONTAL_WALL_LIT = "suspect horizontal wall Lit"
pattern OBSCURED_HORIZONTAL_WALL_LIT = "obscured horizontal wall Lit"
pattern CACHABLE = "cachable"
pattern STAIR_TERMINAL_LIT = "stair terminal Lit"
pattern STAIR_TERMINAL_DARK = "stair terminal Dark"
pattern EMPTY_SET_LIT = "emptySetLit"
pattern NOISE_SET_LIT = "noiseSetLit"
pattern POWER_SET_DARK = "powerSetDark"
pattern BATTLE_SET_DARK = "battleSetDark"
pattern BRAWL_SET_LIT = "brawlSetLit"
pattern SHOOTOUT_SET_LIT = "shootoutSetLit"
pattern ZOO_SET_DARK = "zooSetDark"
pattern CACHE = "cache"
pattern LAMP_POST_OVER_0 = "lampPostOver_0"
pattern SIGNBOARD_UNREAD = "signboard unread"
pattern SIGNBOARD = "signboard"
pattern ESCAPE_SET_DARK = "escapeSetDark"
pattern ESCAPE_SET_LIT = "escapeSetLit"
pattern AMBUSH_SET_LIT = "ambushSetLit"
pattern TREE_SHADOW_OVER_0_LIT = "treeShadeOver_0_Lit"
pattern AMBUSH_SET_DARK = "ambushSetDark"
pattern TREE_WITH_FIRE = "tree with fire"
pattern RUBBLE_PILE = "rubble pile"
pattern EMPTY_SET_DARK = "emptySetDark"
pattern SMOKE_CLUMP_OVER_f_LIT = "smokeClumpOver_f_Lit"
pattern SMOKE_CLUMP_OVER_f_DARK = "smokeClumpOver_f_Dark"
pattern TRAPPED_VERTICAL_DOOR_LIT = "trapped vertical door Lit"
pattern CLOSED_VERTICAL_DOOR_LIT = "closed vertical door Lit"
pattern TRAPPED_HORIZONAL_DOOR_LIT = "trapped horizontal door Lit"
pattern CLOSED_HORIZONTAL_DOOR_LIT = "closed horizontal door Lit"
pattern STAIRCASE_UP = "staircase up"
pattern ORDINARY_STAIRCASE_UP = "ordinary staircase up"
pattern STAIRCASE_OUTDOOR_UP = "staircase outdoor up"
pattern GATED_STAIRCASE_UP = "gated staircase up"
pattern STAIRCASE_DOWN = "staircase down"
pattern ORDINARY_STAIRCASE_DOWN = "ordinary staircase down"
pattern STAIRCASE_OUTDOOR_DOWN = "staircase outdoor down"
pattern GATED_STAIRCASE_DOWN = "gated staircase down"
pattern ESCAPE_UP = "escape up"
pattern ESCAPE_DOWN = "escape down"
pattern ESCAPE_OUTDOOR_DOWN = "escape outdoor down"
pattern GLASSHOURSE_OVER_EXCL_LIT = "glasshouseOver_!_Lit"
pattern GLASSHOURSE_OVER_EQ_LIT = "glasshouseOver_=_Lit"
pattern PULPIT = "pulpit"
pattern BUDH_LIT = "bush Lit"
pattern BUSH_CLUMP_OVER_f_LIT = "bushClumpOver_f_Lit"
pattern BUSH_WITH_FIRE = "bush with fire"
pattern FOG_LIT = "fog Lit"
pattern FOG_CLUMP_OVER_f_LIT = "fogClumpOver_f_Lit"
pattern FOG_CLUMP_OVER_f_DARK = "fogClumpOver_f_Dark"
pattern SMOKE_LIT = "smoke Lit"
pattern LAB_TRAIL_LIT = "labTrailLit"
pattern OPEN_VERTICAL_DOOR_LIT = "open vertical door Lit"
pattern OPEN_HORIZONTAL_DOOR_LIT = "open horizontal door Lit"
pattern FLOOR_CORRIDOR_LIT = "floorCorridorLit"
pattern FLOOR_ARENA_LIT = "floorArenaLit"
pattern ARENA_SET_LIT = "arenaSetLit"
pattern POWER_SET_LIT = "powerSetLit"
pattern DAMP_FLOOR_LIT = "damp floor Lit"
pattern DIRT_LIT = "dirt Lit"
pattern FLOOR_ACTOR_LIT = "floorActorLit"
pattern FLOOR_ASHES_LIT = "floorAshesLit"
pattern FLOOR_ASHES_DARK = "floorAshesDark"
pattern SHALLOW_WATER_LIT = "shallow water Lit"
pattern TRAIL_LIT = "trailLit"
pattern SAFE_TRAIL_LIT = "safeTrailLit"
pattern SHADED_GROUND = "shaded ground"
pattern OUTDOOR_OUTER_FENCE = "outdoor outer fence"

-- * Content

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
  , tfreq    = [(UNKNOWN_SPACE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
unknownOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [(UNKNOWN_OUTER_FENCE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
basicOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [(BASIC_OUTER_FENCE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
bedrock = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [(FILLER_WALL, 1), (LEGEND_LIT, 100), (LEGEND_DARK, 100)]
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
  , tfreq    = [ (LEGEND_LIT, 100), (WALL_LIT, 100)
               , (RECT_WINDOWS_OVER_EXCL_LIT, 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs SUSPECT_VERTICAL_WALL_LIT]
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '|'
  , tname    = "suspect uneven wall"
  , tfreq    = [(SUSPECT_VERTICAL_WALL_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs TRAPPED_VERTICAL_DOOR_LIT
               , ObscureAs OBSCURED_VERTICAL_WALL_LIT
               ]
  }
wallObscured = TileKind
  { tsymbol  = '|'
  , tname    = "scratched wall"
  , tfreq    = [(OBSCURED_VERTICAL_WALL_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed SCRATCH_ON_WALL
               , HideAs SUSPECT_VERTICAL_WALL_LIT
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "sandstone wall"
  , tfreq    = [ (LEGEND_LIT, 100), (WALL_H_LIT, 100)
               , (RECT_WINDOWS_OVER_EQ_LIT, 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs SUSPECT_HORIZONTAL_WALL_LIT]
  }
wallSuspectH = TileKind  -- only on client
  { tsymbol  = '-'
  , tname    = "suspect painted wall"
  , tfreq    = [(SUSPECT_HORIZONTAL_WALL_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs TRAPPED_HORIZONAL_DOOR_LIT
               , ObscureAs OBSCURED_HORIZONTAL_WALL_LIT
               ]
  }
wallObscuredDefacedH = TileKind
  { tsymbol  = '-'
  , tname    = "defaced wall"
  , tfreq    = [(OBSCURED_HORIZONTAL_WALL_LIT, 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed OBSCENE_PICTOGRAM
               , HideAs SUSPECT_HORIZONTAL_WALL_LIT
               ]
  }
wallObscuredFrescoedH = TileKind
  { tsymbol  = '-'
  , tname    = "frescoed wall"
  , tfreq    = [(OBSCURED_HORIZONTAL_WALL_LIT, 10)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed SUBTLE_FRESCO
               , HideAs SUSPECT_HORIZONTAL_WALL_LIT
               ]  -- a bit beneficial, but AI would loop if allowed to trigger
                  -- so no @ConsideredByAI@
  }
pillar = TileKind
  { tsymbol  = '0'
  , tname    = "rock"
  , tfreq    = [ (CACHABLE, 70)
               , (STAIR_TERMINAL_LIT, 100), (STAIR_TERMINAL_DARK, 100)
               , (LEGEND_LIT, 100), (LEGEND_DARK, 100)
               , (EMPTY_SET_LIT, 20), (NOISE_SET_LIT, 700)
               , (POWER_SET_DARK, 700)
               , (BATTLE_SET_DARK, 200), (BRAWL_SET_LIT, 50)
               , (SHOOTOUT_SET_LIT, 10), (ZOO_SET_DARK, 10) ]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = '0'
  , tname    = "smoothed rock"
  , tfreq    = [(CACHABLE, 30), (CACHE, 1), (STAIR_TERMINAL_DARK, 4)]
                 -- treasure only in dark staircases
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed TREASURE_CACHE, Embed TREASURE_CACHE_TRAP
               , ChangeTo CACHABLE, ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger, unless wary of traps.
  }
lampPost = TileKind
  { tsymbol  = '0'
  , tname    = "lamp post"
  , tfreq    = [(LAMP_POST_OVER_0, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [(SIGNBOARD_UNREAD, 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ ConsideredByAI  -- changes after use, so safe for AI
               , RevealAs SIGNBOARD  -- to display as hidden
               ]
  }
signboardRead = TileKind
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [(SIGNBOARD, 1), (ESCAPE_SET_DARK, 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed SIGNAGE, HideAs SIGNBOARD_UNREAD]
  }
tree = TileKind
  { tsymbol  = '0'
  , tname    = "tree"
  , tfreq    = [ (BRAWL_SET_LIT, 140), (SHOOTOUT_SET_LIT, 10)
               , (ESCAPE_SET_LIT, 35), (AMBUSH_SET_LIT, 3)
               , (TREE_SHADOW_OVER_0_LIT, 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [ (AMBUSH_SET_DARK, 3), (ZOO_SET_DARK, 7), (BATTLE_SET_DARK, 50)
               , (TREE_WITH_FIRE, 30) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [ (AMBUSH_SET_DARK, 15), (ZOO_SET_DARK, 70)
               , (TREE_WITH_FIRE, 70) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed BIG_FIRE : ChangeTo TREE_WITH_FIRE : tfeature tree
      -- TODO: dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ (RUBBLE_PILE, 1), (LEGEND_LIT, 1), (LEGEND_DARK, 1)
               , (STAIR_TERMINAL_LIT, 4), (STAIR_TERMINAL_DARK, 4)
               , (EMPTY_SET_LIT, 10), (EMPTY_SET_DARK, 10)
               , (NOISE_SET_LIT, 50), (POWER_SET_DARK, 50)
               , (ZOO_SET_DARK, 100), (AMBUSH_SET_DARK, 10) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [OpenTo FLOOR_ASHES_LIT, Embed RUBBLE]
      -- It's not explorable, due to not being walkable nor clear and due
      -- to being a door (@OpenTo@), which is kind of OK, because getting
      -- the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
  }
rubbleSpice = rubble
  { tfreq    = [(SMOKE_CLUMP_OVER_f_LIT, 1), (SMOKE_CLUMP_OVER_f_DARK, 1)]
  , tfeature = Spice : tfeature rubble
  }
doorTrapped = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [(TRAPPED_VERTICAL_DOOR_LIT, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed DOORWAY_TRAP
               , OpenTo OPEN_VERTICAL_DOOR_LIT
               , HideAs SUSPECT_VERTICAL_WALL_LIT
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [(CLOSED_VERTICAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo OPEN_VERTICAL_DOOR_LIT]  -- never hidden
  }
doorTrappedH = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [(TRAPPED_HORIZONAL_DOOR_LIT, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed DOORWAY_TRAP
               , OpenTo OPEN_HORIZONTAL_DOOR_LIT
               , HideAs SUSPECT_HORIZONTAL_WALL_LIT
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [(CLOSED_HORIZONTAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo OPEN_HORIZONTAL_DOOR_LIT]  -- never hidden
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [(STAIRCASE_UP, 9), (ORDINARY_STAIRCASE_UP, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed STAIRS_UP, ConsideredByAI]
  }
stairsTrappedUp = TileKind
  { tsymbol  = '<'
  , tname    = "windy staircase up"
  , tfreq    = [(STAIRCASE_UP, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed STAIRS_UP, Embed STAIRS_TRAP_UP
               , ConsideredByAI, ChangeTo ORDINARY_STAIRCASE_UP ]
                 -- AI uses despite the trap; exploration more important
  }
stairsOutdoorUp = stairsUp
  { tname    = "signpost pointing backward"
  , tfreq    = [(STAIRCASE_OUTDOOR_UP, 1)]
  }
stairsGatedUp = stairsUp
  { tname    = "gated staircase up"
  , tfreq    = [(GATED_STAIRCASE_UP, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [(STAIRCASE_DOWN, 9), (ORDINARY_STAIRCASE_DOWN, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed STAIRS_DOWN, ConsideredByAI]
  }
stairsTrappedDown = TileKind
  { tsymbol  = '>'
  , tname    = "crooked staircase down"
  , tfreq    = [(STAIRCASE_DOWN, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed STAIRS_DOWN, Embed STAIRS_TRAP_DOWN
               , ConsideredByAI, ChangeTo ORDINARY_STAIRCASE_DOWN ]
  }
stairsOutdoorDown = stairsDown
  { tname    = "signpost pointing forward"
  , tfreq    = [(STAIRCASE_OUTDOOR_DOWN, 1)]
  }
stairsGatedDown = stairsDown
  { tname    = "gated staircase down"
  , tfreq    = [(GATED_STAIRCASE_DOWN, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [(LEGEND_LIT, 1), (LEGEND_DARK, 1), (ESCAPE_UP, 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed ESCAPE, ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [(LEGEND_LIT, 1), (LEGEND_DARK, 1), (ESCAPE_DOWN, 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed ESCAPE, ConsideredByAI]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [(ESCAPE_OUTDOOR_DOWN, 1)]
  }

-- *** Clear

wallGlass = TileKind
  { tsymbol  = '|'
  , tname    = "polished crystal wall"
  , tfreq    = [(GLASSHOURSE_OVER_EXCL_LIT, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs CLOSED_VERTICAL_DOOR_LIT, Clear]
  }
wallGlassSpice = wallGlass
  { tfreq    = [(RECT_WINDOWS_OVER_EXCL_LIT, 20)]
  , tfeature = Spice : tfeature wallGlass
  }
wallGlassH = TileKind
  { tsymbol  = '-'
  , tname    = "polished crystal wall"
  , tfreq    = [(GLASSHOURSE_OVER_EQ_LIT, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs CLOSED_HORIZONTAL_DOOR_LIT, Clear]
  }
wallGlassHSpice = wallGlassH
  { tfreq    = [(RECT_WINDOWS_OVER_EQ_LIT, 20)]
  , tfeature = Spice : tfeature wallGlassH
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "icy outcrop"
  , tfreq    = [(POWER_SET_DARK, 300)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [Clear, Embed FROST, OpenTo SHALLOW_WATER_LIT]
      -- Is door, due to @OpenTo@, so is not explorable, but it's OK, because
      -- it doesn't generate items nor clues. This saves on the need to
      -- get each ice pillar into sight range when exploring level.
  }
pulpit = TileKind
  { tsymbol  = '%'
  , tname    = "pulpit"
  , tfreq    = [(PULPIT, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [Clear, Embed LECTERN]
                 -- mixed blessing, so AI ignores, saved for player fun
  }
bush = TileKind
  { tsymbol  = '%'
  , tname    = "bush"
  , tfreq    = [ (BUDH_LIT, 1), (SHOOTOUT_SET_LIT, 30), (ESCAPE_SET_LIT, 40)
               , (AMBUSH_SET_LIT, 3), (BUSH_CLUMP_OVER_f_LIT, 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [ (BATTLE_SET_DARK, 30), (ZOO_SET_DARK, 30), (AMBUSH_SET_DARK, 3)
               , (BUSH_WITH_FIRE, 70) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [ (AMBUSH_SET_DARK, 15), (ZOO_SET_DARK, 300)
               , (BUSH_WITH_FIRE, 30) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed SMALL_FIRE : ChangeTo BUSH_WITH_FIRE : tfeature bush
  }

-- ** Walkable

-- *** Not clear

fog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ (FOG_LIT, 1), (EMPTY_SET_LIT, 50), (NOISE_SET_LIT, 100)
               , (SHOOTOUT_SET_LIT, 20)
               , (FOG_CLUMP_OVER_f_LIT, 60), (FOG_CLUMP_OVER_f_DARK, 60) ]
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
  , tfreq    = [ (EMPTY_SET_DARK, 50), (POWER_SET_DARK, 100)
               , (ESCAPE_SET_DARK, 50) ]
  , tfeature = Dark : tfeature fog
  }
smoke = TileKind
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ (SMOKE_LIT, 1), (LAB_TRAIL_LIT, 1), (STAIR_TERMINAL_LIT, 4)
               , (SMOKE_CLUMP_OVER_f_LIT, 3), (SMOKE_CLUMP_OVER_f_DARK, 3) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }
smokeDark = smoke
  { tname    = "lingering smoke"
  , tfreq    = [ (STAIR_TERMINAL_DARK, 4), (AMBUSH_SET_DARK, 40)
               , (ZOO_SET_DARK, 20), (BATTLE_SET_DARK, 5) ]
  , tfeature = Dark : tfeature smoke
  }

-- *** Clear

doorOpen = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [(OPEN_VERTICAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo CLOSED_VERTICAL_DOOR_LIT
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [(OPEN_HORIZONTAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo CLOSED_HORIZONTAL_DOOR_LIT
               ]
  }
floorCorridor = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [(FLOOR_CORRIDOR_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]
  }
floorArena = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "stone floor"
  , tfreq    = [ (FLOOR_ARENA_LIT, 1), (ARENA_SET_LIT, 1), (EMPTY_SET_LIT, 900)
               , ("zooSetLit", 600) ]
  }
floorDamp = floorArena
  { tname    = "damp stone floor"
  , tfreq    = [ (NOISE_SET_LIT, 600), (POWER_SET_LIT, 600)
               , (DAMP_FLOOR_LIT, 1), (STAIR_TERMINAL_LIT, 20) ]
  }
floorDirt = floorArena
  { tname    = "dirt"
  , tfreq    = [ (SHOOTOUT_SET_LIT, 1000), (ESCAPE_SET_LIT, 1000)
               , (AMBUSH_SET_LIT, 1000), ("battleSetLit", 1000)
               , (BRAWL_SET_LIT, 1000), (DIRT_LIT, 1) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [("treeShadeOver_s_Lit", 1), (BUSH_CLUMP_OVER_f_LIT, 1)]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [(FLOOR_ACTOR_LIT, 1)]
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [(LEGEND_LIT, 100)]
  , tfeature = VeryOftenItem : tfeature floorActor
  }
floorAshes = floorActor
  { tfreq    = [ (SMOKE_CLUMP_OVER_f_LIT, 2), (SMOKE_CLUMP_OVER_f_DARK, 2)
               , (FLOOR_ASHES_LIT, 1), (FLOOR_ASHES_DARK, 1) ]
  , tname    = "dirt and ash pile"
  , tcolor   = Brown
  , tcolor2  = Brown
  }
shallowWater = TileKind
  { tsymbol  = '~'
  , tname    = "water puddle"
  , tfreq    = [ (SHALLOW_WATER_LIT, 1), (LEGEND_LIT, 100)
               , (EMPTY_SET_LIT, 5), (NOISE_SET_LIT, 20)
               , (POWER_SET_LIT, 20), (SHOOTOUT_SET_LIT, 5) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = Embed SHALLOW_WATER : tfeature floorActor
  }
shallowWaterSpice = shallowWater
  { tfreq    = [(FOG_CLUMP_OVER_f_LIT, 40)]
  , tfeature = Spice : tfeature shallowWater
  }
floorRed = floorCorridor
  { tsymbol  = floorSymbol
  , tname    = "brick pavement"
  , tfreq    = [(TRAIL_LIT, 70), (SAFE_TRAIL_LIT, 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = [Embed STRAIGHT_PATH, Trail, Walkable, Clear]
  }
floorBlue = floorRed
  { tname    = "frozen trail"
  , tfreq    = [(TRAIL_LIT, 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [Embed FROZEN_GROUND, Trail, Walkable, Clear]
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tfreq    = [(TRAIL_LIT, 70), (SAFE_TRAIL_LIT, 70)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tfreq    = [(TRAIL_LIT, 50), (SAFE_TRAIL_LIT, 50)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorArenaShade = floorActor
  { tname    = "shaded ground"
  , tfreq    = [(SHADED_GROUND, 1), ("treeShadeOver_s_Lit", 2)]
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActor
  }

outdoorFence = TileKind
  { tsymbol  = ' '
  , tname    = "event horizon"
  , tfreq    = [(OUTDOOR_OUTER_FENCE, 1)]
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
