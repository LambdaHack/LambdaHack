-- | Definitions of tile kinds. Every terrain tile in the game is
-- an instantiated tile kind.
module Content.TileKind
  ( -- * Group name patterns
    -- ** Used in CaveKind and perhaps elsewhere.
    pattern FILLER_WALL, pattern FLOOR_CORRIDOR_LIT, pattern FLOOR_CORRIDOR_DARK, pattern TRAIL_LIT, pattern SAFE_TRAIL_LIT, pattern LAB_TRAIL_LIT, pattern DAMP_FLOOR_LIT, pattern DAMP_FLOOR_DARK, pattern OUTDOOR_OUTER_FENCE, pattern DIRT_LIT, pattern DIRT_DARK, pattern FLOOR_ARENA_LIT, pattern FLOOR_ARENA_DARK
  , pattern EMPTY_SET_LIT, pattern EMPTY_SET_DARK, pattern NOISE_SET_LIT, pattern POWER_SET_LIT, pattern POWER_SET_DARK, pattern BATTLE_SET_LIT, pattern BATTLE_SET_DARK, pattern BRAWL_SET_LIT, pattern SHOOTOUT_SET_LIT, pattern ZOO_SET_LIT, pattern ZOO_SET_DARK, pattern ESCAPE_SET_LIT, pattern ESCAPE_SET_DARK, pattern AMBUSH_SET_LIT, pattern AMBUSH_SET_DARK, pattern ARENA_SET_LIT, pattern ARENA_SET_DARK
    -- ** Used in PlaceKind, but not in CaveKind.
  , pattern RECT_WINDOWS_VERTICAL_LIT, pattern RECT_WINDOWS_VERTICAL_DARK, pattern RECT_WINDOWS_HORIZONTAL_LIT, pattern RECT_WINDOWS_HORIZONTAL_DARK, pattern TREE_SHADE_WALKABLE_LIT, pattern TREE_SHADE_WALKABLE_DARK, pattern SMOKE_CLUMP_LIT, pattern SMOKE_CLUMP_DARK, pattern GLASSHOUSE_VERTICAL_LIT, pattern GLASSHOUSE_VERTICAL_DARK, pattern GLASSHOUSE_HORIZONTAL_LIT, pattern GLASSHOUSE_HORIZONTAL_DARK, pattern BUSH_CLUMP_LIT, pattern BUSH_CLUMP_DARK, pattern FOG_CLUMP_LIT, pattern FOG_CLUMP_DARK, pattern STAIR_TERMINAL_LIT, pattern STAIR_TERMINAL_DARK, pattern CACHE, pattern SIGNBOARD, pattern STAIRCASE_UP, pattern ORDINARY_STAIRCASE_UP, pattern STAIRCASE_OUTDOOR_UP, pattern GATED_STAIRCASE_UP, pattern STAIRCASE_DOWN, pattern ORDINARY_STAIRCASE_DOWN, pattern STAIRCASE_OUTDOOR_DOWN, pattern GATED_STAIRCASE_DOWN, pattern ESCAPE_UP, pattern ESCAPE_DOWN, pattern ESCAPE_OUTDOOR_DOWN
  , pattern S_LAMP_POST, pattern S_TREE_LIT, pattern S_TREE_DARK, pattern S_WALL_LIT, pattern S_WALL_HORIZONTAL_LIT, pattern S_PULPIT, pattern S_BUSH_LIT, pattern S_FOG_LIT, pattern S_SMOKE_LIT, pattern S_FLOOR_ACTOR_LIT, pattern S_FLOOR_ACTOR_DARK, pattern S_FLOOR_ASHES_LIT, pattern S_FLOOR_ASHES_DARK, pattern S_SHADED_GROUND
  , groupNamesSingleton, groupNames
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

-- Warning, many of these are also sythesized, so typos can happen.

groupNamesSingleton :: [GroupName TileKind]
groupNamesSingleton =
       [S_LAMP_POST, S_TREE_LIT, S_TREE_DARK, S_WALL_LIT, S_WALL_HORIZONTAL_LIT, S_PULPIT, S_BUSH_LIT, S_FOG_LIT, S_SMOKE_LIT, S_FLOOR_ACTOR_LIT, S_FLOOR_ACTOR_DARK, S_FLOOR_ASHES_LIT, S_FLOOR_ASHES_DARK, S_SHADED_GROUND]
    ++ [S_SUSPECT_VERTICAL_WALL_LIT, S_SUSPECT_HORIZONTAL_WALL_LIT, S_CLOSED_VERTICAL_DOOR_LIT, S_CLOSED_HORIZONTAL_DOOR_LIT, S_OPEN_VERTICAL_DOOR_LIT, S_OPEN_HORIZONTAL_DOOR_LIT, S_RUBBLE_PILE, S_SHALLOW_WATER_LIT, S_SIGNBOARD_UNREAD]
    ++ [S_BUSH_DARK, S_CLOSED_HORIZONTAL_DOOR_DARK, S_CLOSED_VERTICAL_DOOR_DARK, S_OPEN_HORIZONTAL_DOOR_DARK, S_OPEN_VERTICAL_DOOR_DARK, S_SHALLOW_WATER_DARK, S_SUSPECT_HORIZONTAL_WALL_DARK, S_SUSPECT_VERTICAL_WALL_DARK, S_WALL_DARK, S_WALL_HORIZONTAL_DARK]

-- ** Used in PlaceKind, but not in CaveKind.
pattern S_LAMP_POST, S_TREE_LIT, S_TREE_DARK, S_WALL_LIT, S_WALL_HORIZONTAL_LIT, S_PULPIT, S_BUSH_LIT, S_FOG_LIT, S_SMOKE_LIT, S_FLOOR_ACTOR_LIT, S_FLOOR_ACTOR_DARK, S_FLOOR_ASHES_LIT, S_FLOOR_ASHES_DARK, S_SHADED_GROUND :: GroupName TileKind

-- ** Used only internally in other TileKind definitions or never used.
pattern S_SUSPECT_VERTICAL_WALL_LIT, S_SUSPECT_HORIZONTAL_WALL_LIT, S_CLOSED_VERTICAL_DOOR_LIT, S_CLOSED_HORIZONTAL_DOOR_LIT, S_OPEN_VERTICAL_DOOR_LIT, S_OPEN_HORIZONTAL_DOOR_LIT, S_RUBBLE_PILE, S_SHALLOW_WATER_LIT, S_SIGNBOARD_UNREAD :: GroupName TileKind

-- * Not used, but needed, because auto-generated. Singletons.
pattern S_BUSH_DARK, S_CLOSED_HORIZONTAL_DOOR_DARK, S_CLOSED_VERTICAL_DOOR_DARK, S_OPEN_HORIZONTAL_DOOR_DARK, S_OPEN_VERTICAL_DOOR_DARK, S_SHALLOW_WATER_DARK, S_SUSPECT_HORIZONTAL_WALL_DARK, S_SUSPECT_VERTICAL_WALL_DARK, S_WALL_DARK, S_WALL_HORIZONTAL_DARK :: GroupName TileKind

-- TODO: if we stick to the current system of generating extra kinds and their
-- group names, let's also add the generated group names to @groupNames@.
groupNames :: [GroupName TileKind]
groupNames =
       [FILLER_WALL, FLOOR_CORRIDOR_LIT, FLOOR_CORRIDOR_DARK, TRAIL_LIT, SAFE_TRAIL_LIT, LAB_TRAIL_LIT, DAMP_FLOOR_LIT, DAMP_FLOOR_DARK, OUTDOOR_OUTER_FENCE, DIRT_LIT, DIRT_DARK, FLOOR_ARENA_LIT, FLOOR_ARENA_DARK]
    ++ [EMPTY_SET_LIT, EMPTY_SET_DARK, NOISE_SET_LIT, POWER_SET_LIT, POWER_SET_DARK, BATTLE_SET_LIT, BATTLE_SET_DARK, BRAWL_SET_LIT, SHOOTOUT_SET_LIT, ZOO_SET_LIT, ZOO_SET_DARK, ESCAPE_SET_LIT, ESCAPE_SET_DARK, AMBUSH_SET_LIT, AMBUSH_SET_DARK, ARENA_SET_LIT, ARENA_SET_DARK]
    ++ [RECT_WINDOWS_VERTICAL_LIT, RECT_WINDOWS_VERTICAL_DARK, RECT_WINDOWS_HORIZONTAL_LIT, RECT_WINDOWS_HORIZONTAL_DARK, TREE_SHADE_WALKABLE_LIT, TREE_SHADE_WALKABLE_DARK, SMOKE_CLUMP_LIT, SMOKE_CLUMP_DARK, GLASSHOUSE_VERTICAL_LIT, GLASSHOUSE_VERTICAL_DARK, GLASSHOUSE_HORIZONTAL_LIT, GLASSHOUSE_HORIZONTAL_DARK, BUSH_CLUMP_LIT, BUSH_CLUMP_DARK, FOG_CLUMP_LIT, FOG_CLUMP_DARK, STAIR_TERMINAL_LIT, STAIR_TERMINAL_DARK, CACHE, SIGNBOARD, STAIRCASE_UP, ORDINARY_STAIRCASE_UP, STAIRCASE_OUTDOOR_UP, GATED_STAIRCASE_UP, STAIRCASE_DOWN, ORDINARY_STAIRCASE_DOWN, STAIRCASE_OUTDOOR_DOWN, GATED_STAIRCASE_DOWN, ESCAPE_UP, ESCAPE_DOWN, ESCAPE_OUTDOOR_DOWN]
    ++ [OBSCURED_VERTICAL_WALL_LIT, OBSCURED_HORIZONTAL_WALL_LIT, TRAPPED_VERTICAL_DOOR_LIT, TRAPPED_HORIZONAL_DOOR_LIT, TREE_BURNING_OR_NOT, BUSH_BURNING_OR_NOT, CACHE_OR_NOT]
    ++ [BRAWL_SET_DARK, NOISE_SET_DARK, OBSCURED_HORIZONTAL_WALL_DARK, OBSCURED_VERTICAL_WALL_DARK, SHOOTOUT_SET_DARK, TRAPPED_HORIZONAL_DOOR_DARK, TRAPPED_VERTICAL_DOOR_DARK]

pattern FILLER_WALL, FLOOR_CORRIDOR_LIT, FLOOR_CORRIDOR_DARK, TRAIL_LIT, SAFE_TRAIL_LIT, LAB_TRAIL_LIT, DAMP_FLOOR_LIT, DAMP_FLOOR_DARK, OUTDOOR_OUTER_FENCE, DIRT_LIT, DIRT_DARK, FLOOR_ARENA_LIT, FLOOR_ARENA_DARK :: GroupName TileKind

pattern EMPTY_SET_LIT, EMPTY_SET_DARK, NOISE_SET_LIT, POWER_SET_LIT, POWER_SET_DARK, BATTLE_SET_LIT, BATTLE_SET_DARK, BRAWL_SET_LIT, SHOOTOUT_SET_LIT, ZOO_SET_LIT, ZOO_SET_DARK, ESCAPE_SET_LIT, ESCAPE_SET_DARK, AMBUSH_SET_LIT, AMBUSH_SET_DARK, ARENA_SET_LIT, ARENA_SET_DARK :: GroupName TileKind

-- ** Used in PlaceKind, but not in CaveKind.
pattern RECT_WINDOWS_VERTICAL_LIT, RECT_WINDOWS_VERTICAL_DARK, RECT_WINDOWS_HORIZONTAL_LIT, RECT_WINDOWS_HORIZONTAL_DARK, TREE_SHADE_WALKABLE_LIT, TREE_SHADE_WALKABLE_DARK, SMOKE_CLUMP_LIT, SMOKE_CLUMP_DARK, GLASSHOUSE_VERTICAL_LIT, GLASSHOUSE_VERTICAL_DARK, GLASSHOUSE_HORIZONTAL_LIT, GLASSHOUSE_HORIZONTAL_DARK, BUSH_CLUMP_LIT, BUSH_CLUMP_DARK, FOG_CLUMP_LIT, FOG_CLUMP_DARK, STAIR_TERMINAL_LIT, STAIR_TERMINAL_DARK, CACHE, SIGNBOARD, STAIRCASE_UP, ORDINARY_STAIRCASE_UP, STAIRCASE_OUTDOOR_UP, GATED_STAIRCASE_UP, STAIRCASE_DOWN, ORDINARY_STAIRCASE_DOWN, STAIRCASE_OUTDOOR_DOWN, GATED_STAIRCASE_DOWN, ESCAPE_UP, ESCAPE_DOWN, ESCAPE_OUTDOOR_DOWN :: GroupName TileKind

-- ** Used only internally in other TileKind definitions or never used.
pattern OBSCURED_VERTICAL_WALL_LIT, OBSCURED_HORIZONTAL_WALL_LIT, TRAPPED_VERTICAL_DOOR_LIT, TRAPPED_HORIZONAL_DOOR_LIT, TREE_BURNING_OR_NOT, BUSH_BURNING_OR_NOT, CACHE_OR_NOT :: GroupName TileKind

-- * Not used, but needed, because auto-generated. Not singletons.
pattern BRAWL_SET_DARK, NOISE_SET_DARK, OBSCURED_HORIZONTAL_WALL_DARK, OBSCURED_VERTICAL_WALL_DARK, SHOOTOUT_SET_DARK, TRAPPED_HORIZONAL_DOOR_DARK, TRAPPED_VERTICAL_DOOR_DARK :: GroupName TileKind

-- ** Used in CaveKind and perhaps elsewhere (or a dark/lit version thereof).
pattern FILLER_WALL = GroupName "fillerWall"
pattern FLOOR_CORRIDOR_LIT = GroupName "floorCorridorLit"
pattern FLOOR_CORRIDOR_DARK = GroupName "floorCorridorDark"
pattern TRAIL_LIT = GroupName "trailLit"
pattern SAFE_TRAIL_LIT = GroupName "safeTrailLit"
pattern LAB_TRAIL_LIT = GroupName "labTrailLit"
  -- these three would work without @_LIT@, but it will be needed when
  -- in the future a lit trail is made from terrain that has an autogenerated
  -- dark variant
pattern DAMP_FLOOR_LIT = GroupName "damp floor Lit"
pattern DAMP_FLOOR_DARK = GroupName "damp floor Dark"
pattern OUTDOOR_OUTER_FENCE = GroupName "outdoor outer fence"
pattern DIRT_LIT = GroupName "dirt Lit"
pattern DIRT_DARK = GroupName "dirt Dark"
pattern FLOOR_ARENA_LIT = GroupName "floorArenaLit"
pattern FLOOR_ARENA_DARK = GroupName "floorArenaDark"

-- ** Used in CaveKind and perhaps elsewhere; sets of tiles for filling cave.
pattern EMPTY_SET_LIT = GroupName "emptySetLit"
pattern EMPTY_SET_DARK = GroupName "emptySetDark"
pattern NOISE_SET_LIT = GroupName "noiseSetLit"
pattern POWER_SET_LIT = GroupName "powerSetLit"
pattern POWER_SET_DARK = GroupName "powerSetDark"
pattern BATTLE_SET_LIT = GroupName "battleSetLit"
pattern BATTLE_SET_DARK = GroupName "battleSetDark"
pattern BRAWL_SET_LIT = GroupName "brawlSetLit"
pattern SHOOTOUT_SET_LIT = GroupName "shootoutSetLit"
pattern ZOO_SET_LIT = GroupName "zooSetLit"
pattern ZOO_SET_DARK = GroupName "zooSetDark"
pattern ESCAPE_SET_LIT = GroupName "escapeSetLit"
pattern ESCAPE_SET_DARK = GroupName "escapeSetDark"
pattern AMBUSH_SET_LIT = GroupName "ambushSetLit"
pattern AMBUSH_SET_DARK = GroupName "ambushSetDark"
pattern ARENA_SET_LIT = GroupName "arenaSetLit"
pattern ARENA_SET_DARK = GroupName "arenaSetDark"

-- ** Used in PlaceKind, but not in CaveKind. Not singletons.
pattern RECT_WINDOWS_VERTICAL_LIT = GroupName "rectWindowsVerticalLit"
pattern RECT_WINDOWS_VERTICAL_DARK = GroupName "rectWindowsVerticalDark"
pattern RECT_WINDOWS_HORIZONTAL_LIT = GroupName "rectWindowsHorizontalLit"
pattern RECT_WINDOWS_HORIZONTAL_DARK = GroupName "rectWindowsHorizontalDark"
pattern TREE_SHADE_WALKABLE_LIT = GroupName "treeShadeWalkableLit"
pattern TREE_SHADE_WALKABLE_DARK = GroupName "treeShadeWalkableDark"
pattern SMOKE_CLUMP_LIT = GroupName "smokeClumpLit"
pattern SMOKE_CLUMP_DARK = GroupName "smokeClumpDark"
pattern GLASSHOUSE_VERTICAL_LIT = GroupName "glasshouseVerticalLit"
pattern GLASSHOUSE_VERTICAL_DARK = GroupName "glasshouseVerticalDark"
pattern GLASSHOUSE_HORIZONTAL_LIT = GroupName "glasshouseHorizontalLit"
pattern GLASSHOUSE_HORIZONTAL_DARK = GroupName "glasshouseHorizontalDark"
pattern BUSH_CLUMP_LIT = GroupName "bushClumpLit"
pattern BUSH_CLUMP_DARK = GroupName "bushClumpDark"
pattern FOG_CLUMP_LIT = GroupName "fogClumpLit"
pattern FOG_CLUMP_DARK = GroupName "fogClumpDark"
pattern STAIR_TERMINAL_LIT = GroupName "stair terminal Lit"
pattern STAIR_TERMINAL_DARK = GroupName "stair terminal Dark"
pattern CACHE = GroupName "cache"
pattern SIGNBOARD = GroupName "signboard"
pattern STAIRCASE_UP = GroupName "staircase up"
pattern ORDINARY_STAIRCASE_UP = GroupName "ordinary staircase up"
pattern STAIRCASE_OUTDOOR_UP = GroupName "staircase outdoor up"
pattern GATED_STAIRCASE_UP = GroupName "gated staircase up"
pattern STAIRCASE_DOWN = GroupName "staircase down"
pattern ORDINARY_STAIRCASE_DOWN = GroupName "ordinary staircase down"
pattern STAIRCASE_OUTDOOR_DOWN = GroupName "staircase outdoor down"
pattern GATED_STAIRCASE_DOWN = GroupName "gated staircase down"
pattern ESCAPE_UP = GroupName "escape up"
pattern ESCAPE_DOWN = GroupName "escape down"
pattern ESCAPE_OUTDOOR_DOWN = GroupName "escape outdoor down"

-- ** Used in PlaceKind, but not in CaveKind. Singletons.
pattern S_LAMP_POST = GroupName "lamp post"
pattern S_TREE_LIT = GroupName "tree Lit"
pattern S_TREE_DARK = GroupName "tree Dark"
pattern S_WALL_LIT = GroupName "wall Lit"
pattern S_WALL_HORIZONTAL_LIT = GroupName "wall horizontal Lit"
pattern S_PULPIT = GroupName "pulpit"
pattern S_BUSH_LIT = GroupName "bush Lit"
pattern S_FOG_LIT = GroupName "fog Lit"
pattern S_SMOKE_LIT = GroupName "smoke Lit"
pattern S_FLOOR_ACTOR_LIT = GroupName "floor with actors Lit"
pattern S_FLOOR_ACTOR_DARK = GroupName "floor with actors Dark"
pattern S_FLOOR_ASHES_LIT = GroupName "floor with ashes Lit"
pattern S_FLOOR_ASHES_DARK = GroupName "floor with ashes Dark"
pattern S_SHADED_GROUND = GroupName "shaded ground"

-- ** Used only internally in other TileKind definitions. Not singletons.
pattern OBSCURED_VERTICAL_WALL_LIT = GroupName "obscured vertical wall Lit"
pattern OBSCURED_HORIZONTAL_WALL_LIT = GroupName "obscured horizontal wall Lit"
pattern TRAPPED_VERTICAL_DOOR_LIT = GroupName "trapped vertical door Lit"
pattern TRAPPED_HORIZONAL_DOOR_LIT = GroupName "trapped horizontal door Lit"
pattern TREE_BURNING_OR_NOT = GroupName "tree burning or not"
pattern BUSH_BURNING_OR_NOT = GroupName "bush burning or not"
pattern CACHE_OR_NOT = GroupName "cache or not"

-- ** Used only internally in other TileKind definitions. Singletons.
pattern S_SUSPECT_VERTICAL_WALL_LIT = GroupName "suspect vertical wall Lit"
pattern S_SUSPECT_HORIZONTAL_WALL_LIT = GroupName "suspect horizontal wall Lit"
pattern S_CLOSED_VERTICAL_DOOR_LIT = GroupName "closed vertical door Lit"
pattern S_CLOSED_HORIZONTAL_DOOR_LIT = GroupName "closed horizontal door Lit"
pattern S_OPEN_VERTICAL_DOOR_LIT = GroupName "open vertical door Lit"
pattern S_OPEN_HORIZONTAL_DOOR_LIT = GroupName "open horizontal door Lit"
pattern S_RUBBLE_PILE = GroupName "rubble pile"
pattern S_SHALLOW_WATER_LIT = GroupName "shallow water Lit"
pattern S_SIGNBOARD_UNREAD = GroupName "signboard unread"

-- * Not used, but needed, because auto-generated. Not singletons.
-- This is a rotten compromise, because these are synthesized below,
-- so typos can happen. Similarly below
pattern BRAWL_SET_DARK = GroupName "brawlSetDark"
pattern NOISE_SET_DARK = GroupName "noiseSetDark"
pattern OBSCURED_HORIZONTAL_WALL_DARK =
  GroupName "obscured horizontal wall Dark"
pattern OBSCURED_VERTICAL_WALL_DARK = GroupName "obscured vertical wall Dark"
pattern SHOOTOUT_SET_DARK = GroupName "shootoutSetDark"
pattern TRAPPED_HORIZONAL_DOOR_DARK = GroupName "trapped horizontal door Dark"
pattern TRAPPED_VERTICAL_DOOR_DARK = GroupName "trapped vertical door Dark"

-- * Not used, but needed, because auto-generated. Singletons.
pattern S_BUSH_DARK = GroupName "bush Dark"
pattern S_CLOSED_HORIZONTAL_DOOR_DARK = GroupName "closed horizontal door Dark"
pattern S_CLOSED_VERTICAL_DOOR_DARK = GroupName "closed vertical door Dark"
pattern S_OPEN_HORIZONTAL_DOOR_DARK = GroupName "open horizontal door Dark"
pattern S_OPEN_VERTICAL_DOOR_DARK = GroupName "open vertical door Dark"
pattern S_SHALLOW_WATER_DARK = GroupName "shallow water Dark"
pattern S_SUSPECT_HORIZONTAL_WALL_DARK =
  GroupName "suspect horizontal wall Dark"
pattern S_SUSPECT_VERTICAL_WALL_DARK = GroupName "suspect vertical wall Dark"
pattern S_WALL_DARK = GroupName "wall Dark"
pattern S_WALL_HORIZONTAL_DARK = GroupName "wall horizontal Dark"

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

-- White, cyan and green terrain is usually inert, red is burning or trapped,
-- blue activable or trapped, magenta searchable or activable.

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
  , tfreq    = [(S_UNKNOWN_SPACE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
unknownOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [(S_UNKNOWN_OUTER_FENCE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
basicOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [(S_BASIC_OUTER_FENCE, 1)]
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
  , tfreq    = [ (LEGEND_LIT, 100), (S_WALL_LIT, 100)
               , (RECT_WINDOWS_VERTICAL_LIT, 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs S_SUSPECT_VERTICAL_WALL_LIT]
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '|'
  , tname    = "suspect uneven wall"
  , tfreq    = [(S_SUSPECT_VERTICAL_WALL_LIT, 1)]
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
               , HideAs S_SUSPECT_VERTICAL_WALL_LIT
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "sandstone wall"
  , tfreq    = [ (LEGEND_LIT, 100), (S_WALL_HORIZONTAL_LIT, 100)
               , (RECT_WINDOWS_HORIZONTAL_LIT, 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs S_SUSPECT_HORIZONTAL_WALL_LIT]
  }
wallSuspectH = TileKind  -- only on client
  { tsymbol  = '-'
  , tname    = "suspect painted wall"
  , tfreq    = [(S_SUSPECT_HORIZONTAL_WALL_LIT, 1)]
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
               , HideAs S_SUSPECT_HORIZONTAL_WALL_LIT
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
               , HideAs S_SUSPECT_HORIZONTAL_WALL_LIT
               ]  -- a bit beneficial, but AI would loop if allowed to trigger
                  -- so no @ConsideredByAI@
  }
pillar = TileKind
  { tsymbol  = '0'
  , tname    = "rock outcrop"
  , tfreq    = [ (CACHE_OR_NOT, 70)
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
  , tname    = "smoothed outcrop"
  , tfreq    = [(CACHE_OR_NOT, 30), (CACHE, 1), (STAIR_TERMINAL_DARK, 4)]
                 -- treasure only in dark staircases
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed TREASURE_CACHE, Embed TREASURE_CACHE_TRAP
               , ChangeTo CACHE_OR_NOT, ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger, unless wary of traps.
  }
lampPost = TileKind
  { tsymbol  = '0'
  , tname    = "lamp post"
  , tfreq    = [(S_LAMP_POST, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [(S_SIGNBOARD_UNREAD, 1)]
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
  , tfeature = [Embed SIGNAGE, HideAs S_SIGNBOARD_UNREAD]
  }
tree = TileKind
  { tsymbol  = '0'
  , tname    = "tree"
  , tfreq    = [ (BRAWL_SET_LIT, 140), (SHOOTOUT_SET_LIT, 10)
               , (ESCAPE_SET_LIT, 35), (AMBUSH_SET_LIT, 3)
               , (S_TREE_LIT, 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [ (AMBUSH_SET_DARK, 3), (ZOO_SET_DARK, 7), (BATTLE_SET_DARK, 50)
               , (TREE_BURNING_OR_NOT, 30) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [ (AMBUSH_SET_DARK, 15), (ZOO_SET_DARK, 70)
               , (TREE_BURNING_OR_NOT, 70) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed BIG_FIRE : ChangeTo TREE_BURNING_OR_NOT : tfeature tree
      -- TODO: dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ (S_RUBBLE_PILE, 1), (LEGEND_LIT, 1), (LEGEND_DARK, 1)
               , (STAIR_TERMINAL_LIT, 4), (STAIR_TERMINAL_DARK, 4)
               , (EMPTY_SET_LIT, 10), (EMPTY_SET_DARK, 10)
               , (NOISE_SET_LIT, 50), (POWER_SET_DARK, 50)
               , (ZOO_SET_DARK, 100), (AMBUSH_SET_DARK, 10) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [Embed RUBBLE, OpenTo S_FLOOR_ASHES_LIT]
      -- Getting the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
  }
rubbleSpice = rubble
  { tfreq    = [(SMOKE_CLUMP_LIT, 1), (SMOKE_CLUMP_DARK, 1)]
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
               , OpenTo S_OPEN_VERTICAL_DOOR_LIT
               , HideAs S_SUSPECT_VERTICAL_WALL_LIT
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [(S_CLOSED_VERTICAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo S_OPEN_VERTICAL_DOOR_LIT]  -- never hidden
  }
doorTrappedH = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [(TRAPPED_HORIZONAL_DOOR_LIT, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed DOORWAY_TRAP
               , OpenTo S_OPEN_HORIZONTAL_DOOR_LIT
               , HideAs S_SUSPECT_HORIZONTAL_WALL_LIT
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [(S_CLOSED_HORIZONTAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo S_OPEN_HORIZONTAL_DOOR_LIT]  -- never hidden
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
  , tfreq    = [(GLASSHOUSE_VERTICAL_LIT, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs S_CLOSED_VERTICAL_DOOR_LIT, Clear]
  }
wallGlassSpice = wallGlass
  { tfreq    = [(RECT_WINDOWS_VERTICAL_LIT, 20)]
  , tfeature = Spice : tfeature wallGlass
  }
wallGlassH = TileKind
  { tsymbol  = '-'
  , tname    = "polished crystal wall"
  , tfreq    = [(GLASSHOUSE_HORIZONTAL_LIT, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs S_CLOSED_HORIZONTAL_DOOR_LIT, Clear]
  }
wallGlassHSpice = wallGlassH
  { tfreq    = [(RECT_WINDOWS_HORIZONTAL_LIT, 20)]
  , tfeature = Spice : tfeature wallGlassH
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "icy outcrop"
  , tfreq    = [(POWER_SET_DARK, 300)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [Clear, Embed FROST, OpenTo S_SHALLOW_WATER_LIT]
  }
pulpit = TileKind
  { tsymbol  = '%'
  , tname    = "pulpit"
  , tfreq    = [(S_PULPIT, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [Clear, Embed LECTERN]
                 -- mixed blessing, so AI ignores, saved for player fun
  }
bush = TileKind
  { tsymbol  = '%'
  , tname    = "bush"
  , tfreq    = [ (S_BUSH_LIT, 1), (SHOOTOUT_SET_LIT, 30), (ESCAPE_SET_LIT, 40)
               , (AMBUSH_SET_LIT, 3), (BUSH_CLUMP_LIT, 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [ (BATTLE_SET_DARK, 30), (ZOO_SET_DARK, 30), (AMBUSH_SET_DARK, 3)
               , (BUSH_BURNING_OR_NOT, 70) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [ (AMBUSH_SET_DARK, 15), (ZOO_SET_DARK, 300)
               , (BUSH_BURNING_OR_NOT, 30) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed SMALL_FIRE : ChangeTo BUSH_BURNING_OR_NOT
               : tfeature bush
  }

-- ** Walkable

-- *** Not clear

fog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ (S_FOG_LIT, 1), (EMPTY_SET_LIT, 50), (NOISE_SET_LIT, 100)
               , (SHOOTOUT_SET_LIT, 20)
               , (FOG_CLUMP_LIT, 60), (FOG_CLUMP_DARK, 60) ]
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
  , tfreq    = [ (S_SMOKE_LIT, 1), (LAB_TRAIL_LIT, 1), (STAIR_TERMINAL_LIT, 4)
               , (SMOKE_CLUMP_LIT, 3), (SMOKE_CLUMP_DARK, 3) ]
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
  , tfreq    = [(S_OPEN_VERTICAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo S_CLOSED_VERTICAL_DOOR_LIT
                   -- not explorable due to that
               ]
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [(S_OPEN_HORIZONTAL_DOOR_LIT, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo S_CLOSED_HORIZONTAL_DOOR_LIT
                   -- not explorable due to that
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
               , (ZOO_SET_LIT, 600) ]
  }
floorDamp = floorArena
  { tname    = "damp stone floor"
  , tfreq    = [ (NOISE_SET_LIT, 600), (POWER_SET_LIT, 600)
               , (DAMP_FLOOR_LIT, 1), (STAIR_TERMINAL_LIT, 20) ]
  }
floorDirt = floorArena
  { tname    = "dirt floor"
  , tfreq    = [ (SHOOTOUT_SET_LIT, 1000), (ESCAPE_SET_LIT, 1000)
               , (AMBUSH_SET_LIT, 1000), (BATTLE_SET_LIT, 1000)
               , (BRAWL_SET_LIT, 1000), (DIRT_LIT, 1) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [(TREE_SHADE_WALKABLE_LIT, 1), (BUSH_CLUMP_LIT, 1)]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [(S_FLOOR_ACTOR_LIT, 1)]
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [(LEGEND_LIT, 100)]
  , tfeature = VeryOftenItem : tfeature floorActor
  }
floorAshes = floorActor
  { tfreq    = [ (SMOKE_CLUMP_LIT, 2), (SMOKE_CLUMP_DARK, 2)
               , (S_FLOOR_ASHES_LIT, 1), (S_FLOOR_ASHES_DARK, 1) ]
  , tname    = "dirt and ash pile"
  , tcolor   = Brown
  , tcolor2  = Brown
  }
shallowWater = TileKind
  { tsymbol  = '~'
  , tname    = "water puddle"
  , tfreq    = [ (S_SHALLOW_WATER_LIT, 1), (LEGEND_LIT, 100)
               , (EMPTY_SET_LIT, 5), (NOISE_SET_LIT, 20)
               , (POWER_SET_LIT, 20), (SHOOTOUT_SET_LIT, 5) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = Embed SHALLOW_WATER : tfeature floorActor
  }
shallowWaterSpice = shallowWater
  { tfreq    = [(FOG_CLUMP_LIT, 40)]
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
  , tfreq    = [(S_SHADED_GROUND, 1), (TREE_SHADE_WALKABLE_LIT, 2)]
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

-- * Helper functions

makeDark :: TileKind -> TileKind
makeDark k = let darkenText :: GroupName TileKind -> GroupName TileKind
                 darkenText t = maybe t (GroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ fromGroupName t
                 darkFrequency :: Freqs TileKind
                 darkFrequency = map (first darkenText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkenText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkenText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkenText t
                 darkFeat (OpenWith proj grps t) =
                   Just $ OpenWith proj grps $ darkenText t
                 darkFeat (CloseWith proj grps t) =
                   Just $ CloseWith proj grps $ darkenText t
                 darkFeat (ChangeWith proj grps t) =
                   Just $ ChangeWith proj grps $ darkenText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkenText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkenText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkenText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkenText t
                 darkFeat VeryOftenItem = Just OftenItem
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) {tcolor2 = BrBlack}
