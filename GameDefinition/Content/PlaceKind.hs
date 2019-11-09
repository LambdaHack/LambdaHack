-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( -- * Group name patterns
    pattern ROGUE, pattern LABORATORY, pattern ZOO, pattern BRAWL, pattern SHOOTOUT, pattern ARENA, pattern ESCAPE, pattern AMBUSH, pattern BATTLE, pattern NOISE, pattern MINE, pattern EMPTY
  , pattern INDOOR_ESCAPE_DOWN, pattern INDOOR_ESCAPE_UP, pattern OUTDOOR_ESCAPE_DOWN, pattern TINY_STAIRCASE, pattern OPEN_STAIRCASE, pattern CLOSED_STAIRCASE, pattern WALLED_STAIRCASE, pattern GATED_TINY_STAIRCASE, pattern GATED_OPEN_STAIRCASE, pattern GATED_CLOSED_STAIRCASE, pattern OUTDOOR_TINY_STAIRCASE, pattern OUTDOOR_CLOSED_STAIRCASE, pattern OUTDOOR_WALLED_STAIRCASE
  , -- * Content
    content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.TileKind hiding (content)
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)
import Game.LambdaHack.Definition.Defs

-- * Group name patterns

pattern ROGUE, LABORATORY, ZOO, BRAWL, SHOOTOUT, ARENA, ESCAPE, AMBUSH, BATTLE, NOISE, MINE, EMPTY :: GroupName PlaceKind

pattern INDOOR_ESCAPE_DOWN, INDOOR_ESCAPE_UP, OUTDOOR_ESCAPE_DOWN, TINY_STAIRCASE, OPEN_STAIRCASE, CLOSED_STAIRCASE, WALLED_STAIRCASE, GATED_TINY_STAIRCASE, GATED_OPEN_STAIRCASE, GATED_CLOSED_STAIRCASE, OUTDOOR_TINY_STAIRCASE, OUTDOOR_CLOSED_STAIRCASE, OUTDOOR_WALLED_STAIRCASE :: GroupName PlaceKind

pattern ROGUE = GroupName "rogue"
pattern LABORATORY = GroupName "laboratory"
pattern ZOO = GroupName "zoo"
pattern BRAWL = GroupName "brawl"
pattern SHOOTOUT = GroupName "shootout"
pattern ARENA = GroupName "arena"
pattern ESCAPE = GroupName "escape"
pattern AMBUSH = GroupName "ambush"
pattern BATTLE = GroupName "battle"
pattern NOISE = GroupName "noise"
pattern MINE = GroupName "mine"
pattern EMPTY = GroupName "empty"

pattern INDOOR_ESCAPE_DOWN = GroupName "escape down"
pattern INDOOR_ESCAPE_UP = GroupName "escape up"
pattern OUTDOOR_ESCAPE_DOWN = GroupName "outdoor escape route"
pattern TINY_STAIRCASE = GroupName "tiny staircase"
pattern OPEN_STAIRCASE = GroupName "open staircase"
pattern CLOSED_STAIRCASE = GroupName "closed staircase"
pattern WALLED_STAIRCASE = GroupName "walled staircase"

-- This is a rotten compromise, because these are synthesized below,
-- so typos can happen.
pattern GATED_TINY_STAIRCASE = GroupName "gated tiny staircase"
pattern GATED_OPEN_STAIRCASE = GroupName "gated open staircase"
pattern GATED_CLOSED_STAIRCASE = GroupName "gated closed staircase"
pattern OUTDOOR_TINY_STAIRCASE = GroupName "outdoor tiny staircase"
pattern OUTDOOR_CLOSED_STAIRCASE = GroupName "outdoor closed staircase"
pattern OUTDOOR_WALLED_STAIRCASE = GroupName "outdoor walled staircase"

-- * Content

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rect3, rect4, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- automatically generated
  ++ generatedStairs ++ generatedEscapes

deadEnd,    rect, rect2, rect3, rect4, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37 :: PlaceKind

staircase :: PlaceKind  -- template

staircaseBasic :: [PlaceKind]
staircaseBasic = [staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]

generatedStairs :: [PlaceKind]
generatedStairs =
  let gatedStairs = map switchStaircaseToGated staircaseBasic
      outdoorStairs = map switchStaircaseToOutdoor staircaseBasic
      stairsAll = staircaseBasic ++ gatedStairs ++ outdoorStairs
  in gatedStairs ++ outdoorStairs
     ++ map switchStaircaseToUp stairsAll
     ++ map switchStaircaseToDown stairsAll

escapeDownBasic :: [PlaceKind]
escapeDownBasic =
  [escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5]

generatedEscapes :: [PlaceKind]
generatedEscapes =
  let upEscapes = map switchEscapeToUp escapeDownBasic
      outdoorEscapes = map switchEscapeToOutdoorDown escapeDownBasic
  in upEscapes ++ outdoorEscapes

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
deadEnd = PlaceKind  -- needs to have index 0
  { psymbol  = 'd'
  , pname    = "a dead end"
  , pfreq    = []
  , prarity  = []
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = ["·"]
  , poverrideDark = []
  , poverrideLit = []
  }
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "a chamber"
  , pfreq    = [(ROGUE, 30), (LABORATORY, 10)]
  , prarity  = [(1, 10), (10, 6)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|·"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
rect2 = rect
  { pname    = "a pen"
  , pfreq    = [(ZOO, 3)]
  }
rect3 = rect
  { pname    = "a shed"
  , pfreq    = [(BRAWL, 10), (SHOOTOUT, 1)]
  , poverrideDark = [ ('|', WALL_LIT)  -- visible from afar
                    , ('-', WALL_H_LIT) ]
  , poverrideLit = [ ('|', WALL_LIT)
                   , ('-', WALL_H_LIT) ]
  }
rect4 = rect3
  { pname    = "cabinet"
  , pfreq    = [(ARENA, 10)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a hut"
  , pfreq    = [(ESCAPE, 10), (AMBUSH, 7)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-="
               , "!·"
               ]
  , poverrideDark = [ ('=', RECT_WINDOWS_OVER_EQ_DARK)
                    , ('!', RECT_WINDOWS_OVER_EXCL_DARK) ]
  , poverrideLit = [ ('=', RECT_WINDOWS_OVER_EQ_LIT)
                   , ('!', RECT_WINDOWS_OVER_EXCL_LIT) ]
  }
glasshouse = PlaceKind
  { psymbol  = 'g'
  , pname    = "a glasshouse"
  , pfreq    = [(SHOOTOUT, 4)]
  , prarity  = [(1, 10), (10, 7)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "=="
               , "!·"
               ]
  , poverrideDark = [ ('=', GLASSHOUSE_OVER_EQ_LIT)  -- visible from afar
                    , ('!', GLASSHOUSE_OVER_EXCL_LIT) ]
  , poverrideLit = [ ('=', GLASSHOUSE_OVER_EQ_LIT)
                   , ('!', GLASSHOUSE_OVER_EXCL_LIT) ]
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [(ZOO, 10)]
  , poverrideDark = [ ('=', GLASSHOUSE_OVER_EQ_DARK)
                    , ('!', GLASSHOUSE_OVER_EXCL_DARK) ]
  , poverrideLit = [ ('=', GLASSHOUSE_OVER_EQ_LIT)
                   , ('!', GLASSHOUSE_OVER_EXCL_LIT) ]
  }
glasshouse3 = glasshouse
  { pname    = "a reading room"
  , pfreq    = [(ARENA, 40)]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "a stand dais"
  , pfreq    = [(ARENA, 200), (ZOO, 200)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "==·"
               , "!··"
               , "··0"
               ]
  , poverrideDark = [ ('=', GLASSHOUSE_OVER_EQ_LIT)
                    , ('!', GLASSHOUSE_OVER_EXCL_LIT)
                    , ('0', PULPIT) ]
  , poverrideLit = [ ('=', GLASSHOUSE_OVER_EQ_LIT)
                   , ('!', GLASSHOUSE_OVER_EXCL_LIT)
                   , ('0', PULPIT) ]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruins"
  , pfreq    = [(BATTLE, 330)]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|X"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
ruin2 = ruin
  { pname    = "blasted walls"
  , pfreq    = [(AMBUSH, 50)]
  , poverrideDark = [ ('|', WALL_LIT)  -- visible from afar
                    , ('-', WALL_H_LIT) ]
  , poverrideLit = [ ('|', WALL_LIT)
                   , ('-', WALL_H_LIT) ]
  }
collapsed = PlaceKind
  { psymbol  = 'c'
  , pname    = "a collapsed cavern"
  , pfreq    = [(NOISE, 1)]
      -- no point taking up space if very little space taken,
      -- but if no other place can be generated, a failsafe is useful
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "0"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
collapsed2 = collapsed
  { pfreq    = [(NOISE, 1000), (BATTLE, 200)]
  , ptopLeft = [ "X0"
               , "00"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [(NOISE, 2000), (BATTLE, 200)]
  , ptopLeft = [ "XX0"
               , "000"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [(NOISE, 2000), (BATTLE, 200)]
  , ptopLeft = [ "XXX0"
               , "0000"
               ]
  }
collapsed5 = collapsed
  { pfreq    = [(NOISE, 3000), (BATTLE, 500)]
  , ptopLeft = [ "XX0"
               , "X00"
               , "000"
               ]
  }
collapsed6 = collapsed
  { pfreq    = [(NOISE, 4000), (BATTLE, 1000)]
  , ptopLeft = [ "XXX0"
               , "X000"
               , "0000"
               ]
  }
collapsed7 = collapsed
  { pfreq    = [(NOISE, 4000), (BATTLE, 1000)]
  , ptopLeft = [ "XXX0"
               , "XX00"
               , "0000"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "a hall"
  , pfreq    = [(ROGUE, 600), (LABORATORY, 2000)]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  -- Larger rooms require support pillars.
  , ptopLeft = [ "----"
               , "|···"
               , "|·0·"
               , "|···"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
pillar2 = pillar
  { pfreq    = [(ROGUE, 60), (LABORATORY, 200)]
  , ptopLeft = [ "----"
               , "|0··"
               , "|···"
               , "|···"
               ]
  }
pillar3 = pillar
  { pfreq    = [(ROGUE, 8000), (LABORATORY, 25000)]
  , ptopLeft = [ "-----"
               , "|0···"
               , "|····"
               , "|··0·"
               , "|····"
               ]
  }
pillar4 = pillar
  { pname    = "an exquisite hall"
  , pfreq    = [(ROGUE, 30000), (LABORATORY, 100000)]
  , ptopLeft = [ "-----"
               , "|&·0·"
               , "|····"
               , "|0·0·"
               , "|····"
               ]
  , poverrideDark = [('&', CACHE)]
  , poverrideLit = [('&', CACHE)]
  }
pillar5 = pillar
  { pname    = "a decorated hall"
  , pfreq    = [(ROGUE, 30000), (LABORATORY, 100000)]
  , ptopLeft = [ "-----"
               , "|&·0·"
               , "|····"
               , "|0···"
               , "|····"
               ]
  , poverrideDark = [('&', CACHE)]
  , poverrideLit = [('&', CACHE)]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ (ROGUE, 3), (ARENA, 20), (LABORATORY, 2)
               , (EMPTY, 10000), (MINE, 1000), (BRAWL, 4)
               , (ESCAPE, 40), (AMBUSH, 40) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "0·"
               , "··"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 15), (10, 15)]
  , ptopLeft = [ "0·"
               , "·0"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 800), (10, 800)]
  , ptopLeft = [ "··0"
               , "·0·"
               , "0··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 200), (10, 200)]
  , ptopLeft = [ "0··"
               , "·0·"
               , "··0"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 10), (10, 10)]
  , ptopLeft = [ "0··"
               , "··0"
               ]
  }
colonnade6 = colonnade
  { prarity  = [(1, 100), (10, 100)]
  , ptopLeft = [ "0·"
               , "··"
               , "·0"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "a lamp-lit area"
  , pfreq    = [(ESCAPE, 200), (AMBUSH, 200), (ZOO, 100), (BATTLE, 100)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X·X"
               , "·0·"
               , "X·X"
               ]
  , poverrideDark = [('0', LAMP_POST_OVER_0), ('·', FLOOR_ACTOR_LIT)]
  , poverrideLit = [('0', LAMP_POST_OVER_0), ('·', FLOOR_ACTOR_LIT)]
  }
lampPost2 = lampPost
  { ptopLeft = [ "···"
               , "·0·"
               , "···"
               ]
  }
lampPost3 = lampPost
  { pfreq    = [ (ESCAPE, 3000), (AMBUSH, 3000), (ZOO, 50)
               , (BATTLE, 110) ]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··0··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [(ESCAPE, 3000), (AMBUSH, 3000), (ZOO, 50), (BATTLE, 60)]
  , ptopLeft = [ "X···X"
               , "·····"
               , "··0··"
               , "·····"
               , "X···X"
               ]
  }
treeShade = PlaceKind
  { psymbol  = 't'
  , pname    = "a tree shade"
  , pfreq    = [(BRAWL, 1000)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "··s"
               , "s0·"
               , "Xs·"
               ]
  , poverrideDark = [ ('0', TREE_SHADE_OVER_0_DARK)
                    , ('s', TREE_SHADE_OVER_s_DARK)
                    , ('·', SHADED_GROUND) ]
  , poverrideLit = [ ('0', TREE_SHADE_OVER_0_LIT)
                   , ('s', TREE_SHADE_OVER_s_LIT)
                   , ('·', SHADED_GROUND) ]
  }
fogClump = PlaceKind
  { psymbol  = 'f'
  , pname    = "a foggy patch"
  , pfreq    = [(SHOOTOUT, 150), (EMPTY, 15)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";X"
               ]
  , poverrideDark = [('f', FOG_CLUMP_OVER_f_DARK), (';', FOG_LIT)]
  , poverrideLit = [('f', FOG_CLUMP_OVER_f_LIT), (';', FOG_LIT)]
  }
fogClump2 = fogClump
  { pfreq    = [(SHOOTOUT, 500), (EMPTY, 50)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump = PlaceKind
  { psymbol  = 's'
  , pname    = "a smoky patch"
  , pfreq    = [(ZOO, 50)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";X"
               ]
  , poverrideDark = [ ('f', SMOKE_CLUMP_OVER_f_DARK), (';', SMOKE_LIT)
                    , ('·', FLOOR_ACTOR_DARK) ]
  , poverrideLit = [ ('f', SMOKE_CLUMP_OVER_f_LIT), (';', SMOKE_LIT)
                   , ('·', FLOOR_ACTOR_LIT) ]
  }
smokeClump2 = smokeClump
  { pfreq    = [(ZOO, 500)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump3FGround = smokeClump
  { pname    = "a burned out area"
  , pfreq    = [(LABORATORY, 150)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ ";f;"
               , "f·f"
               , "f·f"
               , ";f;"
               ]
      -- should not be used in caves with trails, because bushes should
      -- not grow over such artificial trails
  }
bushClump = PlaceKind
  { psymbol  = 'b'
  , pname    = "a bushy patch"
  , pfreq    = [(SHOOTOUT, 80)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";X"  -- one sure exit needed not to block a corner
               , ";f"
               ]
  , poverrideDark = [('f', BUSH_CLUMP_OVER_f_DARK), (';', BUSH_LIT)]
  , poverrideLit = [('f', BUSH_CLUMP_OVER_f_LIT), (';', BUSH_LIT)]
      -- should not be used in caves with trails, because bushes can't
      -- grow over such artificial trails
  }
escapeDown = PlaceKind
  { psymbol  = '>'
  , pname    = "an escape down"
  , pfreq    = [(INDOOR_ESCAPE_DOWN, 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ ">"
               ]
  , poverrideDark = [ ('|', WALL_LIT)  -- visible from afar
                    , ('-', WALL_H_LIT) ]
  , poverrideLit = [ ('|', WALL_LIT)
                   , ('-', WALL_H_LIT) ]
  }
escapeDown2 = escapeDown
  { pfreq    = [(INDOOR_ESCAPE_DOWN, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0"
               , "·>·"
               , "0·0"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [(INDOOR_ESCAPE_DOWN, 2000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|0·0|"
               , "|·>·|"
               , "|0·0|"
               , "-----"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [(INDOOR_ESCAPE_DOWN, 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "0··"
               , "·>·"
               , "··0"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [(INDOOR_ESCAPE_DOWN, 2000)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|0··|"
               , "|·>·|"
               , "|0·0|"
               , "-----"
               ]
  }
staircase = PlaceKind
  { psymbol  = '/'
  , pname    = "a staircase"
  , pfreq    = [(TINY_STAIRCASE, 1)]  -- no cover when arriving; low freq
  , prarity  = [(1, 100), (10, 100)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<·>"
               ]
  , poverrideDark = [ ('<', STAIRCASE_UP), ('>', STAIRCASE_DOWN)
                    , ('I', SIGNBOARD)
                    , ('|', WALL_LIT), ('-', WALL_H_LIT) ]  -- seen from afar
  , poverrideLit = [ ('<', STAIRCASE_UP), ('>', STAIRCASE_DOWN)
                   , ('I', SIGNBOARD)
                   , ('|', WALL_LIT), ('-', WALL_H_LIT) ]  -- seen from afar
  }
staircase1 = staircase
  { prarity  = [(1, 1)]  -- no cover when arriving; so low rarity
  }
staircase2 = staircase
  { pfreq    = [(TINY_STAIRCASE, 3)]
  , prarity  = [(1, 1)]
  , pfence   = FGround
  , ptopLeft = [ "·<·>·"
               ]
  }
staircase3 = staircase
  { prarity  = [(1, 1)]
  , pfence   = FFloor
  }
staircase4 = staircase2
  { pfence   = FFloor
  , prarity  = [(1, 1)]
  }
staircase5 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 200)]  -- no cover, open
  , pfence   = FGround
  , ptopLeft = [ "0·0"
               , "···"
               , "<·>"
               , "···"
               , "0·0"
               ]
  }
staircase6 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 300)]
  , pfence   = FGround
  , ptopLeft = [ "0·0·0"
               , "·····"
               , "·<·>·"
               , "·····"
               , "0·0·0"
               ]
  }
staircase7 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·0·0·0"
               , "·······"
               , "0·<·>·0"
               , "·······"
               , "0·0·0·0"
               ]
  }
staircase8 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 2000)]
  , pfence   = FGround
  , ptopLeft = [ "·0·I·0·"
               , "0·····0"
               , "··<·>··"
               , "0·····0"
               , "·0·0·0·"
               ]
  }
staircase9 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·······0"
               , "···<·>···"
               , "0·······0"
               ]
  }
staircase10 = staircase
  { pfreq    = [(OPEN_STAIRCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·····0"
               , "··<·>··"
               , "0·····0"
               ]
  }
staircase11 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 2000)]  -- weak cover, low freq
  , pfence   = FFloor
  , ptopLeft = [ "·0·"
               , "0·0"
               , "···"
               , "<·>"
               , "···"
               , "0·0"
               , "·0·"
               ]
  }
staircase12 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 4000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·"
               , "0·0·0"
               , "·····"
               , "·<·>·"
               , "·····"
               , "0·0·0"
               , "·0·0·"
               ]
  }
staircase13 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 6000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·0·"
               , "0·0·0·0"
               , "·······"
               , "0·<·>·0"
               , "·······"
               , "0·0·0·0"
               , "·0·0·0·"
               ]
  }
staircase14 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·0·0"
               , "·0·0·0·"
               , "0·····0"
               , "··<·>··"
               , "0·····0"
               , "·0·0·0·"
               , "0·0·0·0"
               ]
  }
staircase15 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·0·0·"
               , "0·0·0·0·0"
               , "·0·····0·"
               , "0··<·>··0"
               , "·0·····0·"
               , "0·0·0·0·0"
               , "·0·0·0·0·"
               ]
  }
staircase16 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·0·0·0"
               , "·0·0·0·0·"
               , "0·······0"
               , "·0·<·>·0·"
               , "0·······0"
               , "·0·0·0·0·"
               , "0·0·0·0·0"
               ]
  }
staircase17 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·0·0·0·0"
               , "·0·0·0·0·0·"
               , "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               , "·0·0·0·0·0·"
               , "0·0·0·0·0·0"
               ]
  }
staircase18 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 80000)]
  , pfence   = FFloor
  , ptopLeft = [ "··0·0·0·0··"
               , "·0·0·0·0·0·"
               , "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               , "·0·0·0·0·0·"
               , "··0·0·0·0··"
               ]
  }
staircase19 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·0·0·0·"
               , "0·0·0·0·0·0"
               , "·0·······0·"
               , "0·0·<·>·0·0"
               , "·0·······0·"
               , "0·0·0·0·0·0"
               , "·0·0·0·0·0·"
               ]
  }
staircase20 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·0·0·0·"
               , "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               , "·0·0·I·0·0·"
               ]
  }
staircase21 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·I·0·0"
               , "·0·····0·"
               , "0··<·>··0"
               , "·0·····0·"
               , "0·0·0·0·0"
               ]
  }
staircase22 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 2000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               ]
  }
staircase23 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·······0·"
               , "0·0·<·>·0·0"
               , "·0·······0·"
               ]
  }
staircase24 = staircase
  { pfreq    = [(CLOSED_STAIRCASE, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·····0·"
               , "0··<·>··0"
               , "·0·····0·"
               ]
  }
staircase25 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 10)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase26 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 50)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "---------"
               ]
  }
staircase27 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|0·····0|"
               , "|··<·>··|"
               , "|0·····0|"
               , "---------"
               ]
  }
staircase28 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "|·····|"
               , "-------"
               ]
  }
staircase29 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|0···0|"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "|0···0|"
               , "-------"
               ]
  }
staircase30 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|0·0·0|"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "|0·0·0|"
               , "-------"
               ]
  }
staircase31 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 2000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "|·······|"
               , "---------"
               ]
  }
staircase32 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|0·····0|"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "|0·····0|"
               , "---------"
               ]
  }
staircase33 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|0·0·0·0|"
               , "|·······|"
               , "|0·<·>·0|"
               , "|·······|"
               , "|0·0·0·0|"
               , "---------"
               ]
  }
staircase34 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·0·0·0·|"
               , "|0·····0|"
               , "|··<·>··|"
               , "|0·····0|"
               , "|·0·I·0·|"
               , "---------"
               ]
  }
staircase35 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 200)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|···<·>···|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase36 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 500)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·0·····0·|"
               , "|0··<·>··0|"
               , "|·0·····0·|"
               , "-----------"
               ]
  }
staircase37 = staircase
  { pfreq    = [(WALLED_STAIRCASE, 500)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|0·······0|"
               , "|·0·<·>·0·|"
               , "|0·······0|"
               , "-----------"
               ]
  }

switchStaircaseToUp :: PlaceKind -> PlaceKind
switchStaircaseToUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (GroupName $ fromGroupName t <+> "up", k))
               $ pfreq s
 , poverrideDark = ('>', STAIR_TERMINAL_DARK)
                   : filter ((/= '>') . fst) (poverrideDark s)
 , poverrideLit = ('>', STAIR_TERMINAL_LIT)
                  : filter ((/= '>') . fst) (poverrideLit s)
 }

switchStaircaseToDown :: PlaceKind -> PlaceKind
switchStaircaseToDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (GroupName $ fromGroupName t <+> "down", k))
               $ pfreq s
 , poverrideDark = ('<', STAIR_TERMINAL_DARK)
                   : filter ((/= '<') . fst) (poverrideDark s)
 , poverrideLit = ('<', STAIR_TERMINAL_LIT)
                  : filter ((/= '<') . fst) (poverrideLit s)
 }

overrideGated :: [(Char, GroupName TileKind)]
overrideGated =
  [ ('<', GATED_STAIRCASE_UP), ('>', GATED_STAIRCASE_DOWN)
  , ('I', SIGNBOARD)
  , ('|', WALL_LIT), ('-', WALL_H_LIT) ]  -- visible from afar

switchStaircaseToGated :: PlaceKind -> PlaceKind
switchStaircaseToGated s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> GroupName $ "gated" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideGated
 , poverrideLit = overrideGated
 }

overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', STAIRCASE_OUTDOOR_UP), ('>', STAIRCASE_OUTDOOR_DOWN)
  , ('I', SIGNBOARD)
  , ('|', WALL_LIT), ('-', WALL_H_LIT) ]  -- visible from afar

switchStaircaseToOutdoor :: PlaceKind -> PlaceKind
switchStaircaseToOutdoor s = s
 { psymbol   = 'o'
 , pname     = "an outdoor area exit"
 , pfreq     = map (first (\t -> GroupName $ "outdoor" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideOutdoor
 , poverrideLit = overrideOutdoor
 }

switchEscapeToUp :: PlaceKind -> PlaceKind
switchEscapeToUp s = s
 { psymbol   = '<'
 , pname     = "an escape up"
 , pfreq     = map (\(_, n) -> (INDOOR_ESCAPE_UP, n)) $ pfreq s
 , poverrideDark = ('>', ESCAPE_UP) : poverrideDark s
 , poverrideLit = ('>', ESCAPE_UP) : poverrideLit s
 }

switchEscapeToOutdoorDown :: PlaceKind -> PlaceKind
switchEscapeToOutdoorDown s = s
 { pname     = "outdoor escape route"
 , pfreq     = map (\(_, n) -> (OUTDOOR_ESCAPE_DOWN, n)) $ pfreq s
 , poverrideDark = ('>', ESCAPE_OUTDOOR_DOWN) : poverrideDark s
 , poverrideLit = ('>', ESCAPE_OUTDOOR_DOWN) : poverrideLit s
 }
