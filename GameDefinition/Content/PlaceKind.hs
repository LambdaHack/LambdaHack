-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( -- * Group name patterns
    pattern ROGUE, pattern LABORATORY, pattern ZOO, pattern BRAWL, pattern SHOOTOUT, pattern ARENA, pattern ESCAPE, pattern AMBUSH, pattern BATTLE, pattern NOISE, pattern MINE, pattern ESCAPE_DOWN, pattern ESCAPE_UP, pattern ESCAPE_OUTDOOR_DOWN, pattern TINY_STARCASE, pattern OPEN_STARCASE, pattern CLOSED_STARCASE, pattern WALLED_STARCASE
  , -- * Content
    content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)
import Game.LambdaHack.Definition.Defs

-- * Group name patterns

pattern ROGUE, LABORATORY, ZOO, BRAWL, SHOOTOUT, ARENA, ESCAPE, AMBUSH, BATTLE, NOISE, MINE, ESCAPE_DOWN, ESCAPE_UP, ESCAPE_OUTDOOR_DOWN, TINY_STARCASE, OPEN_STARCASE, CLOSED_STARCASE, WALLED_STARCASE :: GroupName PlaceKind

pattern ROGUE = "rogue"
pattern LABORATORY = "laboratory"
pattern ZOO = "zoo"
pattern BRAWL = "brawl"
pattern SHOOTOUT = "shootout"
pattern ARENA = "arena"
pattern ESCAPE = "escape"
pattern AMBUSH = "ambush"
pattern BATTLE = "battle"
pattern NOISE = "noise"
pattern MINE = "mine"
pattern ESCAPE_DOWN = "escape down"
pattern ESCAPE_UP = "escape up"
pattern ESCAPE_OUTDOOR_DOWN = "escape outdoor down"
pattern TINY_STARCASE = "tiny staircase"
pattern OPEN_STARCASE = "open staircase"
pattern CLOSED_STARCASE = "closed staircase"
pattern WALLED_STARCASE = "walled staircase"

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
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
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
  , poverrideDark = [ ('=', "rectWindowsOver_=_Dark")
                    , ('!', "rectWindowsOver_!_Dark") ]
  , poverrideLit = [ ('=', "rectWindowsOver_=_Lit")
                   , ('!', "rectWindowsOver_!_Lit") ]
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
  , poverrideDark = [ ('=', "glasshouseOver_=_Lit")  -- visible from afar
                    , ('!', "glasshouseOver_!_Lit") ]
  , poverrideLit = [ ('=', "glasshouseOver_=_Lit")
                   , ('!', "glasshouseOver_!_Lit") ]
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [(ZOO, 10)]
  , poverrideDark = [ ('=', "glasshouseOver_=_Dark")
                    , ('!', "glasshouseOver_!_Dark") ]
  , poverrideLit = [ ('=', "glasshouseOver_=_Lit")
                   , ('!', "glasshouseOver_!_Lit") ]
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
  , poverrideDark = [ ('=', "glasshouseOver_=_Lit")
                    , ('!', "glasshouseOver_!_Lit")
                    , ('0', "pulpit") ]
  , poverrideLit = [ ('=', "glasshouseOver_=_Lit")
                   , ('!', "glasshouseOver_!_Lit")
                   , ('0', "pulpit") ]
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
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
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
  , poverrideDark = [('&', "cache")]
  , poverrideLit = [('&', "cache")]
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
  , poverrideDark = [('&', "cache")]
  , poverrideLit = [('&', "cache")]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ (ROGUE, 3), (ARENA, 20), (LABORATORY, 2)
               , ("empty", 10000), (MINE, 1000), (BRAWL, 4)
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
  , poverrideDark = [('0', "lampPostOver_0"), ('·', "floorActorLit")]
  , poverrideLit = [('0', "lampPostOver_0"), ('·', "floorActorLit")]
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
  , poverrideDark = [ ('0', "treeShadeOver_0_Dark")
                    , ('s', "treeShadeOver_s_Dark")
                    , ('·', "shaded ground") ]
  , poverrideLit = [ ('0', "treeShadeOver_0_Lit")
                   , ('s', "treeShadeOver_s_Lit")
                   , ('·', "shaded ground") ]
  }
fogClump = PlaceKind
  { psymbol  = 'f'
  , pname    = "a foggy patch"
  , pfreq    = [(SHOOTOUT, 150), ("empty", 15)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";X"
               ]
  , poverrideDark = [('f', "fogClumpOver_f_Dark"), (';', "fog Lit")]
  , poverrideLit = [('f', "fogClumpOver_f_Lit"), (';', "fog Lit")]
  }
fogClump2 = fogClump
  { pfreq    = [(SHOOTOUT, 500), ("empty", 50)]
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
  , poverrideDark = [ ('f', "smokeClumpOver_f_Dark"), (';', "smoke Lit")
                    , ('·', "floorActorDark") ]
  , poverrideLit = [ ('f', "smokeClumpOver_f_Lit"), (';', "smoke Lit")
                   , ('·', "floorActorLit") ]
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
  , poverrideDark = [('f', "bushClumpOver_f_Dark"), (';', "bush Lit")]
  , poverrideLit = [('f', "bushClumpOver_f_Lit"), (';', "bush Lit")]
      -- should not be used in caves with trails, because bushes can't
      -- grow over such artificial trails
  }
escapeDown = PlaceKind
  { psymbol  = '>'
  , pname    = "an escape down"
  , pfreq    = [(ESCAPE_DOWN, 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ ">"
               ]
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
  }
escapeDown2 = escapeDown
  { pfreq    = [(ESCAPE_DOWN, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0"
               , "·>·"
               , "0·0"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [(ESCAPE_DOWN, 2000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|0·0|"
               , "|·>·|"
               , "|0·0|"
               , "-----"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [(ESCAPE_DOWN, 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "0··"
               , "·>·"
               , "··0"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [(ESCAPE_DOWN, 2000)]
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
  , pfreq    = [(TINY_STARCASE, 1)]  -- no cover when arriving; low freq
  , prarity  = [(1, 100), (10, 100)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<·>"
               ]
  , poverrideDark = [ ('<', "staircase up"), ('>', "staircase down")
                    , ('I', "signboard")
                    , ('|', "wall Lit"), ('-', "wallH Lit") ]  -- seen from afar
  , poverrideLit = [ ('<', "staircase up"), ('>', "staircase down")
                   , ('I', "signboard")
                   , ('|', "wall Lit"), ('-', "wallH Lit") ]  -- seen from afar
  }
staircase1 = staircase
  { prarity  = [(1, 1)]  -- no cover when arriving; so low rarity
  }
staircase2 = staircase
  { pfreq    = [(TINY_STARCASE, 3)]
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
  { pfreq    = [(OPEN_STARCASE, 200)]  -- no cover, open
  , pfence   = FGround
  , ptopLeft = [ "0·0"
               , "···"
               , "<·>"
               , "···"
               , "0·0"
               ]
  }
staircase6 = staircase
  { pfreq    = [(OPEN_STARCASE, 300)]
  , pfence   = FGround
  , ptopLeft = [ "0·0·0"
               , "·····"
               , "·<·>·"
               , "·····"
               , "0·0·0"
               ]
  }
staircase7 = staircase
  { pfreq    = [(OPEN_STARCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·0·0·0"
               , "·······"
               , "0·<·>·0"
               , "·······"
               , "0·0·0·0"
               ]
  }
staircase8 = staircase
  { pfreq    = [(OPEN_STARCASE, 2000)]
  , pfence   = FGround
  , ptopLeft = [ "·0·I·0·"
               , "0·····0"
               , "··<·>··"
               , "0·····0"
               , "·0·0·0·"
               ]
  }
staircase9 = staircase
  { pfreq    = [(OPEN_STARCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·······0"
               , "···<·>···"
               , "0·······0"
               ]
  }
staircase10 = staircase
  { pfreq    = [(OPEN_STARCASE, 500)]
  , pfence   = FGround
  , ptopLeft = [ "0·····0"
               , "··<·>··"
               , "0·····0"
               ]
  }
staircase11 = staircase
  { pfreq    = [(CLOSED_STARCASE, 2000)]  -- weak cover, low freq
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
  { pfreq    = [(CLOSED_STARCASE, 4000)]
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
  { pfreq    = [(CLOSED_STARCASE, 6000)]
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
  { pfreq    = [(CLOSED_STARCASE, 10000)]
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
  { pfreq    = [(CLOSED_STARCASE, 20000)]
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
  { pfreq    = [(CLOSED_STARCASE, 20000)]
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
  { pfreq    = [(CLOSED_STARCASE, 20000)]
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
  { pfreq    = [(CLOSED_STARCASE, 80000)]
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
  { pfreq    = [(CLOSED_STARCASE, 20000)]
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
  { pfreq    = [(CLOSED_STARCASE, 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·0·0·0·0·"
               , "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               , "·0·0·I·0·0·"
               ]
  }
staircase21 = staircase
  { pfreq    = [(CLOSED_STARCASE, 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·I·0·0"
               , "·0·····0·"
               , "0··<·>··0"
               , "·0·····0·"
               , "0·0·0·0·0"
               ]
  }
staircase22 = staircase
  { pfreq    = [(CLOSED_STARCASE, 2000)]
  , pfence   = FFloor
  , ptopLeft = [ "0·0·····0·0"
               , "·0··<·>··0·"
               , "0·0·····0·0"
               ]
  }
staircase23 = staircase
  { pfreq    = [(CLOSED_STARCASE, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·······0·"
               , "0·0·<·>·0·0"
               , "·0·······0·"
               ]
  }
staircase24 = staircase
  { pfreq    = [(CLOSED_STARCASE, 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·0·····0·"
               , "0··<·>··0"
               , "·0·····0·"
               ]
  }
staircase25 = staircase
  { pfreq    = [(WALLED_STARCASE, 10)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase26 = staircase
  { pfreq    = [(WALLED_STARCASE, 50)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "---------"
               ]
  }
staircase27 = staircase
  { pfreq    = [(WALLED_STARCASE, 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|0·····0|"
               , "|··<·>··|"
               , "|0·····0|"
               , "---------"
               ]
  }
staircase28 = staircase
  { pfreq    = [(WALLED_STARCASE, 1000)]
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
  { pfreq    = [(WALLED_STARCASE, 1000)]
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
  { pfreq    = [(WALLED_STARCASE, 1000)]
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
  { pfreq    = [(WALLED_STARCASE, 2000)]
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
  { pfreq    = [(WALLED_STARCASE, 5000)]
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
  { pfreq    = [(WALLED_STARCASE, 5000)]
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
  { pfreq    = [(WALLED_STARCASE, 5000)]
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
  { pfreq    = [(WALLED_STARCASE, 200)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|···<·>···|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase36 = staircase
  { pfreq    = [(WALLED_STARCASE, 500)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·0·····0·|"
               , "|0··<·>··0|"
               , "|·0·····0·|"
               , "-----------"
               ]
  }
staircase37 = staircase
  { pfreq    = [(WALLED_STARCASE, 500)]
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
 , pfreq     = map (\(t, k) -> (toGroupName $ fromGroupName t <+> "up", k))
               $ pfreq s
 , poverrideDark = ('>', "stair terminal Dark")
                   : filter ((/= '>') . fst) (poverrideDark s)
 , poverrideLit = ('>', "stair terminal Lit")
                  : filter ((/= '>') . fst) (poverrideLit s)
 }

switchStaircaseToDown :: PlaceKind -> PlaceKind
switchStaircaseToDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ fromGroupName t <+> "down", k))
               $ pfreq s
 , poverrideDark = ('<', "stair terminal Dark")
                   : filter ((/= '<') . fst) (poverrideDark s)
 , poverrideLit = ('<', "stair terminal Lit")
                  : filter ((/= '<') . fst) (poverrideLit s)
 }

overrideGated :: [(Char, GroupName TileKind)]
overrideGated =
  [ ('<', "gated staircase up"), ('>', "gated staircase down")
  , ('I', "signboard")
  , ('|', "wall Lit"), ('-', "wallH Lit") ]  -- visible from afar

switchStaircaseToGated :: PlaceKind -> PlaceKind
switchStaircaseToGated s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideGated
 , poverrideLit = overrideGated
 }

overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard")
  , ('|', "wall Lit"), ('-', "wallH Lit") ]  -- visible from afar

switchStaircaseToOutdoor :: PlaceKind -> PlaceKind
switchStaircaseToOutdoor s = s
 { psymbol   = 'o'
 , pname     = "an outdoor area exit"
 , pfreq     = map (first (\t -> toGroupName $ "outdoor" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideOutdoor
 , poverrideLit = overrideOutdoor
 }

switchEscapeToUp :: PlaceKind -> PlaceKind
switchEscapeToUp s = s
 { psymbol   = '<'
 , pname     = "an escape up"
 , pfreq     = map (\(_, n) -> (ESCAPE_UP, n)) $ pfreq s
 , poverrideDark = ('>', "escape up") : poverrideDark s
 , poverrideLit = ('>', "escape up") : poverrideLit s
 }

switchEscapeToOutdoorDown :: PlaceKind -> PlaceKind
switchEscapeToOutdoorDown s = s
 { pname     = "outdoor escape route"
 , pfreq     = map (\(_, n) -> (ESCAPE_OUTDOOR_DOWN, n)) $ pfreq s
 , poverrideDark = ('>', "escape outdoor down") : poverrideDark s
 , poverrideLit = ('>', "escape outdoor down") : poverrideLit s
 }
