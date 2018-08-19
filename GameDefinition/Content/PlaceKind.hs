-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rect3, rect4, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- automatically generated
  ++ generatedStairs

deadEnd,    rect, rect2, rect3, rect4, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37 :: PlaceKind

staircase :: PlaceKind  -- template

staircaseBasic :: [PlaceKind]
staircaseBasic = [staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]

generatedStairs :: [PlaceKind]
generatedStairs =
  let gatedStairs = map makeGated staircaseBasic
      outdoorStairs = map makeOutdoor staircaseBasic
      stairsAll = staircaseBasic ++ gatedStairs ++ outdoorStairs
  in gatedStairs ++ outdoorStairs
     ++ map makeStaircaseUp stairsAll
     ++ map makeStaircaseDown stairsAll

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
  , pfreq    = [("rogue", 100), ("laboratory", 30)]
  , prarity  = [(1, 3), (10, 2)]
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
  , pfreq    = [("zoo", 10)]
  }
rect3 = rect
  { pname    = "a shed"
  , pfreq    = [("shootout", 2)]
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
  }
rect4 = rect3
  { pname    = "cabinet"
  , pfreq    = [("arena", 35)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a hut"
  , pfreq    = [("escape", 10), ("ambush", 7)]
  , prarity  = [(1, 10), (10, 7)]
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
  , pfreq    = [("shootout", 4)]
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
  , pfreq    = [("zoo", 10)]
  , poverrideDark = [ ('=', "glasshouseOver_=_Dark")
                    , ('!', "glasshouseOver_!_Dark") ]
  , poverrideLit = [ ('=', "glasshouseOver_=_Lit")
                   , ('!', "glasshouseOver_!_Lit") ]
  }
glasshouse3 = glasshouse
  { pname    = "a reading room"
  , pfreq    = [("arena", 40)]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "a stand"
  , pfreq    = [("arena", 30), ("zoo", 20)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "==·"
               , "!··"
               , "··O"
               ]
  , poverrideDark = [ ('=', "glasshouseOver_=_Lit")
                    , ('!', "glasshouseOver_!_Lit")
                    , ('O', "pulpit") ]
  , poverrideLit = [ ('=', "glasshouseOver_=_Lit")
                   , ('!', "glasshouseOver_!_Lit")
                   , ('O', "pulpit") ]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruins"
  , pfreq    = [("battle", 33), ("ambush", 5)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|X"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
ruin2 = ruin
  { pname    = "a "
  , pfreq    = [("blasted walls", 120)]
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
  }
collapsed = PlaceKind
  { psymbol  = 'c'
  , pname    = "a collapsed cavern"
  , pfreq    = [("noise", 1)]
  , prarity  = [(1, 1), (10, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "O"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
collapsed2 = collapsed
  { pfreq    = [("noise", 1000), ("battle", 200)]
  , ptopLeft = [ "XO"
               , "OO"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 2000), ("battle", 200)]
  , ptopLeft = [ "XXO"
               , "OOO"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 2000), ("battle", 200)]
  , ptopLeft = [ "XXXO"
               , "OOOO"
               ]
  }
collapsed5 = collapsed
  { pfreq    = [("noise", 3000), ("battle", 500)]
  , ptopLeft = [ "XXO"
               , "XOO"
               , "OOO"
               ]
  }
collapsed6 = collapsed
  { pfreq    = [("noise", 4000), ("battle", 1000)]
  , ptopLeft = [ "XXXO"
               , "XOOO"
               , "OOOO"
               ]
  }
collapsed7 = collapsed
  { pfreq    = [("noise", 4000), ("battle", 1000)]
  , ptopLeft = [ "XXXO"
               , "XXOO"
               , "OOOO"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "a hall"
  , pfreq    = [ ("rogue", 300), ("laboratory", 1000), ("noise", 1000) ]
  , prarity  = [(1, 2), (10, 2)]
  , pcover   = CStretch
  , pfence   = FNone
  -- Larger rooms require support pillars.
  , ptopLeft = [ "----"
               , "|···"
               , "|·O·"
               , "|···"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
pillar2 = pillar
  { pfreq    = [ ("rogue", 500), ("laboratory", 1000), ("noise", 1000) ]
  , prarity  = [(1, 15), (10, 15)]
  , ptopLeft = [ "-----"
               , "|O···"
               , "|····"
               , "|··O·"
               , "|····"
               ]
  }
pillar3 = pillar
  { pfreq    = [ ("rogue", 500), ("laboratory", 1000), ("noise", 1000) ]
  , prarity  = [(1, 30), (10, 30)]
  , ptopLeft = [ "-----"
               , "|O···"
               , "|····"
               , "|····"
               , "|····"
               ]
  }
pillar4 = pillar
  { pname    = "an exquisite hall"
  , prarity  = [(1, 20), (10, 40)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O·O·"
               , "|····"
               ]
  , poverrideDark = [('&', "cache")]
  , poverrideLit = [('&', "cache")]
  }
pillar5 = pillar
  { pname    = "a decorated hall"
  , prarity  = [(1, 20), (10, 40)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O···"
               , "|····"
               ]
  , poverrideDark = [('&', "cache")]
  , poverrideLit = [('&', "cache")]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ ("rogue", 3), ("arena", 7), ("laboratory", 4), ("empty", 10)
               , ("mine", 1000), ("escape", 40), ("ambush", 40) ]
  , prarity  = [(1, 15), (10, 15)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O·"
               , "·O"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 10), (10, 10)]
  , ptopLeft = [ "O·"
               , "··"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 800), (10, 800)]
  , ptopLeft = [ "··O"
               , "·O·"
               , "O··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 200), (10, 200)]
  , ptopLeft = [ "O··"
               , "·O·"
               , "··O"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 10), (10, 10)]
  , ptopLeft = [ "O··"
               , "··O"
               ]
  }
colonnade6 = colonnade
  { prarity  = [(1, 100), (10, 100)]
  , ptopLeft = [ "O·"
               , "··"
               , "·O"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "a lamp post"
  , pfreq    = [("escape", 20), ("ambush", 20), ("zoo", 10), ("battle", 10)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X·X"
               , "·O·"
               , "X·X"
               ]
  , poverrideDark = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
  , poverrideLit = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
  }
lampPost2 = lampPost
  { ptopLeft = [ "···"
               , "·O·"
               , "···"
               ]
  }
lampPost3 = lampPost
  { pfreq    = [ ("escape", 3000), ("ambush", 3000), ("zoo", 50)
               , ("battle", 110) ]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("escape", 3000), ("ambush", 3000), ("zoo", 50), ("battle", 60)]
  , ptopLeft = [ "X···X"
               , "·····"
               , "··O··"
               , "·····"
               , "X···X"
               ]
  }
treeShade = PlaceKind
  { psymbol  = 't'
  , pname    = "a tree shade"
  , pfreq    = [("brawl", 100)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "··s"
               , "sO·"
               , "Xs·"
               ]
  , poverrideDark = [ ('O', "treeShadeOver_O_Dark")
                    , ('s', "treeShadeOver_s_Dark")
                    , ('·', "shaded ground") ]
  , poverrideLit = [ ('O', "treeShadeOver_O_Lit")
                   , ('s', "treeShadeOver_s_Lit")
                   , ('·', "shaded ground") ]
  }
fogClump = PlaceKind
  { psymbol  = 'f'
  , pname    = "a foggy patch"
  , pfreq    = [("shootout", 150), ("empty", 500)]
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
  { pfreq    = [("shootout", 500), ("empty", 1500)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump = PlaceKind
  { psymbol  = 's'
  , pname    = "a smoky patch"
  , pfreq    = [("zoo", 50)]
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
  { pfreq    = [("zoo", 500)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump3FGround = smokeClump
  { pname    = "a burned out area"
  , pfreq    = [("laboratory", 150)]
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
  , pfreq    = [("shootout", 80)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";f"
               ]
  , poverrideDark = [('f', "bushClumpOver_f_Dark"), (';', "bush Lit")]
  , poverrideLit = [('f', "bushClumpOver_f_Lit"), (';', "bush Lit")]
      -- should not be used in caves with trails, because bushes can't
      -- grow over such artificial trails
  }
escapeUp = PlaceKind
  { psymbol  = '<'
  , pname    = "an escape up"
  , pfreq    = [("escape up", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<"
               ]
  , poverrideDark = [ ('|', "wall Lit")  -- visible from afar
                    , ('-', "wallH Lit") ]
  , poverrideLit = [ ('|', "wall Lit")
                   , ('-', "wallH Lit") ]
  }
escapeUp2 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O"
               , "·<·"
               , "O·O"
               ]
  }
escapeUp3 = escapeUp
  { pfreq    = [("escape up", 2000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O·O|"
               , "|·<·|"
               , "|O·O|"
               , "-----"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "O··"
               , "·<·"
               , "··O"
               ]
  }
escapeUp5 = escapeUp
  { pfreq    = [("escape up", 2000)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O··|"
               , "|·<·|"
               , "|O·O|"
               , "-----"
               ]
  }
escapeDown = PlaceKind
  { psymbol  = '>'
  , pname    = "an escape down"
  , pfreq    = [("escape down", 1)]
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
  { pfreq    = [("escape down", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O"
               , "·>·"
               , "O·O"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O·O|"
               , "|·>·|"
               , "|O·O|"
               , "-----"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "O··"
               , "·>·"
               , "··O"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O··|"
               , "|·>·|"
               , "|O·O|"
               , "-----"
               ]
  }
escapeOutdoorDown = escapeDown
  { pfreq     = [("escape outdoor down", 1)]
  , poverrideDark = [('>', "escape outdoor down")]
  , poverrideLit = [('>', "escape outdoor down")]
  }
staircase = PlaceKind
  { psymbol  = '/'
  , pname    = "a staircase"
  , pfreq    = [("tiny staircase", 1)]  -- no cover when arriving; low freq
  , prarity  = [(1, 100)]
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
  { pfreq    = [("tiny staircase", 3)]
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
  { pfreq    = [("open staircase", 200)]  -- no cover, open, intentional
  , pfence   = FGround
  , ptopLeft = [ "O·O"
               , "···"
               , "<·>"
               , "···"
               , "O·O"
               ]
  }
staircase6 = staircase
  { pfreq    = [("open staircase", 300)]
  , pfence   = FGround
  , ptopLeft = [ "O·O·O"
               , "·····"
               , "·<·>·"
               , "·····"
               , "O·O·O"
               ]
  }
staircase7 = staircase
  { pfreq    = [("open staircase", 500)]
  , pfence   = FGround
  , ptopLeft = [ "O·O·O·O"
               , "·······"
               , "O·<·>·O"
               , "·······"
               , "O·O·O·O"
               ]
  }
staircase8 = staircase
  { pfreq    = [("open staircase", 2000)]
  , pfence   = FGround
  , ptopLeft = [ "·O·I·O·"
               , "O·····O"
               , "··<·>··"
               , "O·····O"
               , "·O·O·O·"
               ]
  }
staircase9 = staircase
  { pfreq    = [("open staircase", 500)]
  , pfence   = FGround
  , ptopLeft = [ "O·······O"
               , "···<·>···"
               , "O·······O"
               ]
  }
staircase10 = staircase
  { pfreq    = [("open staircase", 500)]
  , pfence   = FGround
  , ptopLeft = [ "O·····O"
               , "··<·>··"
               , "O·····O"
               ]
  }
staircase11 = staircase
  { pfreq    = [("closed staircase", 2000)]  -- weak cover, low freq
  , pfence   = FFloor
  , ptopLeft = [ "·O·"
               , "O·O"
               , "···"
               , "<·>"
               , "···"
               , "O·O"
               , "·O·"
               ]
  }
staircase12 = staircase
  { pfreq    = [("closed staircase", 4000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·"
               , "O·O·O"
               , "·····"
               , "·<·>·"
               , "·····"
               , "O·O·O"
               , "·O·O·"
               ]
  }
staircase13 = staircase
  { pfreq    = [("closed staircase", 6000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·O·"
               , "O·O·O·O"
               , "·······"
               , "O·<·>·O"
               , "·······"
               , "O·O·O·O"
               , "·O·O·O·"
               ]
  }
staircase14 = staircase
  { pfreq    = [("closed staircase", 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·O·O·O·"
               , "O·····O"
               , "··<·>··"
               , "O·····O"
               , "·O·O·O·"
               , "O·O·O·O"
               ]
  }
staircase15 = staircase
  { pfreq    = [("closed staircase", 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·O·O·"
               , "O·O·O·O·O"
               , "·O·····O·"
               , "O··<·>··O"
               , "·O·····O·"
               , "O·O·O·O·O"
               , "·O·O·O·O·"
               ]
  }
staircase16 = staircase
  { pfreq    = [("closed staircase", 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O·O"
               , "·O·O·O·O·"
               , "O·······O"
               , "·O·<·>·O·"
               , "O·······O"
               , "·O·O·O·O·"
               , "O·O·O·O·O"
               ]
  }
staircase17 = staircase
  { pfreq    = [("closed staircase", 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O·O·O"
               , "·O·O·O·O·O·"
               , "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               , "·O·O·O·O·O·"
               , "O·O·O·O·O·O"
               ]
  }
staircase18 = staircase
  { pfreq    = [("closed staircase", 80000)]
  , pfence   = FFloor
  , ptopLeft = [ "··O·O·O·O··"
               , "·O·O·O·O·O·"
               , "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               , "·O·O·O·O·O·"
               , "··O·O·O·O··"
               ]
  }
staircase19 = staircase
  { pfreq    = [("closed staircase", 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·O·O·O·"
               , "O·O·O·O·O·O"
               , "·O·······O·"
               , "O·O·<·>·O·O"
               , "·O·······O·"
               , "O·O·O·O·O·O"
               , "·O·O·O·O·O·"
               ]
  }
staircase20 = staircase
  { pfreq    = [("closed staircase", 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·O·O·O·"
               , "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               , "·O·O·I·O·O·"
               ]
  }
staircase21 = staircase
  { pfreq    = [("closed staircase", 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·I·O·O"
               , "·O·····O·"
               , "O··<·>··O"
               , "·O·····O·"
               , "O·O·O·O·O"
               ]
  }
staircase22 = staircase
  { pfreq    = [("closed staircase", 2000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               ]
  }
staircase23 = staircase
  { pfreq    = [("closed staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·······O·"
               , "O·O·<·>·O·O"
               , "·O·······O·"
               ]
  }
staircase24 = staircase
  { pfreq    = [("closed staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·····O·"
               , "O··<·>··O"
               , "·O·····O·"
               ]
  }
staircase25 = staircase
  { pfreq    = [("walled staircase", 10)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase26 = staircase
  { pfreq    = [("walled staircase", 50)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "---------"
               ]
  }
staircase27 = staircase
  { pfreq    = [("walled staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O·····O|"
               , "|··<·>··|"
               , "|O·····O|"
               , "---------"
               ]
  }
staircase28 = staircase
  { pfreq    = [("walled staircase", 1000)]
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
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|O···O|"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "|O···O|"
               , "-------"
               ]
  }
staircase30 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|O·O·O|"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "|O·O·O|"
               , "-------"
               ]
  }
staircase31 = staircase
  { pfreq    = [("walled staircase", 2000)]
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
  { pfreq    = [("walled staircase", 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O·····O|"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "|O·····O|"
               , "---------"
               ]
  }
staircase33 = staircase
  { pfreq    = [("walled staircase", 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O·O·O·O|"
               , "|·······|"
               , "|O·<·>·O|"
               , "|·······|"
               , "|O·O·O·O|"
               , "---------"
               ]
  }
staircase34 = staircase
  { pfreq    = [("walled staircase", 5000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·O·O·O·|"
               , "|O·····O|"
               , "|··<·>··|"
               , "|O·····O|"
               , "|·O·I·O·|"
               , "---------"
               ]
  }
staircase35 = staircase
  { pfreq    = [("walled staircase", 200)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|···<·>···|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase36 = staircase
  { pfreq    = [("walled staircase", 500)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·O·····O·|"
               , "|O··<·>··O|"
               , "|·O·····O·|"
               , "-----------"
               ]
  }
staircase37 = staircase
  { pfreq    = [("walled staircase", 500)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|O·······O|"
               , "|·O·<·>·O·|"
               , "|O·······O|"
               , "-----------"
               ]
  }

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverrideDark = ('>', "stair terminal Dark")
                   : filter ((/= '>') . fst) (poverrideDark s)
 , poverrideLit = ('>', "stair terminal Lit")
                  : filter ((/= '>') . fst) (poverrideLit s)
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
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

makeGated :: PlaceKind -> PlaceKind
makeGated s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> tshow t)) $ pfreq s
 , poverrideDark = overrideGated
 , poverrideLit = overrideGated
 }

overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard")
  , ('|', "wall Lit"), ('-', "wallH Lit") ]  -- visible from afar

makeOutdoor :: PlaceKind -> PlaceKind
makeOutdoor s = s
 { psymbol   = 'o'
 , pname     = "an outdoor area exit"
 , pfreq     = map (first (\t -> toGroupName $ "outdoor" <+> tshow t)) $ pfreq s
 , poverrideDark = overrideOutdoor
 , poverrideLit = overrideOutdoor
 }
