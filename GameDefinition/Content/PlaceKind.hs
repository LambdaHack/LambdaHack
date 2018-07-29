-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25, staircase15up, staircase16down, staircase21up, staircase21down, staircase25up, staircase25down]
  ++ map makeStaircaseUp lstaircase
  ++ map makeStaircaseDown lstaircase

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25, staircase15up, staircase16down, staircase21up, staircase21down, staircase25up, staircase25down :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25]

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
deadEnd = PlaceKind  -- needs to have index 0
  { psymbol  = 'd'
  , pname    = "a dead end"
  , pfreq    = []
  , prarity  = []
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = ["·"]
  , poverride = []
  }
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "a chamber"
  , pfreq    = [("rogue", 100), ("arena", 40), ("laboratory", 40)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|·"
               ]
  , poverride = []
  }
rect2 = rect
  { pname    = "a pen"
  , pfreq    = [("shootout", 2), ("zoo", 10)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a hut"
  , pfreq    = [("escape", 10), ("ambush", 7)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-="
               , "!·"
               ]
  , poverride = [('=', "rectWindowsOver_=_Lit"), ('!', "rectWindowsOver_!_Lit")]
      -- for now I need to specify 'Lit' or I'd be randomly getting lit and dark
      -- tiles, until ooverride is extended to take night/dark into account
  }
glasshouse = PlaceKind
  { psymbol  = 'g'
  , pname    = "a glasshouse"
  , pfreq    = [("shootout", 4)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "=="
               , "!·"
               ]
  , poverride = [('=', "glasshouseOver_=_Lit"), ('!', "glasshouseOver_!_Lit")]
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [("zoo", 10)]
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
  , poverride = [ ('=', "glasshouseOver_=_Lit"), ('!', "glasshouseOver_!_Lit")
                , ('O', "pulpit") ]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruins"
  , pfreq    = [("battle", 33), ("noise", 50), ("ambush", 5)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|X"
               ]
  , poverride = []
  }
collapsed = PlaceKind  -- in a dark cave, they have little lights --- that's OK
  { psymbol  = 'c'
  , pname    = "a collapsed cavern"
  , pfreq    = [("noise", 1)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "O"
               ]
  , poverride = []
  }
collapsed2 = collapsed
  { pfreq    = [("noise", 100), ("battle", 20)]
  , ptopLeft = [ "XO"
               , "OO"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 200), ("battle", 20)]
  , ptopLeft = [ "XXO"
               , "OOO"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 200), ("battle", 20)]
  , ptopLeft = [ "XXXO"
               , "OOOO"
               ]
  }
collapsed5 = collapsed
  { pfreq    = [("noise", 300), ("battle", 50)]
  , ptopLeft = [ "XXO"
               , "XOO"
               , "OOO"
               ]
  }
collapsed6 = collapsed
  { pfreq    = [("noise", 400), ("battle", 100)]
  , ptopLeft = [ "XXXO"
               , "XOOO"
               , "OOOO"
               ]
  }
collapsed7 = collapsed
  { pfreq    = [("noise", 400), ("battle", 100)]
  , ptopLeft = [ "XXXO"
               , "XXOO"
               , "OOOO"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "a hall"
  , pfreq    = [ ("rogue", 500), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 300), ("noise", 1000) ]
  , prarity  = [(1, 15), (10, 15)]
  , pcover   = CStretch
  , pfence   = FNone
  -- Larger rooms require support pillars.
  , ptopLeft = [ "-----"
               , "|····"
               , "|·O··"
               , "|····"
               , "|····"
               ]
  , poverride = [('&', "cache")]
  }
pillar2 = pillar
  { prarity  = [(1, 7), (10, 7)]
  , ptopLeft = [ "-----"
               , "|O···"
               , "|····"
               , "|··O·"
               , "|····"
               ]
  }
pillar3 = pillar
  { ptopLeft = [ "-----"
               , "|O···"
               , "|····"
               , "|····"
               , "|····"
               ]
  }
pillar4 = pillar
  { pname    = "an exquisite hall"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 200), ("noise", 1000) ]
  , prarity  = [(1, 10), (10, 20)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O·O·"
               , "|····"
               ]
  }
pillar5 = pillar
  { pname    = "a decorated hall"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 200), ("noise", 1000) ]
  , prarity  = [(1, 10), (10, 20)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O···"
               , "|····"
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ ("rogue", 3), ("arena", 7), ("laboratory", 4), ("empty", 10)
               , ("mine", 1000), ("escape", 40), ("ambush", 40) ]
  , prarity  = [(1, 20), (10, 20)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O·"
               , "·O"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "O·"
               , "··"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 500), (10, 500)]
  , ptopLeft = [ "··O"
               , "·O·"
               , "O··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 100), (10, 100)]
  , ptopLeft = [ "O··"
               , "·O·"
               , "··O"
               ]
  }
colonnade5 = colonnade
  { ptopLeft = [ "O··"
               , "··O"
               ]
  }
colonnade6 = colonnade
  { prarity  = [(1, 80), (10, 80)]
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
  , poverride = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
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
  , poverride = [ ('O', "treeShadeOver_O_Lit"), ('s', "treeShadeOver_s_Lit")
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
               , ";f"
               ]
  , poverride = [('f', "fogClumpOver_f_Lit"), (';', "lit fog")]
  }
fogClump2 = fogClump
  { pfreq    = [("shootout", 500), ("empty", 1500)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "Xff"
               , "f;f"
               , ";;f"
               , "XfX"
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
               , ";f"
               ]
  , poverride = [ ('f', "smokeClumpOver_f_Lit"), (';', "lit smoke")
                , ('·', "floorActorLit") ]
  }
smokeClump2FGround = smokeClump
  { pname    = "a burned out area"
  , pfreq    = [("laboratory", 150), ("zoo", 500)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ ";f;"
               , "f·f"
               , ";·f"
               , ";f;"
               ]
  }
bushClump = PlaceKind
  { psymbol  = 'b'
  , pname    = "a bushy patch"
  , pfreq    = [("shootout", 100)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";f"
               ]
  , poverride = [('f', "bushClumpOver_f_Lit"), (';', "bush Lit")]
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
  , poverride = []
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
               , "O·O"
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
  , poverride = []
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
               , "O·O"
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
  , poverride = [('>', "escape outdoor down")]
  }
staircase = PlaceKind
  { psymbol  = '/'
  , pname    = "a staircase"
  , pfreq    = [ ("open staircase", 1), ("closed staircase", 1)
               , ("walled staircase", 1) ]
  , prarity  = [(1, 1)]  -- no cover when arriving, so low rarity
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<·>"
               ]
  , poverride = [ ('<', "staircase up"), ('>', "staircase down")
                , ('I', "signboard") ]
  }
staircase2 = staircase
  { pfreq    = [ ("open staircase", 3), ("closed staircase", 3)
               , ("walled staircase", 3) ]
  , pfence   = FGround
  , ptopLeft = [ "·<·>·"
               ]
  }
staircase3 = staircase
  { pfence   = FFloor
  }
staircase4 = staircase2
  { pfence   = FFloor
  }
staircase5 = staircase
  { pfreq    = [("open staircase", 200)]  -- no cover, open
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
  { pfreq    = [("closed staircase", 5000)]
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
  { pfreq    = [("closed staircase", 10000)]
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
  { pfreq    = [("closed staircase", 20000)]
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
  { pfreq    = [("closed staircase", 100000)]
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
  { pfreq    = [("closed staircase", 100000)]
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
  { pfreq    = [("closed staircase", 100000)]
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
  { pfreq    = [("closed staircase", 500000)]
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
  { pfreq    = [("closed staircase", 100000)]
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
  { pfreq    = [("walled staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase26 = staircase
  { pfreq    = [("walled staircase", 200)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "---------"
               ]
  }
staircase27 = staircase
  { pfreq    = [("walled staircase", 500)]
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
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|···<·>···|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase36 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·O·····O·|"
               , "|O··<·>··O|"
               , "|·O·····O·|"
               , "-----------"
               ]
  }
staircase37 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|O·······O|"
               , "|·O·<·>·O·|"
               , "|O·······O|"
               , "-----------"
               ]
  }
overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard") ]
staircaseOutdoor = staircase
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = overrideOutdoor
  }
staircaseOutdoor16 = staircase16
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 10000)]
  , poverride = overrideOutdoor
  }
staircaseOutdoor25 = staircase25
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1000)]
  , poverride = overrideOutdoor
  }
overrideGated :: [(Char, GroupName TileKind)]
overrideGated =
  [ ('<', "gated staircase up"), ('>', "gated staircase down")
  , ('I', "signboard") ]
staircaseGated = staircase
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = overrideGated
  }
staircaseGated16 = staircase16
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 10000)]
  , poverride = overrideGated
  }
staircaseGated25 = staircase25
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1000)]
  , poverride = overrideGated
  }
staircase15up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("closed staircase up", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "·O·O·O·"
               , "O·O·O·O"
               , "·O···O·"
               , "O··<··O"
               , "·O···O·"
               , "O·O·O·O"
               , "·O·O·O·"
               ]
  }
staircase16down = staircase
  { pname    = "a staircase down"
  , pfreq    = [("closed staircase down", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·O·O·O·"
               , "O·····O"
               , "·O·>·O·"
               , "O·····O"
               , "·O·O·O·"
               , "O·O·O·O"
               ]
  }
staircase21up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("closed staircase up", 10000), ("open staircase up", 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·O···O·"
               , "O··<··O"
               , "·O···O·"
               , "O·O·O·O"
               ]
  }
staircase21down = staircase
  { pname    = "a staircase down"
  , pfreq    = [ ("closed staircase down", 10000)
               , ("open staircase down", 10000) ]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·O···O·"
               , "O··>··O"
               , "·O···O·"
               , "O·O·O·O"
               ]
  }
staircase25up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("walled staircase up", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|···|"
               , "|·<·|"
               , "|···|"
               , "-----"
               ]
  }
staircase25down = staircase
  { pname    = "a staircase down"
  , pfreq    = [("walled staircase down", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|···|"
               , "|·>·|"
               , "|···|"
               , "-----"
               ]
  }

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverride = ('>', "stair terminal") : filter ((/= '>') . fst) (poverride s)
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = ('<', "stair terminal") : filter ((/= '<') . fst) (poverride s)
 }
