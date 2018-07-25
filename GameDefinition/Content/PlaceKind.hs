-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown]
  ++ map makeStaircaseUp lstaircase
  ++ map makeStaircaseDown lstaircase

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated]

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
-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
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
  , pfreq    = [("zoo", 10)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a hut"
  , pfreq    = [("park", 10)]
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
  , pfreq    = [("shootout", 6)]
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
  , prarity  = [(1, 10), (10, 8)]
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
  , pfreq    = [("battle", 33), ("noise", 50)]
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
               , "|····"
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
  , pfreq    = [ ("rogue", 3), ("arena", 7), ("laboratory", 4)
               , ("empty", 10), ("mine", 1000), ("park", 40) ]
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
  , pfreq    = [("park", 20), ("zoo", 10), ("battle", 10)]
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
  { pfreq    = [("park", 3000), ("zoo", 50), ("battle", 110)]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("park", 3000), ("zoo", 50), ("battle", 60)]
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
  , pfreq    = [("laboratory", 100), ("zoo", 500)]
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
staircase = PlaceKind
  { psymbol  = '|'
  , pname    = "a staircase"
  , pfreq    = [("staircase", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<·>"
               ]
  , poverride = [ ('<', "staircase up"), ('>', "staircase down")
                , ('I', "signboard") ]
  }
staircase2 = staircase
  { pfreq    = [("staircase", 30)]
  , pfence   = FGround
  , ptopLeft = [ "·<·>·"
               ]
  }
staircase3 = staircase
  { pfreq    = [("staircase", 500)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O"
               , "···"
               , "<·>"
               , "···"
               , "O·O"
               ]
  }
staircase4 = staircase
  { pfreq    = [("staircase", 500)]
  , pfence   = FFloor
  , ptopLeft = [ "O·I·O"
               , "·····"
               , "·<·>·"
               , "·····"
               , "O·O·O"
               ]
  }
staircase5 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·······"
               , "O·<·>·O"
               , "·······"
               , "O·O·O·O"
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 200)]
  , pfence   = FGround
  , ptopLeft = [ "O·<·>·O"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 200)]
  , pfence   = FGround
  , ptopLeft = [ "·O·<·>·O·"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 300)]
  , pfence   = FGround
  , ptopLeft = [ "I·O·<·>·O·O"
               ]
  }
staircase9 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·····O"
               , "··<·>··"
               , "O·····O"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·······O"
               , "·O·<·>·O·"
               , "O·······O"
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               ]
  }
staircase12 = staircase
  { pfreq    = [("staircase", 2500)]
  , pfence   = FGround
  , ptopLeft = [ "··O·O··"
               , "O·····O"
               , "··<·>··"
               , "O·····O"
               , "··O·O··"
               ]
  }
staircase13 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase14 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|··<·>··|"
               , "|·······|"
               , "---------"
               ]
  }
staircase15 = staircase
  { pfreq    = [("staircase", 500)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|O·<·>·O|"
               , "|·······|"
               , "---------"
               ]
  }
staircase16 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|·O·<·>·O·|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase17 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------------"
               , "|···········|"
               , "|O·O·<·>·I·O|"
               , "|···········|"
               , "-------------"
               ]
  }
staircase18 = staircase
  { pfreq    = [("staircase", 500)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O·····O|"
               , "|··<·>··|"
               , "|O·····O|"
               , "---------"
               ]
  }
staircase19 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|O·······O|"
               , "|·O·<·>·O·|"
               , "|O·······O|"
               , "-----------"
               ]
  }
staircaseOutdoor = staircase
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
  }
staircaseGated = staircase
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = [('<', "gated staircase up"), ('>', "gated staircase down")]
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
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "O··"
               , "·<·"
               , "O·O"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O·O|"
               , "|·<·|"
               , "|O·O|"
               , "-----"
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
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "O··"
               , "·>·"
               , "O·O"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O·O|"
               , "|·>·|"
               , "|O·O|"
               , "-----"
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
