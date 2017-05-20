-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content = contentFromList $
      [rect, rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown]
      ++ map makeStaircaseUp lstaircase
      ++ map makeStaircaseDown lstaircase
  }
rect,        rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated]

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [ ("rogue", 100), ("arena", 40), ("laboratory", 40)
               , ("shootout", 8), ("zoo", 7) ]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|·"
               ]
  , poverride = []
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "room"
  , pfreq    = [("empty", 10), ("park", 7)]
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
  , pname    = "glasshouse"
  , pfreq    = [("arena", 40), ("zoo", 12)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "=="
               , "!·"
               ]
  , poverride = [('=', "glasshouseOver_=_Lit"), ('!', "glasshouseOver_!_Lit")]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "pulpit"
  , pfreq    = [("arena", 10), ("zoo", 30)]
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
  , pname    = "ruin"
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
  , pname    = "collapsed cavern"
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
  , pname    = "pillar room"
  , pfreq    = [ ("rogue", 500), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 300), ("noise", 1000) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  -- Larger rooms require support pillars.
  , ptopLeft = [ "-----"
               , "|····"
               , "|·O··"
               , "|····"
               , "|····"
               ]
  , poverride = [('&', "cachable")]
  }
pillar2 = pillar
  { ptopLeft = [ "-----"
               , "|O···"
               , "|····"
               , "|····"
               , "|····"
               ]
  }
pillar3 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O·O·"
               , "|····"
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "-----"
               , "|&·O·"
               , "|····"
               , "|O···"
               , "|····"
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [ ("rogue", 30), ("arena", 70), ("laboratory", 40)
               , ("empty", 100), ("mine", 10000), ("park", 3000) ]
  , prarity  = [(1, 3), (10, 3)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O·"
               , "·O"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 2), (10, 2)]
  , ptopLeft = [ "O·"
               , "··"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "··O"
               , "·O·"
               , "O··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "O··"
               , "·O·"
               , "··O"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 7), (10, 7)]
  , ptopLeft = [ "O··"
               , "··O"
               ]
  }
colonnade6 = colonnade
  { ptopLeft = [ "O·"
               , "··"
               , "·O"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
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
  , pname    = "tree shade"
  , pfreq    = [("brawl", 300)]
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
  , pname    = "foggy patch"
  , pfreq    = [("shootout", 170)]
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
  { pfreq    = [("shootout", 400), ("empty", 1500)]
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
  , pname    = "smoky patch"
  , pfreq    = [("zoo", 100)]
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
  { pfreq    = [("laboratory", 100), ("zoo", 1000)]
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
  , pname    = "bushy patch"
  , pfreq    = [("shootout", 120)]
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
  , pname    = "staircase"
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
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O"
               , "···"
               , "<·>"
               , "···"
               , "O·O"
               ]
  }
staircase3 = staircase
  { pfreq    = [("staircase", 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "O·I·O"
               , "·····"
               , "·<·>·"
               , "·····"
               , "O·O·O"
               ]
  }
staircase4 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·O·O"
               , "·······"
               , "O·<·>·O"
               , "·······"
               , "O·O·O·O"
               ]
  }
staircase5 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "O·<·>·O"
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "O··<·>··O"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 100)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "O·I·<·>·O·O"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·····O"
               , "··<·>··"
               , "O·····O"
               ]
  }
staircase9 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·······O"
               , "·O·<·>·O·"
               , "O·······O"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O·O·····O·O"
               , "·O··<·>··O·"
               , "O·O·····O·O"
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 10000)]
  , pfence   = FGround
  , ptopLeft = [ "··O·O··"
               , "O·····O"
               , "··<·>··"
               , "O·····O"
               , "··O·O··"
               ]
  }
staircase12 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|·····|"
               , "|·<·>·|"
               , "|·····|"
               , "-------"
               ]
  }
staircase13 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|·······|"
               , "|O·<·>·O|"
               , "|·······|"
               , "---------"
               ]
  }
staircase14 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|·········|"
               , "|·O·<·>·O·|"
               , "|·········|"
               , "-----------"
               ]
  }
staircase15 = staircase
  { pfreq    = [("staircase", 1000)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "-------------"
               , "|···········|"
               , "|O·O·<·>·I·O|"
               , "|···········|"
               , "-------------"
               ]
  }
staircase16 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O·····O|"
               , "|··<·>··|"
               , "|O·····O|"
               , "---------"
               ]
  }
staircase17 = staircase
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
  { pname     = "staircase outdoor"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
  }
staircaseGated = staircase
  { pname     = "gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = [('<', "gated staircase up"), ('>', "gated staircase down")]
  }
escapeUp = PlaceKind
  { psymbol  = '<'
  , pname    = "escape up"
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
  { pfreq    = [("escape down", 2000)]
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
  , pname    = "escape down"
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
 , poverride = [ ('>', "stair terminal")
               , ('<', toGroupName $ pname s <+> "up")
               , ('I', "signboard") ]
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = [ ('<', "stair terminal")
               , ('>', toGroupName $ pname s <+> "down")
               , ('I', "signboard") ]
 }
