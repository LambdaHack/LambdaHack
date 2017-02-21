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
      [rect, rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClumpFGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, boardgame]
      ++ map makeStaircaseUp lstaircase
      ++ map makeStaircaseDown lstaircase
  }
rect,        rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClumpFGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, boardgame :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor]

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("arena", 40), ("laboratory", 40)]
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
  , pfreq    = [("empty", 10)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-="
               , "!·"
               ]
  , poverride = [('=', "rectWindowsOver_=_Lit"), ('!', "rectWindowsOver_!_Lit")]
  }
glasshouse = PlaceKind
  { psymbol  = 'g'
  , pname    = "glasshouse"
  , pfreq    = [("shootout", 8), ("arena", 40)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "=="
               , "!·"
               ]
  , poverride = [('=', "wallGlassH"), ('!', "wallGlassV")]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "pulpit"
  , pfreq    = [("arena", 5)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "==·"
               , "!··"
               , "··O"
               ]
  , poverride = [('=', "wallGlassH"), ('!', "wallGlassV"), ('O', "pulpit")]
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
               , ("empty", 1000) , ("noise", 1000) ]
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
  , pfreq    = [ ("rogue", 30), ("arena", 70), ("laboratory", 70)
               , ("empty", 70), ("noise", 4000), ("ambush", 150) ]
  , prarity  = [(1, 5), (10, 5)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O·"
               , "·O"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 4), (10, 4)]
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
  { prarity  = [(1, 8), (10, 8)]
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
  , pfreq    = [("ambush", 20), ("battle", 10)]
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
  { pfreq    = [("ambush", 300), ("battle", 110)]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("ambush", 300), ("battle", 60)]
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
  , ptopLeft = [ "ff"
               , "#f"
               , "#f"
               ]
  , poverride = [('f', "fogClumpOver_f_Lit"), ('#', "lit fog")]
  }
fogClump2 = fogClump
  { pfreq    = [("shootout", 400), ("empty", 10000)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , ptopLeft = [ "XfX"
               , "f#f"
               , "f#f"
               , "XfX"
               ]
  }
smokeClumpFGround = PlaceKind
  { psymbol  = 's'
  , pname    = "smoky patch"
  , pfreq    = [("laboratory", 100)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "#f#"
               , "f·f"
               , "f·f"
               , "#f#"
               ]
  , poverride = [ ('f', "smokeClumpOver_f_Lit"), ('#', "lit smoke")
                , ('·', "floorActorLit") ]
  }
bushClump = PlaceKind
  { psymbol  = 'b'
  , pname    = "bushy patch"
  , pfreq    = [("shootout", 120)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "ff"
               , "#f"
               , "#f"
               ]
  , poverride = [('f', "bushClumpOver_f_Lit"), ('#', "lit bush")]
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
                , ('I', "signpost") ]
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
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
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
  { pname    = "staircase outdoor"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
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
boardgame = PlaceKind
  { psymbol  = 'b'
  , pname    = "boardgame"
  , pfreq    = [("boardgame", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "----------"
               , "|·b·b·b·b|"
               , "|b·b·b·b·|"
               , "|·b·b·b·b|"
               , "|b·b·b·b·|"
               , "|·b·b·b·b|"
               , "|b·b·b·b·|"
               , "|·b·b·b·b|"
               , "|b·b·b·b·|"
               , "----------"
               ]
  , poverride = [('b', "trailChessLit")]
  }

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverride = [ ('>', "stair terminal")
               , ('<', toGroupName $ pname s <+> "up")
               , ('I', "signpost") ]
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = [ ('<', "stair terminal")
               , ('>', toGroupName $ pname s <+> "down")
               , ('I', "signpost") ]
 }
