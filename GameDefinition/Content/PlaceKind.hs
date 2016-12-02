-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content = contentFromList $
      [rect, rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, boardgame]
      ++ map makeStaircaseUp lstaircase
      ++ map makeStaircaseDown lstaircase
  }
rect,        rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, boardgame :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14]

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|."
               ]
  , poverride = []
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "room"
  , pfreq    = [("ambush", 8), ("noise", 80)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-="
               , "!."
               ]
  , poverride = [ ('=', "horizontalWallOrGlassOver_=_Lit")
                , ('!', "verticalWallOrGlassOver_!_Lit") ]
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("ambush", 17), ("battle", 33), ("noise", 40)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|X"
               ]
  , poverride = []
  }
collapsed = PlaceKind
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
  , ptopLeft = [ "XXO"
               , "XOO"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 200), ("battle", 20)]
  , ptopLeft = [ "XXXO"
               , "XOOO"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 400), ("battle", 100)]
  , ptopLeft = [ "XXXO"
               , "XXXO"
               , "XOOO"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "pillar room"
  , pfreq    = [("rogue", 1000), ("noise", 50)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  -- Larger rooms require support pillars.
  , ptopLeft = [ "-----"
               , "|...."
               , "|.O.."
               , "|...."
               , "|...."
               ]
  , poverride = []
  }
pillar2 = pillar
  { ptopLeft = [ "-----"
               , "|O..."
               , "|...."
               , "|...."
               , "|...."
               ]
  }
pillar3 = pillar
  { prarity  = [(1, 2), (10, 2)]
  , ptopLeft = [ "-----"
               , "|O..."
               , "|..O."
               , "|.O.."
               , "|...."
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 10)]
  , ptopLeft = [ "-----"
               , "|&.O."
               , "|...."
               , "|O..."
               , "|...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 70), ("noise", 2000)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O."
               , ".O"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 4), (10, 4)]
  , ptopLeft = [ "O."
               , ".."
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 2), (10, 2)]
  , pfence   = FGround
  , ptopLeft = [ ".."
               , ".O"
               ]
  }
colonnade4 = colonnade
  { ptopLeft = [ "O.."
               , ".O."
               , "..O"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 4), (10, 4)]
  , ptopLeft = [ "O.."
               , "..O"
               ]
  }
colonnade6 = colonnade
  { ptopLeft = [ "O."
               , ".."
               , ".O"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("ambush", 30), ("battle", 10)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X.X"
               , ".O."
               , "X.X"
               ]
  , poverride = [('O', "lampPostOver_O")]
  }
lampPost2 = lampPost
  { ptopLeft = [ "..."
               , ".O."
               , "..."
               ]
  }
lampPost3 = lampPost
  { ptopLeft = [ "XX.XX"
               , "X...X"
               , "..O.."
               , "X...X"
               , "XX.XX"
               ]
  }
lampPost4 = lampPost
  { ptopLeft = [ "X...X"
               , "....."
               , "..O.."
               , "....."
               , "X...X"
               ]
  }
treeShade = PlaceKind
  { psymbol  = 't'
  , pname    = "tree shade"
  , pfreq    = [("skirmish", 100)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "sss"
               , "XOs"
               , "XXs"
               ]
  , poverride = [('O', "treeShadeOver_O"), ('s', "treeShadeOrFogOver_s")]
  }
treeShade2 = treeShade
  { ptopLeft = [ "sss"
               , "XOs"
               , "Xss"
               ]
  }
treeShade3 = treeShade
  { ptopLeft = [ "sss"
               , "sOs"
               , "XXs"
               ]
  }
staircase = PlaceKind
  { psymbol  = '|'
  , pname    = "staircase"
  , pfreq    = [("staircase", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<.>"
               ]
  , poverride = [('<', "staircase up"), ('>', "staircase down")]
  }
staircase2 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "....."
               , ".<.>."
               , "....."
               ]
  }
staircase3 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "......."
               , "O.<.>.O"
               , "......."
               ]
  }
staircase4 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "O.......O"
               , ".O.<.>.O."
               , "O.......O"
               ]
  }
staircase5 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "..........."
               , "O.O.<.>.O.O"
               , "..........."
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "O.........O"
               , "..O.<.>.O.."
               , "O.........O"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "O.....O"
               , "..<.>.."
               , "O.....O"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "O.O.O.O"
               , "......."
               , "O.<.>.O"
               , "......."
               , "O.O.O.O"
               ]
  }
staircase9 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FFloor
  , ptopLeft = [ "XXO.OXX"
               , "O.....O"
               , "..<.>.."
               , "O.....O"
               , "XXO.OXX"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-------"
               , "|.....|"
               , "|.<.>.|"
               , "|.....|"
               , "-------"
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "-----------"
               , "|.........|"
               , "|.O.<.>.O.|"
               , "|.........|"
               , "-----------"
               ]
  }
staircase12 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------------"
               , "|.............|"
               , "|.O.O.<.>.O.O.|"
               , "|.............|"
               , "---------------"
               ]
  }
staircase13 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O.....O|"
               , "|..<.>..|"
               , "|O.....O|"
               , "---------"
               ]
  }
staircase14 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FNone
  , ptopLeft = [ "---------"
               , "|O......|"
               , "|..<.>..|"
               , "|O..O..O|"
               , "---------"
               ]
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
  , ptopLeft = [ "O.O"
               , ".<."
               , "O.O"
               ]
  }
escapeUp3 = escapeUp
  { pfreq    = [("escape up", 1100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O.O|"
               , "|.<.|"
               , "|O.O|"
               , "-----"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O..|"
               , "|.<.|"
               , "|O.O|"
               , "-----"
               ]
  }
escapeUp5 = escapeUp
  { pfreq    = [("escape up", 1100)]
  , pfence   = FFloor
  , ptopLeft = [ "XXOXX"
               , "O...O"
               , "..<.."
               , "O...O"
               , "XXOXX"
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
  , ptopLeft = [ "O.O"
               , ".>."
               , "O.O"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [("escape down", 1100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O.O|"
               , "|.>.|"
               , "|O.O|"
               , "-----"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1100)]
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|O..|"
               , "|.>.|"
               , "|O.O|"
               , "-----"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 1100)]
  , pfence   = FFloor
  , ptopLeft = [ "XXOXX"
               , "O...O"
               , "..>.."
               , "O...O"
               , "XXOXX"
               ]
  }
boardgame = PlaceKind
  { psymbol  = 'b'
  , pname    = "boardgame"
  , pfreq    = [("boardgame", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "----------"
               , "|.b.b.b.b|"
               , "|b.b.b.b.|"
               , "|.b.b.b.b|"
               , "|b.b.b.b.|"
               , "|.b.b.b.b|"
               , "|b.b.b.b.|"
               , "|.b.b.b.b|"
               , "|b.b.b.b.|"
               , "----------"
               ]
  , poverride = [('b', "trailChessLit")]
  }

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = "staircase up"
 , pfreq     = map (\(_, k) -> ("staircase up", k)) $ pfreq s
 , poverride = [('>', "stair terminal"), ('<', "staircase up")]
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = "staircase down"
 , pfreq     = map (\(_, k) -> ("staircase down", k)) $ pfreq s
 , poverride = [('<', "stair terminal"), ('>', "staircase down")]
 }
