-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content =
      [rect, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, boardgame]
  }
rect,        ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, boardgame :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("ambush", 8), ("noise", 80)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|."
               ]
  , poverride = []
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("ambush", 17), ("battle", 100), ("noise", 40)]
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
  { pfreq    = [("noise", 100), ("battle", 50)]
  , ptopLeft = [ "XXO"
               , "XOO"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 200), ("battle", 50)]
  , ptopLeft = [ "XXXO"
               , "XOOO"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 400), ("battle", 200)]
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
  , poverride = [('O', "treeShadeOver_O"), ('s', "treeShadeOver_s")]
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
