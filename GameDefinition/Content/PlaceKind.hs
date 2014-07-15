-- | Rooms, halls and passages for LambdaHack.
module Content.PlaceKind ( cdefs ) where

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validate = validatePlaceKind
  , content =
      [rect, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillarC, pillar3, colonnade, colonnade2, colonnade3, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3]
  }
rect,        ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillarC, pillar3, colonnade, colonnade2, colonnade3, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3 :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("ambush", 8)]
  , prarity  = [(1, 1)]
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
  , pfreq    = [("ambush", 17), ("battle", 100)]
  , prarity  = [(1, 1)]
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
  , prarity  = [(1, 1)]
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
  , pfreq    = [("rogue", 1000)]  -- larger rooms require support pillars
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|...."
               , "|.O.."
               , "|...."
               , "|...."
               ]
  , poverride = []
  }
pillarC = pillar
  { ptopLeft = [ "-----"
               , "|O..."
               , "|...."
               , "|...."
               , "|...."
               ]
  }
pillar3 = pillar
  { ptopLeft = [ "-----"
               , "|&.O."
               , "|...."
               , "|O..."
               , "|...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 60)]
  , prarity  = [(1, 1)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O."
               , ".O"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { ptopLeft = [ "O."
               , ".."
               ]
  }
colonnade3 = colonnade
  { pfreq    = [("rogue", 6)]
  , ptopLeft = [ ".."
               , ".O"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("ambush", 30), ("battle", 10)]
  , prarity  = [(1, 1)]
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
  , prarity  = [(1, 1)]
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
