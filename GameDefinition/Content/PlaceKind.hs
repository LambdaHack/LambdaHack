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
      [rect, pillar, pillarC, pillar3, colonnade, colonnadeW, lampPost, lampPost2, lampPost3]
  }
rect,        pillar, pillarC, pillar3, colonnade, colonnadeW, lampPost, lampPost2, lampPost3 :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("ambush", 20)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "--"
               , "|."
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "pillar room"
  , pfreq    = [("rogue", 1000)]  -- larger rooms require support pillars
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "-----"
               , "|...."
               , "|.O.."
               , "|...."
               , "|...."
               ]
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
  , pfreq    = [("rogue", 500)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "O."
               , ".O"
               ]
  }
colonnadeW = colonnade
  { ptopLeft = [ "O."
               , ".."
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("ambush", 50)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ " . "
               , ".O."
               , " . "
               ]
  }
lampPost2 = lampPost
  { pfreq    = [("ambush", 50)]
  , ptopLeft = [ "..."
               , ".O."
               , "..."
               ]
  }
lampPost3 = lampPost
  { pfreq    = [("ambush", 999)]
  , ptopLeft = [ "  .  "
               , " ... "
               , "..O.."
               , " ... "
               , "  .  "
               ]
  }
