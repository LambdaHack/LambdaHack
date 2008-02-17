module Item where

import Data.Binary

data Item =
   Ring
 | Scroll
 | Potion
 | Wand
 | Amulet
 | Gem
 | Gold
 deriving Show

instance Binary Item where
  put Ring   = putWord8 0
  put Scroll = putWord8 1
  put Potion = putWord8 2
  put Wand   = putWord8 3
  put Amulet = putWord8 4
  put Gem    = putWord8 5
  put Gold   = putWord8 6
  get = do
          tag <- getWord8
          case tag of
            0 -> return Ring
            1 -> return Scroll
            2 -> return Potion
            3 -> return Wand
            4 -> return Amulet
            5 -> return Gem
            6 -> return Gold

viewItem :: Item -> (Char, Attr -> Attr)
viewItem Ring   = ('=', id)
viewItem Scroll = ('?', id)
viewItem Potion = ('!', id)
viewItem Wand   = ('/', id)
viewItem Gold   = ('$', setFG yellow)
viewItem Gem    = ('*', setFG red)
viewItem _      = ('~', id)

objectItem :: Item -> String
objectItem Ring   = "a ring"
objectItem Scroll = "a scroll"
objectItem Potion = "a potion"
objectItem Wand   = "a wand"
objectItem Amulet = "an amulet"
objectItem Gem    = "a gem"
objectItem Gold   = "some gold"
