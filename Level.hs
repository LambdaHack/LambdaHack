module Level where

import Data.Map as M

type X = Int
type Y = Int

type Level = Map (Y,X) Tile

data Tile = Rock
          | Floor
          | Unknown
          | Corridor

instance Show Tile where
  show Rock  = " "
  show Floor = "."
  show Unknown = "?"
  show Corridor = "#"

type Room = Area
type Loc = (Y,X)
type Area = ((Y,X),(Y,X))

