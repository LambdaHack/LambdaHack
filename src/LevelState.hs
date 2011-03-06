module LevelState where

import Display
import Geometry
import Level
import State
import Item
import ItemState
import Grammar

viewTile :: Bool -> Tile -> Assocs -> (Char, Attr -> Attr)
viewTile b (Tile t [])    a = viewTerrain 0 b t
viewTile b (Tile t (i:_)) a = viewItem (itype i) a

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> State -> LMap -> Loc -> String -> String
lookAt detailed s lmap loc msg
  | detailed  =
    lookTerrain (tterrain (lmap `rememberAt` loc)) ++ " " ++ msg ++ isd
  | otherwise = msg ++ isd
  where
    is  = titems (lmap `rememberAt` loc)
    isd = case is of
            []    -> ""
            [i]   -> "You see " ++ objectItem s (icount i) (itype i) ++ "."
            [i,j] -> "You see " ++ objectItem s (icount i) (itype i) ++ " and "
                                ++ objectItem s (icount j) (itype j) ++ "."
            _     -> "There are several objects here" ++
                     if detailed then ":" else "."
