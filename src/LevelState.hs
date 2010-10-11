module LevelState where

import Display
import Geometry
import Level
import State
import Item
import ItemState

viewTile :: Bool -> Tile -> Assocs -> (Char, Attr -> Attr)
viewTile b (Tile t [])    a = viewTerrain 0 b t 
viewTile b (Tile t (i:_)) a = viewItem (itype i) a

-- | Produces a textual description of the items at a location. It's
-- probably correct to use 'at' rather than 'rememberAt' at this point,
-- although we could argue that 'rememberAt' reflects what the player can
-- perceive more correctly ...
--
-- The "detailed" variant is for use with an explicit look command.
lookAt :: Bool -> State -> LMap -> Loc -> String
lookAt detailed s lvl loc
  | detailed  = lookTerrain (tterrain (lvl `at` loc)) ++ " " ++ isd
  | otherwise = isd
  where
    is  = titems (lvl `at` loc)
    isd = case is of
            []    -> ""
            [i]   -> "You see " ++ objectItem s (icount i) (itype i) ++ "."
            [i,j] -> "You see " ++ objectItem s (icount i) (itype i) ++ " and "
                                ++ objectItem s (icount j) (itype j) ++ "."
            _     -> "There are several objects here" ++
                     if detailed then ":" else "."


