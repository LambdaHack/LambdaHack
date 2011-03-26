module LevelState where

import qualified Attr
import Geometry
import Level
import State
import Item
import Grammar
import qualified Terrain

viewTile :: Bool -> Tile -> Assocs -> (Char, Attr.Color)
viewTile b (Tile t [])    a = Terrain.viewTerrain 0 b t
viewTile b (Tile t (i:_)) a = Item.viewItem (ikind i) a

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> State -> LMap -> Loc -> String -> String
lookAt detailed s lmap loc msg
  | detailed  =
    Terrain.lookTerrain (tterrain (lmap `rememberAt` loc)) ++ " " ++ msg ++ isd
  | otherwise = msg ++ isd
  where
    is  = titems (lmap `rememberAt` loc)
    isd = case is of
            []    -> ""
            [i]   -> "You see " ++ objectItem s i ++ "."
            [i,j] -> "You see " ++ objectItem s i ++ " and "
                                ++ objectItem s j ++ "."
            _     -> "There are several objects here" ++
                     if detailed then ":" else "."
