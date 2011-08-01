module LevelState where

import qualified Color
import Geometry
import Level
import State
import Item
import Grammar
import qualified Terrain

viewTile :: Bool -> Tile -> Assocs -> (Char, Color.Color)
viewTile b (Tile t [])    a = Terrain.viewTerrain b t
viewTile b (Tile t (i:_)) a = Item.viewItem (ikind i) a

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> Bool -> State -> LMap -> Loc -> String -> String
lookAt detailed canSee s lmap loc msg
  | detailed  =
    Terrain.lookTerrain (tterrain (lmap `rememberAt` loc)) ++ " " ++ msg ++ isd
  | otherwise = msg ++ isd
  where
    is  = titems (lmap `rememberAt` loc)
    prefixSee = if canSee then "You see " else "You remember "
    prefixThere = if canSee
                  then "There are several objects here"
                  else "You remember several objects here"
    isd = case is of
            []    -> ""
            [i]   -> prefixSee ++ objectItem s i ++ "."
            [i,j] -> prefixSee ++ objectItem s i ++ " and "
                               ++ objectItem s j ++ "."
            _     -> prefixThere ++ if detailed then ":" else "."
