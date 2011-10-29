module LevelState where

import qualified Color
import Geometry
import Level
import State
import Item
import Grammar
import Content.TileKind
import qualified Tile
import qualified Kind

viewTile :: Bool -> Tile.Tile -> Assocs -> (Char, Color.Color)
viewTile b (Tile.Tile t [])     _a =
  let u = Kind.getKind t
  in (usymbol u, if b then ucolor u else ucolor2 u)
viewTile _b (Tile.Tile _t (i:_)) a = Item.viewItem (ikind i) a

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> Bool -> State -> LMap -> Loc -> String -> String
lookAt detailed canSee s lm loc msg
  | detailed  =
    let tile = lm `rememberAt` loc
        name = uname . Kind.getKind . Tile.tkind $ tile
    in name ++ " " ++ msg ++ isd
  | otherwise = msg ++ isd
  where
    is  = Tile.titems (lm `rememberAt` loc)
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
