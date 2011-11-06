module LevelState where

import Level
import State
import Grammar
import Loc
import Content.TileKind
import qualified Kind

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> Bool -> State -> Level -> Loc -> String -> String
lookAt detailed canSee s lvl loc msg
  | detailed  =
    let tile = lvl `rememberAt` loc
        name = uname . Kind.getKind $ tile
    in name ++ " " ++ msg ++ isd
  | otherwise = msg ++ isd
  where
    is  = lvl `irememberAt` loc
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
