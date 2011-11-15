module Game.LambdaHack.LevelState where

import Game.LambdaHack.Level
import Game.LambdaHack.State
import Game.LambdaHack.Grammar
import Game.LambdaHack.Loc
import qualified Game.LambdaHack.Kind as Kind

-- | Produces a textual description of the terrain and items at an already
-- explored location. Mute for unknown locations.
-- The "detailed" variant is for use in the targeting mode.
lookAt :: Bool -> Bool -> State -> Level -> Loc -> String -> String
lookAt detailed canSee s lvl loc msg
  | detailed  =
    let Kind.COps{cotile=Kind.Ops{oname}} = scops s
        tile = lvl `rememberAt` loc
        name = oname  $ tile
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
