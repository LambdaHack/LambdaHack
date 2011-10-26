module DungeonState where

import qualified System.Random as R
import qualified Data.List as L
import qualified Control.Monad.State as MState

import Utils.Assert
import Geometry
import Level
import Dungeon
import Random
import qualified Config
import WorldLoc

connect :: StairsLoc ->
           [(StairsLoc -> StairsLoc -> Level,
             Loc, Loc)]
           -> (Level, [Level])
connect au [(x,_,_)] = (x au Nothing, [])
connect au ((x,_,d):ys@((_,u,_):_)) =
  let (z, zs) = connect (Just (Just (lname x',d))) ys
      x'      = x au (Just (Just (lname z,u)))
  in  (x', z : zs)
connect au _ = assert `failure` au

matchGenerator :: Int -> Maybe String -> LevelConfig -> LevelId
                  -> Rnd (StairsLoc -> StairsLoc -> Level,
                          Loc, Loc)
matchGenerator _ Nothing = rogueRoom  -- the default
matchGenerator _ (Just "bigRoom")   = bigRoom
matchGenerator _ (Just "noiseRoom") = noiseRoom
matchGenerator _ (Just "rogueRoom") = rogueRoom
matchGenerator n (Just s) =
  error $ "Unknown dungeon generator " ++ s ++ " for level " ++ show n ++ "."

findGenerator :: Config.CP -> Int
                 -> Rnd (StairsLoc -> StairsLoc -> Level,
                         Loc, Loc)
findGenerator config n =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  in  matchGenerator n genName (defaultLevelConfig n) (LambdaCave n)

-- | Generate the dungeon for a new game.
generate :: Config.CP -> Rnd (Loc, Level, Dungeon)
generate config =
  let d = Config.get config "dungeon" "depth"
      gen :: R.StdGen -> Int -> (R.StdGen, (StairsLoc -> StairsLoc -> Level,
                                            Loc, Loc))
      gen g k =
        let (g1, g2) = R.split g
            (res, _) = MState.runState (findGenerator config k) g1
        in (g2, res)
      con :: R.StdGen -> ((Loc, Level, Dungeon), R.StdGen)
      con g =
        let (gd, levels) = L.mapAccumL gen g [1..d]
            (lvl, lvls) = connect (Just Nothing) levels
            ploc = (\ (_,x,_) -> x) (head levels)
        in ((ploc, lvl, fromList lvls), gd)
  in MState.state con
