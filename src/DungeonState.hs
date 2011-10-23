module DungeonState where

import Utils.Assert
import Geometry
import Level
import Dungeon
import Random
import qualified Config
import WorldLoc

connect ::
  Maybe (Maybe WorldLoc) ->
  [(Maybe (Maybe WorldLoc) -> Maybe (Maybe WorldLoc) -> Level, Loc, Loc)] ->
  (Level, [Level])
connect au [(x,_,_)] = (x au Nothing, [])
connect au ((x,_,d):ys@((_,u,_):_)) =
  let (z, zs) = connect (Just (Just (lname x',d))) ys
      x'      = x au (Just (Just (lname z,u)))
  in  (x', z : zs)
connect au _ = assert `failure` au

matchGenerator :: Int -> Maybe String -> LevelConfig -> LevelId
                  -> Rnd (Maybe (Maybe WorldLoc) ->
                            Maybe (Maybe WorldLoc) -> Level,
                          Loc, Loc)
matchGenerator _ Nothing = rogueRoom  -- the default
matchGenerator _ (Just "bigRoom")   = bigRoom
matchGenerator _ (Just "noiseRoom") = noiseRoom
matchGenerator _ (Just "rogueRoom") = rogueRoom
matchGenerator n (Just s) =
  error $ "Unknown dungeon generator " ++ s ++ " for level " ++ show n ++ "."

findGenerator :: Config.CP -> Int
                 -> Rnd (Maybe (Maybe WorldLoc) ->
                           Maybe (Maybe WorldLoc) -> Level,
                         Loc, Loc)
findGenerator config n =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  in  matchGenerator n genName (defaultLevelConfig n) (LambdaCave n)

-- | Generate the dungeon for a new game.
generate :: Config.CP -> Rnd (Loc, Level, Dungeon)
generate config = do
  let d = Config.get config "dungeon" "depth"
  levels <- mapM (findGenerator config) [1..d]
  let (lvl, lvls) = connect (Just Nothing) levels
      ploc = (\ (_,x,_) -> x) (head levels)
  return (ploc, lvl, dungeon lvls)
