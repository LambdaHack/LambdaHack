module DungeonState where

import Data.Map as M

import Geometry
import Level
import Dungeon
import Random
import qualified Config
import WorldLoc

connect ::
  Maybe (Maybe WorldLoc) ->
  [(Maybe (Maybe WorldLoc) -> Maybe (Maybe WorldLoc) -> Level, Loc, Loc)] ->
  [Level]
connect au [(x,_,_)] = [x au Nothing]
connect au ((x,_,d):ys@((_,u,_):_)) =
  let (z:zs) = connect (Just (Just (lname x',d))) ys
      x'     = x au (Just (Just (lname z,u)))
  in  x' : z : zs

matchGenerator n Nothing = rogueRoom  -- the default
matchGenerator n (Just "bigRoom")   = bigRoom
matchGenerator n (Just "noiseRoom") = noiseRoom
matchGenerator n (Just "rogueRoom") = rogueRoom
matchGenerator n (Just s) =
  error $ "matchGenerator: unknown: " ++ show n ++ ", " ++ s

findGenerator config n =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  in  matchGenerator n genName (defaultLevelConfig n) (LambdaCave n)

-- | Generate the dungeon for a new game.
generate :: Config.CP -> Rnd (Loc, Level, Dungeon)
generate config = do
  let depth = Config.get config "dungeon" "depth"
  levels <- mapM (findGenerator config) [1..depth]
  let lvls = connect (Just Nothing) levels
      ploc = ((\ (_,x,_) -> x) (head levels))
  return $ (ploc, head lvls, dungeon (tail lvls))
