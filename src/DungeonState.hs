module DungeonState where

import qualified System.Random as R
import qualified Data.List as L
import qualified Control.Monad.State as MState

import Utils.Assert
import Loc
import Level
import qualified Dungeon
import Random
import qualified Config
import WorldLoc
import State
import qualified Feature as F
import qualified Tile
import Content.CaveKind
import Cave

matchGenerator :: Int -> Maybe String -> CaveKind -> Bool
                  -> Rnd Level
matchGenerator _ Nothing = rogueRoom  -- the default
matchGenerator _ (Just "bigRoom")   = bigRoom
matchGenerator _ (Just "noiseRoom") = noiseRoom
matchGenerator _ (Just "rogueRoom") = rogueRoom
matchGenerator n (Just s) =
  error $ "Unknown dungeon generator " ++ s ++ " for level " ++ show n ++ "."

findGenerator :: Config.CP -> Int -> Int -> Rnd Level
findGenerator config n depth =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  in matchGenerator
       n genName (defaultCaveKind n) (n == depth)

-- | Generate the dungeon for a new game.
generate :: Config.CP -> Rnd (Loc, LevelId, Dungeon.Dungeon)
generate config =
  let depth = Config.get config "dungeon" "depth"
      gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = MState.evalState (findGenerator config k depth) g1
        in (g2, (LambdaCave k, res))
      con :: R.StdGen -> ((Loc, LevelId, Dungeon.Dungeon), R.StdGen)
      con g =
        let (gd, levels) = L.mapAccumL gen g [1..depth]
            ploc = fst (lstairs (snd (head levels)))
        in ((ploc, LambdaCave 1, Dungeon.fromList levels), gd)
  in MState.state con

whereTo :: State -> Loc -> Maybe WorldLoc
whereTo state@State{slid, sdungeon} loc =
  let lvl = slevel state
      tile = lvl `at` loc
      k | Tile.hasFeature F.Climbable tile = -1
        | Tile.hasFeature F.Descendable tile = 1
        | otherwise = assert `failure` tile
      n = levelNumber slid
      nln = n + k
      ln = LambdaCave nln
      lvlTrg = sdungeon Dungeon.! ln
  in if (nln < 1)
     then Nothing
     else Just (ln, (if k == 1 then fst else snd) (lstairs lvlTrg))
